namespace ProjectionTree

module ReteBuilder =
  open ProjectionTree.Tree
  open ProjectionTree.ReteNetwork

  let mkRete nodeType children = {nodeType = nodeType; children = children; parent = ref None}
  let mkBetaMem children = mkRete (Beta {tokens = ref Set.empty}) children
  let mkBetaMemDummy children = mkRete (Beta {tokens = ref <| Set.singleton []}) children
  let mkJoin tests children = mkRete (Join {tests = tests;alphaMem = ref None}) children
  let mkProd prodId = mkRete (Production prodId) [||]
  let mkAlphaMem children = {wmes = ref Set.empty;successors = children}

  // backpointer helpers
  let alphaMemsInAlphaNetwork (alphaNet:AlphaNetwork<_>) =
    Seq.append (Seq.map snd <| Map.toSeq alphaNet.InstanceMap) (Seq.map snd <| Map.toSeq alphaNet.AssignmentFacetMap)

  let setBackPointers ((reteTopNode, alphaNet):ReteGraph<_>) =
    let rec setParents node =
      for child in node.children do
          child.parent := Some node
          setParents child
    let setAlphaMem alphaMem =
        for succ in alphaMem.successors do
            match succ.nodeType with
            | Join jd -> jd.alphaMem := Some alphaMem
            | _ -> ()
    setParents reteTopNode
    Seq.iter setAlphaMem <| alphaMemsInAlphaNetwork alphaNet

  let addOneToMany k v m =
    match Map.tryFind k m with
    | Some l -> Map.add k (v :: l) m
    | None -> Map.add k [ v ] m

  let map (f:'a->'b) (pm: PatternMap<'a>) : PatternMap<'b> =
    {
      InstanceMap = Map.map (fun _ v -> f v) pm.InstanceMap
      AssignmentFacetMap = Map.map (fun _ v -> f v) pm.AssignmentFacetMap
      PartOf = f pm.PartOf
    }

  let rec collectTests =
    function
    | TestNode(test, node) ->
      let tests, node' = collectTests node
      test :: tests, node'
    | node -> [], node

  let partitionTestChildren children =
    let isTest = function | ([], _) -> false | _ -> true
    List.partition isTest children

  let addAlphaMap pattern value alphaMap =
    match pattern with
    | InstanceNode itype ->
      {alphaMap with InstanceMap = addOneToMany itype value alphaMap.InstanceMap }
    | ProjectionNode(_, cstic, facet) ->
      {alphaMap with AssignmentFacetMap = addOneToMany (cstic, facet) value alphaMap.AssignmentFacetMap }
    | PartOfPattern _ ->
      {alphaMap with PartOf = value :: alphaMap.PartOf }

  let reteGraphFromProjectionTree ptrees : ReteGraph<_> =
    let alphaMapRef : PatternMap<(int * ReteNode<_>) list> ref =
      ref { InstanceMap = Map.empty; AssignmentFacetMap = Map.empty; PartOf = [] }
    let rec loop depth =
      function
      | ActionNode action -> Seq.singleton <| mkProd action
      | TestNode _ -> failwith "this should never happen"
      | PatternNode(pattern, children) ->
        let childrenWithCollectedTests = List.map collectTests children
        let testChildren, nonTestChildren = partitionTestChildren childrenWithCollectedTests
        let betaMem = mkBetaMem << Array.ofSeq <| Seq.collect (loop (depth + 1)) (Seq.toArray <| Seq.map snd nonTestChildren)
        let objTests =
          match pattern with
          | InstanceNode _ -> []
          | ProjectionNode(targetVariable, _, _) ->
            let thisVar : Variable = 0
            [Comparison(Deref thisVar, EQ, Deref targetVariable)]
          | PartOfPattern _ ->
            []
        let buildTestJoinNode (tests, node) =
          mkJoin (objTests @ tests) (Array.ofSeq <| loop (depth + 1) node)
        let joinNode = mkJoin objTests [|betaMem|]
        let joinNodes = Seq.append (Seq.singleton joinNode) (Seq.map buildTestJoinNode testChildren)
        Seq.iter (fun j -> alphaMapRef := addAlphaMap pattern (depth, j) !alphaMapRef) joinNodes
        joinNodes
    let createAlphaNode (numberedJoinNodes: (int * ReteNode<_>) list) =
      let orderedJoinNodes = Seq.map snd <| List.sortBy (fun (depth, _) -> -depth) numberedJoinNodes
      mkAlphaMem <| Seq.toArray orderedJoinNodes
    let graph =
      mkBetaMemDummy << Array.ofSeq <| Seq.collect (fun ptree -> loop 0 ptree) ptrees,
        map createAlphaNode !alphaMapRef
    setBackPointers graph
    graph
