namespace PatternMatching

module ReteBuilder =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let mkRete nodeType children = {nodeType = nodeType; children = children; parent = ref None}
  let mkBetaMem children = mkRete (Beta {tokens = ref []}) children
  let mkBetaMemDummy children = mkRete (Beta {tokens = ref [[]]}) children
  let mkJoin test children = mkRete (Join {test = test;alphaMem = ref None}) children

  let mkAlphaMem children = {wmes = ref [];successors = children}

  let addOneToMany k v m =
    match Map.tryFind k m with
    | None -> Map.add k [v] m
    | Some vs -> Map.add k (v::vs) m

  // backpointer helpers
  let alphaMemsInAlphaNetwork (alphaNet:AlphaNetwork<_>) = Seq.map snd alphaNet

  let setBackPointers ((reteTopNode, alphaNet):ReteGraph<_>) =
    let rec setParents node =
      for child in node.children do
          child.parent := Some node
          setParents child
    let setAlphaMem alphaMem =
        for succ in alphaMem.successors do
            match succ.nodeType with
                Join jd -> jd.alphaMem := Some alphaMem
                | _ -> ()
    setParents reteTopNode
    Seq.iter setAlphaMem <| alphaMemsInAlphaNetwork alphaNet

  let rec collectTests =
    function
    | TestNode(test, ptree) ->
      let tests, ptree' = collectTests ptree
      test :: tests, ptree'
    | ptree -> [], ptree

  let reteGraphFromPatternTrees ptrees : ReteGraph<_> =
    let alphaMapRef = ref Map.empty
    let rec loop depth =
      function
      | PatternTree.Production prodId -> [| mkRete (Production prodId) [||] |]
      | TestNode _ -> failwith "internal error"
      | PatternNode(pattern, children) ->
        let testChildren, nonTestChildren = Array.partition (function (TestNode _) -> true | _ -> false) children
        let nonTestJoins =
          if Seq.isEmpty nonTestChildren
          then Seq.empty
          else
            let betaMem = mkBetaMem <| Array.collect (loop (depth + 1)) nonTestChildren
            let j = mkJoin (fun _ -> true) [| betaMem |]
            alphaMapRef := addOneToMany pattern (depth, j)  !alphaMapRef
            Seq.singleton j
        let testJoins =
          let f testNode =
            let tests, ptree = collectTests testNode
            let betaMem = mkBetaMem <| Array.collect (loop (depth + 1)) [|ptree|]
            let test testEnv = Seq.forall (fun test -> test testEnv) tests
            let j = mkJoin test [| betaMem |]
            alphaMapRef := addOneToMany pattern (depth, j)  !alphaMapRef
            j
          Seq.map f testChildren
        let nodes = Seq.append nonTestJoins testJoins
        Array.ofSeq nodes

    let createAlphaNode (numberedJoinNodes: (int * ReteNode<_>) list) =
      let orderedJoinNodes = Seq.map snd <| List.sortBy (fun (depth, _) -> -depth) numberedJoinNodes
      mkAlphaMem <| Seq.toArray orderedJoinNodes
    let graph =
      mkBetaMemDummy << Array.ofSeq <| Seq.collect (fun ptree -> loop 0 ptree) ptrees,
        Map.toSeq <| Map.map (fun _ l -> createAlphaNode l) !alphaMapRef
    setBackPointers graph
    graph
