namespace PatternMatching

module ReteBuilder =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let mkRete nodeType children = {nodeType = nodeType; children = children; parent = ref None}
  let mkBetaMem children = mkRete (Beta {tokens = ref []}) children
  let mkBetaMemDummy children = mkRete (Beta {tokens = ref [[]]}) children
  let mkJoin tests children = mkRete (Join {tests = tests;alphaMem = ref None}) children

  let mkAlphaMem children = {wmes = ref [];successors = children}

  let addOneToMany k v m =
    match Map.tryFind k m with
    | None -> Map.add k [v] m
    | Some vs -> Map.add k (v::vs) m

  // backpointer helpers
  let alphaMemsInAlphaNetwork (alphaNet:AlphaNetwork) = Seq.map snd alphaNet

  let setBackPointers ((reteTopNode, alphaNet):ReteGraph) =
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

  let reteGraphFromPatternTrees ptrees : ReteGraph =
    let alphaMapRef = ref Map.empty
    let rec loop depth =
      function
      | PatternTree.Production prodId -> mkRete (Production prodId) [||]
      | PTree(pattern, tests, children) ->
        let betaMem = mkBetaMem <| Array.map (loop (depth + 1)) children
        let j = mkJoin (Array.ofSeq tests) [| betaMem |]
        alphaMapRef := addOneToMany pattern (depth, j)  !alphaMapRef
        j
    let createAlphaNode (numberedJoinNodes: (int * ReteNode) list) =
      let orderedJoinNodes = Seq.map snd <| List.sortBy (fun (depth, _) -> -depth) numberedJoinNodes
      mkAlphaMem <| Seq.toArray orderedJoinNodes
    let graph =
      mkBetaMemDummy << Array.ofSeq <| Seq.map (fun ptree -> loop 0 ptree) ptrees,
        Map.toSeq <| Map.map (fun _ l -> createAlphaNode l) !alphaMapRef
    setBackPointers graph
    graph
