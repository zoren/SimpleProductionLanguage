namespace PatternMatching

module ReteBuilder =
  open PatternMatching.PatternTree
  open PatternMatching.PatternDiscriminatorTree
  open PatternMatching.ReteNetwork

  let mkRete nodeType children = {nodeType = nodeType; children = children; parent = ref None}
  let mkBetaMem children = mkRete (Beta {tokens = ref Set.empty}) children
  let mkBetaMemDummy children = mkRete (Beta {tokens = ref <| Set.singleton []}) children
  let mkJoin test children = mkRete (Join <| RegularJoin{test = test;alphaMem = ref None}) children

  let mkAlphaMem children = {wmes = ref Set.empty;successors = children}

  let addOneToMany (valuePatterns:Pattern) v tree =
    match tryFindPattern valuePatterns tree with
    | None -> addPatternArray valuePatterns [v] tree
    | Some vs -> addPatternArray valuePatterns (v :: vs) tree

  // backpointer helpers
  let alphaMemsInAlphaNetwork (alphaNet:AlphaNetwork<_>) = allValues alphaNet

  let setBackPointers ((reteTopNode, alphaNet):ReteGraph<_>) =
    let rec setParents node =
      for child in node.children do
          child.parent := Some node
          setParents child
    let setAlphaMem alphaMem =
        for succ in alphaMem.successors do
            match succ.nodeType with
            | Join jdn ->
              match jdn with
              | RegularJoin jd -> jd.alphaMem := Some alphaMem
            | _ -> ()
    setParents reteTopNode
    Seq.iter setAlphaMem <| alphaMemsInAlphaNetwork alphaNet

  let reteGraphFromPatternTrees ptrees : ReteGraph<_> =
    let alphaMapRef = ref empty
    let rec loop depth =
      function
      | PatternTree.Production prodId -> mkRete (Production prodId) [||]
      | PatternNode(pattern, test, children) ->
        let betaMem = mkBetaMem <| Array.map (loop (depth + 1)) children
        let joinNode = mkJoin test [| betaMem |]
        alphaMapRef := addOneToMany pattern (depth, joinNode)  !alphaMapRef
        joinNode

    let createAlphaNode (numberedJoinNodes: (int * ReteNode<_>) list) =
      let orderedJoinNodes = Seq.map snd <| List.sortBy (fun (depth, _) -> -depth) numberedJoinNodes
      mkAlphaMem <| Seq.toArray orderedJoinNodes
    let graph =
      mkBetaMemDummy << Array.ofSeq <| Seq.map (fun ptree -> loop 0 ptree) ptrees,
        map createAlphaNode !alphaMapRef
    setBackPointers graph
    graph
