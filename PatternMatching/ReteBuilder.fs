namespace PatternMatching

module ReteBuilder =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let empty = PTreeAny(None, None, Map.empty)

  let optDefault opt defaultVal =
    match opt with
    | Some v -> v
    | None -> defaultVal

  let addPatternArray (pattern:Pattern) value tree =
    let rec loop i (PTreeAny(valueOpt, anythingOpt, childMap)) =
      if i = pattern.Length
      then PTreeAny(Some value, anythingOpt, childMap)
      else
        let keyVal = Array.get pattern i
        match keyVal with
        | Anything _ ->
          PTreeAny(valueOpt, Some <| insertOption anythingOpt i, childMap)
        | PatternValue pv ->
          let child = Map.tryFind pv childMap
          let newTree = insertOption child i
          PTreeAny(valueOpt, anythingOpt, Map.add pv newTree childMap)
    and insertOption opt i = loop (i + 1) <| optDefault opt empty
    loop 0 tree

  let tryFindPattern (patternValues : ValuePattern array) (tree:PatternDiscriminatorTree<'value>) : 'value option =
    let rec loop i node =
      match node with
      | PTreeAny(valueOpt, anythingOpt, childrenMap) ->
        if i = patternValues.Length
        then valueOpt
        else
          if i > patternValues.Length
          then
            None
          else
            let patternValue = Array.get patternValues i
            let childNode =
              match patternValue with
              | Anything _ -> anythingOpt
              | PatternValue value -> Map.tryFind value childrenMap
            Option.bind (fun anything -> loop(i+1) anything) childNode
    loop 0 tree

  let rec allValues (PTreeAny(valueOpt, anythingOpt, childrenMap)) : seq<_> =
    Seq.concat [Option.toArray valueOpt :> seq<_>
                optDefault (Option.map allValues anythingOpt) Seq.empty
                Seq.collect allValues << Seq.map snd <| Map.toSeq childrenMap
                ]
  let rec map f (PTreeAny(valueOpt, anythingOpt, childrenMap)) =
    PTreeAny(Option.map f valueOpt, Option.map (map f) anythingOpt, Map.map (fun _ v -> map f v) childrenMap)


  let mkRete nodeType children = {nodeType = nodeType; children = children; parent = ref None}
  let mkBetaMem children = mkRete (Beta {tokens = ref Set.empty}) children
  let mkBetaMemDummy children = mkRete (Beta {tokens = ref <| Set.singleton []}) children
  let mkJoin test children = mkRete (Join {test = test;alphaMem = ref None}) children

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
    let alphaMapRef = ref empty
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
        map createAlphaNode !alphaMapRef
    setBackPointers graph
    graph
