namespace PatternMatching

module ReteInterpreter =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let evalTest (token:Token) (test:Test) : bool =
    let evalExp =
      function
      | Const v -> v
      | Variable(tokenIndex, fieldIndex) ->
        let (WMETokenElement (_, values)) = List.nth token tokenIndex
        Array.get values fieldIndex  
    match test with
    | Comparison (e1, comp, e2) ->
      let compFunc =
        match comp with
        | Eq -> valueEq
      compFunc (evalExp e1) (evalExp e2)
  let evalTests token = Seq.forall (evalTest token)

  type ActivationFlag = Activate | Deactivate

  let processAlphaMem (flag:ActivationFlag) (alphaMem:AlphaMemory) (w:WME) =
    let delta = ref []
    let rec joinNodeRight ({nodeType = Join jd} as node) (w : WME) : unit =
      let (Some({nodeType = Beta bm})) = !node.parent
      for t in !bm.tokens do
          if evalTests ((WMETokenElement w)::t) jd.tests then
            for child in node.children do
              leftActivation child t (Some <| WMETokenElement w)
    and joinNodeLeft  ({nodeType = Join jd} as node) (token : Token) : unit =
      let alphaMem = Option.get !jd.alphaMem
      for w in !alphaMem.wmes do
          if evalTests ((WMETokenElement w) :: token) jd.tests then
            for child in node.children do
              leftActivation child token (Some <| WMETokenElement w)
    and betaMemoryLeft  ({nodeType = Beta betaMem} as node) (t:Token) (tokElement : TokenElement) : unit =
      let newToken = tokElement :: t
      match flag with
      | Activate ->
        betaMem.tokens := newToken :: !betaMem.tokens
      | Deactivate ->
        let tokenElementToRemove = tokElement
        let tokenFilter tokenElement =
          match tokenElementToRemove with
          | WMETokenElement wme ->
            match tokenElement with
            | WMETokenElement wme' -> wme = wme'
        betaMem.tokens := List.filter (fun token -> not <| List.exists tokenFilter token) !betaMem.tokens
      for child in node.children do
        leftActivation child newToken None
    and leftActivation (node:ReteNode) (t:Token) (tokElementOpt:TokenElement option) : unit =
      match node.nodeType, tokElementOpt with
      | Beta _, Some tokElement -> betaMemoryLeft node t tokElement
      | Join _, None -> joinNodeLeft node t
      | Production s, None ->
        delta := (s, t) :: !delta
    for child in alphaMem.successors do
      joinNodeRight child w
    !delta

  let naiveFindAlphaMems (patMap:seq<Pattern * AlphaMemory>) (fact:Fact) =
    Seq.choose (fun(pattern, alphaMem) -> if matchFactPattern pattern fact then Some alphaMem else None) patMap

  let tokenElementToWME = 
    function
    | WMETokenElement wme -> wme

// interpretation
  let processFact flag ((_, alphaNet):ReteGraph) (fact:Fact) : ConflictSet =
    let alphaMems = naiveFindAlphaMems alphaNet fact
    let setRef = ref Set.empty
    for alphaMem in alphaMems do
      let cs = Seq.map (fun(prod,values) -> prod, List.map tokenElementToWME values)<| processAlphaMem flag alphaMem fact
      let f set conflict =
        if Set.contains conflict set
        then failwith "this seems wrong"
        Set.add conflict set
      setRef := Seq.fold f !setRef cs
    !setRef

  let activate (graph:ReteGraph) (fact:Fact) = processFact Activate graph fact

  let deactivate (graph:ReteGraph) (fact:Fact) = processFact Deactivate graph fact