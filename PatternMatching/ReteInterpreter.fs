namespace PatternMatching

module ReteInterpreter =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let lookupToken token (var:Variable) =
    let (WMETokenElement (_, values)) = List.nth token var.tokenIndex
    Array.get values var.fieldIndex

  let evalTests token tests =
    let lookup = lookupToken token
    Seq.forall (fun test -> test lookup) tests

  type ActivationFlag = Activate | Deactivate

  let processAlphaMem (flag:ActivationFlag) (alphaMem:AlphaMemory<'Production>) (w:WME) : ('Production * Token) list=
    match flag with
    | Activate ->
      if List.exists ((=)w) !alphaMem.wmes
      then failwithf "already added %A" w
      alphaMem.wmes := w :: !alphaMem.wmes
    | Deactivate ->
      if not <| List.exists ((=)w) !alphaMem.wmes
      then failwith "not added"
      alphaMem.wmes := List.filter ((<>)w) !alphaMem.wmes
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
    and leftActivation (node:ReteNode<_>) (t:Token) (tokElementOpt:TokenElement option) : unit =
      match node.nodeType, tokElementOpt with
      | Beta _, Some tokElement -> betaMemoryLeft node t tokElement
      | Join _, None -> joinNodeLeft node t
      | Production s, None ->
        delta := (s, t) :: !delta
    for child in alphaMem.successors do
      joinNodeRight child w
    !delta

  let naiveFindAlphaMems (patMap:seq<Pattern * AlphaMemory<_>>) (fact:Fact) =
    Seq.choose (fun(pattern, alphaMem) -> if matchFactPattern pattern fact then Some alphaMem else None) patMap

  let tokenElementToWME =
    function
    | WMETokenElement wme -> wme

// interpretation
  let processFact flag ((_, alphaNet):ReteGraph<_>) (fact:Fact) =
    let alphaMems = naiveFindAlphaMems alphaNet fact
    let setRef = ref Set.empty
    for alphaMem in alphaMems do
      let cs = Seq.map (fun(prod,values) -> prod, List.map tokenElementToWME values)<| processAlphaMem flag alphaMem fact
      let f set conflict =
        Set.add conflict set
      setRef := Seq.fold f !setRef cs
    !setRef

  let activate (graph:ReteGraph<_>) (fact:Fact) = processFact Activate graph fact

  let deactivate (graph:ReteGraph<_>) (fact:Fact) = processFact Deactivate graph fact