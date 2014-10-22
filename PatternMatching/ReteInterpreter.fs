namespace PatternMatching

module ReteInterpreter =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let evalTest set testEnv (test:Test) = test set testEnv

  type ActivationFlag = Activate | Deactivate

  let processAlphaMem (flag:ActivationFlag) (alphaMem:AlphaMemory<'Production>) (w:WME) : ('Production * Token) list =
    match flag with
    | Activate ->
      if Set.contains w !alphaMem.wmes
      then failwithf "already added %A" w
      alphaMem.wmes := Set.add w !alphaMem.wmes
    | Deactivate ->
      if not <| Set.contains w !alphaMem.wmes
      then failwith "not added"
      alphaMem.wmes := Set.remove w !alphaMem.wmes
    let delta = ref []
    let rec joinNodeRight ({nodeType = Join jdn} as node) (w : WME) : unit =
      let (Some({nodeType = Beta bm})) = !node.parent
      match jdn with
      | RegularJoin jd ->
        for t in !bm.tokens do
            match evalTest !alphaMem.wmes (w, t) jd.test with
            | Some tokenElement ->
              for child in node.children do
                leftActivation child t (Some tokenElement)
            | None -> ()
    and joinNodeLeft  ({nodeType = Join jdn} as node) (token : Token) : unit =
      match jdn with
      | RegularJoin jd ->
        let alphaMem = Option.get !jd.alphaMem
        for w in !alphaMem.wmes do
          match evalTest !alphaMem.wmes (w, token) jd.test with
          | Some tokenElement ->
            for child in node.children do
              leftActivation child token (Some tokenElement)
          | None -> ()

    and betaMemoryLeft  ({nodeType = Beta betaMem} as node) (t:Token) (tokElement : TokenElement) : unit =
      let newToken = tokElement :: t
      match flag with
      | Activate ->
        // todo maybe check and warn if token is already added?
        betaMem.tokens := Set.add newToken !betaMem.tokens
      | Deactivate ->
        let tokenElementToRemove = tokElement
        let tokenFilter tokenElement =
          match tokenElementToRemove with
          | FactTokenElement wme ->
            match tokenElement with
            | FactTokenElement wme' -> wme = wme'
        betaMem.tokens := Set.filter (fun token -> not <| List.exists tokenFilter token) !betaMem.tokens
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

  let processFact flag ((_, alphaNet):ReteGraph<_>) (fact:Fact) : Set<'Action * Environment> =
    let alphaMems = PatternDiscriminatorTree.tryFindValues fact alphaNet
    let setRef = ref Set.empty
    for alphaMem in alphaMems do
      let cs = processAlphaMem flag alphaMem fact
      setRef := Set.union !setRef <| Set.ofList cs
    !setRef

  let activate (graph:ReteGraph<_>) (fact:Fact) : Set<'Action * Environment> = processFact Activate graph fact

  let deactivate (graph:ReteGraph<_>) (fact:Fact) : Set<'Action * Environment> = processFact Deactivate graph fact
