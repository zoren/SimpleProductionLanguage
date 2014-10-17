﻿namespace PatternMatching

module ReteInterpreter =
  open PatternMatching.PatternTree
  open PatternMatching.ReteNetwork

  let lookupToken token (var:Variable) =
    let (WMETokenElement (_, values)) = List.nth token var.tokenIndex
    Array.get values var.fieldIndex

  let evalTest token test =
    let lookup = lookupToken token
    test lookup

  type ActivationFlag = Activate | Deactivate | Modify of newWME : WME

  let processAlphaMem (flag:ActivationFlag) (alphaMem:AlphaMemory<'Production>) (w:WME) : ('Production * Token) list=
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
    let rec joinNodeRight ({nodeType = Join jd} as node) (w : WME) : unit =
      let (Some({nodeType = Beta bm})) = !node.parent
      for t in !bm.tokens do
          if evalTest ((WMETokenElement w)::t) jd.test then
            for child in node.children do
              leftActivation child t (Some <| WMETokenElement w)
    and joinNodeLeft  ({nodeType = Join jd} as node) (token : Token) : unit =
      let alphaMem = Option.get !jd.alphaMem
      for w in !alphaMem.wmes do
          if evalTest ((WMETokenElement w) :: token) jd.test then
            for child in node.children do
              leftActivation child token (Some <| WMETokenElement w)
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
          | WMETokenElement wme ->
            match tokenElement with
            | WMETokenElement wme' -> wme = wme'
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

  let naiveFindAlphaMemIds patMap (fact:Fact) =
    Seq.choose (fun(pattern, alphaMem) -> if matchFactPattern pattern fact then Some alphaMem else None) patMap

  let naiveFindAlphaMems (alphaNet:AlphaNetwork<_>) (fact:Fact) =
    let patMap, alphaMemMap = alphaNet
    let alphaMemIds = Seq.choose (fun(pattern, alphaMem) -> if matchFactPattern pattern fact then Some alphaMem else None) patMap
    Seq.map (fun alphaMemId -> Map.find alphaMemId alphaMemMap) alphaMemIds


  let tokenElementToWME =
    function
    | WMETokenElement wme -> wme

  let processWithFlag flag alphaMems (fact:Fact) =
    let setRef = ref Set.empty
    for alphaMem in alphaMems do
      let cs = Seq.map (fun(prod,values) -> prod, List.map tokenElementToWME values)<| processAlphaMem flag alphaMem fact
      let f set conflict =
        Set.add conflict set
      setRef := Seq.fold f !setRef cs
    !setRef

  let processFact flag ((_, alphaNet):ReteGraph<_>) (fact:Fact) =
    let patMap, alphaMemMap = alphaNet
    let processAlphaMemIds flag alphaMemIds =
      let alphaMems = Seq.map (fun alphaMemId -> Map.find alphaMemId alphaMemMap) alphaMemIds
      processWithFlag flag alphaMems
    match flag with
    | Activate  ->
      let alphaMemIds = naiveFindAlphaMemIds patMap fact
      processAlphaMemIds flag alphaMemIds fact, Set.empty, Set.empty
    | Deactivate ->
      let alphaMemIds = naiveFindAlphaMemIds patMap fact
      Set.empty, Set.empty, processAlphaMemIds flag alphaMemIds fact
    | Modify newFact ->
      let oldFact = fact
      let oldAlphaMemIdSet = Set.ofSeq <| naiveFindAlphaMemIds patMap oldFact
      let newAlphaMemIdSet = Set.ofSeq <| naiveFindAlphaMemIds patMap newFact

      let stillActiveIdSet = Set.intersect newAlphaMemIdSet oldAlphaMemIdSet
      let toActivate = Set.difference newAlphaMemIdSet oldAlphaMemIdSet
      let toDeactivate = Set.difference oldAlphaMemIdSet newAlphaMemIdSet

      processAlphaMemIds Activate toActivate fact, processAlphaMemIds flag stillActiveIdSet fact, processAlphaMemIds Deactivate toDeactivate fact

//      let alphaMems = Set.ofSeq <| naiveFindAlphaMems alphaNet fact
//      processWithFlag flag alphaMems

  let activate (graph:ReteGraph<_>) (fact:Fact) = processFact Activate graph fact

  let deactivate (graph:ReteGraph<_>) (fact:Fact) = processFact Deactivate graph fact