namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type WME = Value array
  type TokenElement = WME
  type Token = TokenElement list
  
  type BetaMemory = { tokens : Token list ref }

  type NodeType = 
    | Beta of BetaMemory
    | Join of JoinData
    | Production of ProductionId

  and JoinData = {
    alphaMem : AlphaMemory option ref
    tests : Test array
    }

  and AlphaMemory = {
    wmes : WME list ref
    successors : ReteNode array
  }

  and ReteNode = {
    nodeType : NodeType
    children : ReteNode array
    parent : ReteNode option ref
  }


  type ReteGraph = ReteNode array

  let evalTest (token:Token) (test:Test) : bool = failwith ""

  type ActivationFlag = Activate | Deactivate

  let processAlphaMem (flag:ActivationFlag) (alphaMem:AlphaMemory) (w:WME) =
    let delta = ref []
    let rec joinNodeRight ({nodeType = Join jd} as node) (w : WME) : unit =
     ()
    and joinNodeLeft  ({nodeType = Join jd} as node) (token : Token) : unit =
      ()
    and betaMemoryLeft  ({nodeType = Beta betaMem} as node) (t:Token) (tokElement : TokenElement) : unit =
      ()
    and leftActivation (node:ReteNode) (t:Token) (tokElementOpt:TokenElement option) : unit =
      ()
    for child in alphaMem.successors do
      joinNodeRight child w
    !delta

// interpretation
  let activate (rete:ReteGraph) (fact:Fact) : ConflictSet =
    failwith ""