namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type WME = Fact
  type TokenElement =
    | WMETokenElement of WME
//    | ListTokenElement of WME
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

  type AlphaNetwork =
    seq<Pattern * AlphaMemory>

  type ReteGraph = ReteNode * AlphaNetwork
