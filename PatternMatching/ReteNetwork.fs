namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type WME = Fact
  type TokenElement =
    | WMETokenElement of WME
//    | ListTokenElement of WME
  type Token = TokenElement list
  
  type BetaMemory = { tokens : Token list ref }

  type NodeType<'Production> = 
    | Beta of BetaMemory
    | Join of JoinData<'Production>
    | Production of 'Production

  and JoinData<'Production> = {
    alphaMem : AlphaMemory<'Production> option ref
    tests : Test array
    }

  and AlphaMemory<'Production> = {
    wmes : WME list ref
    successors : ReteNode<'Production> array
  }

  and ReteNode<'Production> = {
    nodeType : NodeType<'Production>
    children : ReteNode<'Production> array
    parent : ReteNode<'Production> option ref
  }

  type AlphaNetwork<'Production> =
    seq<Pattern * AlphaMemory<'Production>>

  type ReteGraph<'Production> = ReteNode<'Production> * AlphaNetwork<'Production>
