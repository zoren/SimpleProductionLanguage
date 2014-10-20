namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type WME = Fact
  type TokenElement =
    | WMETokenElement of WME
//    | ListTokenElement of WME
  type Token = TokenElement list

  type BetaMemory = { tokens : Set<Token> ref }

  type NodeType<'Production> =
    | Beta of BetaMemory
    | Join of JoinData<'Production>
    | Production of 'Production

  and JoinData<'Production> = {
    alphaMem : AlphaMemory<'Production> option ref
    test : Test
    }

  and AlphaMemory<'Production> = {
    wmes : Set<WME> ref
    successors : ReteNode<'Production> array
  }

  and ReteNode<'Production> = {
    nodeType : NodeType<'Production>
    children : ReteNode<'Production> array
    parent : ReteNode<'Production> option ref
  }

  type PatternDiscriminatorTree<'value> =
    PTreeAny of 'value option * PatternDiscriminatorTree<'value> option * Map<Value, PatternDiscriminatorTree<'value>>

  type AlphaNetwork<'Production> = PatternDiscriminatorTree<AlphaMemory<'Production>>

  type ReteGraph<'Production> = ReteNode<'Production> * AlphaNetwork<'Production>

  let empty = PTreeAny(None, None, Map.empty)
