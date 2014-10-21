namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type WME = Fact
  type Token = TokenElement list

  type BetaMemory = { tokens : Set<Token> ref }

  type NodeType<'Production> =
    | Beta of BetaMemory
    | Join of JoinNodeType<'Production>
    | Production of 'Production

  and JoinNodeType<'Production> =
    | RegularJoin of JoinData<'Production>

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

  open PatternMatching.PatternDiscriminatorTree

  type AlphaNetwork<'Production> = PatternDiscriminatorTree<AlphaMemory<'Production>>

  type ReteGraph<'Production> = ReteNode<'Production> * AlphaNetwork<'Production>
