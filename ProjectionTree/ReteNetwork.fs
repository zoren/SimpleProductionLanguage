namespace ProjectionTree

module ReteNetwork =
  open ProjectionTree.Tree
  type InstanceId = int
  type WME =
    | Instance of InstanceId * InstanceType
    | Assignment of InstanceId * Cstic * ValueDomainFacet * Value
    | Unit of InstanceId * Cstic * PresentationFacet

  type Fact = WME
  type TokenElement =
    | FactTokenElement of Fact
    | NestedTokenElement of Set<Fact>
  type Token = TokenElement list

  type BetaMemory = { tokens : Set<Token> ref }

  type NodeType<'Production> =
    | Beta of BetaMemory
    | Join of JoinData<'Production>
    | Production of 'Production

  and JoinData<'Production> = {
    alphaMem : AlphaMemory<'Production> option ref
    tests : Test list
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

  type PatternMap<'Value> =
    {
      InstanceMap : Map<InstanceType,'Value>
      AssignmentFacetMap : Map<Cstic * Facet,'Value>
      PartOf : 'Value
    }

  type AlphaNetwork<'Production> = PatternMap<AlphaMemory<'Production>>

  type ReteGraph<'Production> = ReteNode<'Production> * AlphaNetwork<'Production>
