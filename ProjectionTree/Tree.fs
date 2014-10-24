namespace ProjectionTree

module Tree =
  type InstanceType = string
  type Cstic = string
  type ValueDomainFacet = ValueFacet | DomainFacet

  type PresentationFacet = Invisible | Required | Nil

  type Facet =
    | PresentationFacet of PresentationFacet
    | ValueDomainFacet of ValueDomainFacet

  type TargetIndex = int

  type CompOp = EQ | LT
  type FieldSelector = InstField | ValueField
  type Value =
    | Int of int
    | String of string
    | Double of double

  type Variable = TargetIndex

  type Exp =
    | Constant of Value
    | Deref of Variable

  type Test =
    | Comparison of Exp * CompOp * Exp

  type PartTest =
    | PartOf of Variable * Variable
    | SubpartOf of Variable * Variable

  type Pattern =
    | InstanceNode of InstanceType
    | ProjectionNode of Variable * Cstic * Facet
    | PartOfPattern of PartTest

  type Node<'Action when 'Action : comparison> =
    | PatternNode of Pattern * Node<'Action> list
    | TestNode of Test * Node<'Action>
    | ActionNode of 'Action
