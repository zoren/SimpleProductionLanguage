namespace PatternMatching

// facts
module PatternTree =
    type Value =
        | Int of int
        | String of string
        | Double of double

    type Fact = Value array

    // patterns
    type ValueType = IntType | StringType | DoubleType
    type ValuePattern =
        | Anything of ValueType
        | PatternValue of Value

    type Pattern = ValuePattern array

    type TokenElement =
      | FactTokenElement of Fact

    type Variable = { tokenIndex : int; fieldIndex : int}
    type TestEnvironment = (Variable -> Value)

    type Test = TestEnvironment -> bool

    type PatternTree<'Production> =
        | PatternNode of Pattern * PatternTree<'Production> array
        | TestNode of Test * PatternTree<'Production>
        | Production of 'Production

    type Environment = TokenElement list

    type ConflictSet<'Production> when 'Production : comparison = ConflictSet of Set<'Production * Environment>

    let getInt =
      function
        | (Int i) -> i
        | v -> failwithf "Runtime type error: value not of the expected type %A" v

    let valueComp pred v v' =
      match v,v' with
      | Int _, Int _ | String _, String _ | Double _, Double _ -> pred v v'
      | _ -> failwith "type error, incompatible values compared"

    let matchValuePat (v:Value) (vp:ValuePattern) =
        match vp with
        | Anything valueType ->
          match v, valueType with
          | Int _, IntType | String _, StringType | Double _, DoubleType -> true
          | _ -> failwith "type error, value incompatible with anything type"
        | PatternValue v'  -> valueComp (=) v v'

    let matchFactPattern (patArgs:Pattern) (args:Fact) = Array.forall2 matchValuePat args patArgs
