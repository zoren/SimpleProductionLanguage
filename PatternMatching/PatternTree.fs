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

    type Environment = TokenElement list
    type TestEnvironment = Fact * TokenElement list

    type Test = Set<Fact> -> TestEnvironment -> TokenElement option

    type PatternTree<'Production> =
        | PatternNode of Pattern * Test * PatternTree<'Production> array
        | Production of 'Production

    type ConflictSet<'Production> when 'Production : comparison = ConflictSet of Set<'Production * Environment>

    let getInt =
      function
        | (Int i) -> i
        | v -> failwithf "Runtime type error: value not of the expected type %A" v

    let lookupEnv (env:Environment) (var:Variable) =
      match List.nth env var.tokenIndex with
      | FactTokenElement args -> Array.get args var.fieldIndex

    let lookupTestEnv ((fact, token):TestEnvironment) (var:Variable) =
      let envFact =
        if var.tokenIndex = 0
        then fact
        else
          let (FactTokenElement fact) = List.nth token (var.tokenIndex - 1)
          fact
      Array.get envFact var.fieldIndex
