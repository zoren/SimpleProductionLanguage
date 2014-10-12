namespace PatternMatching

// facts
module PatternTree =
    type Value =
        | Int of int
        | String of string
        | Double of double

    type FactKind = string

    type Fact = FactKind * Value array

    // patterns
    type ValueType = IntType | StringType | DoubleType
    type ValuePattern =
        | Anything of ValueType
        | PatternValue of Value

    type Pattern = FactKind * ValuePattern array

    type ProductionId = string

    type BinOperator = Plus | Minus | Times | Division

    type Exp =
        | Const of Value
        | Variable of tokenIndex : int * fieldIndex : int
        | BinOp of Exp * BinOperator * Exp

    type ComparisonOperator = Eq | Lt

    type Test =
        Comparison of Exp * ComparisonOperator * Exp

    type PatternTree =
        | PTree of Pattern * Test list * PatternTree array
        | Production of ProductionId

// interpretation  
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
    
    let matchFactPattern ((patFactKind, patArgs):Pattern)  ((factKind, args):Fact) =
        factKind = patFactKind && Array.forall2 matchValuePat args patArgs

    type Environment = Fact list

    let compFunc =
      function
      | Eq -> valueComp (=)
      | Lt -> valueComp (<)

    let evalTest (env:Environment) test =
        let evalExp =
            function
                | Const v -> v
                | Variable(tokenIndex, fieldIndex) ->
                  let _,args = List.nth env tokenIndex
                  Array.get args fieldIndex
        match test with
            | Comparison (e1, comp, e2) ->
              compFunc comp (evalExp e1) (evalExp e2)

    type ConflictSet = Set<ProductionId * Environment>

    let matchTree ptree facts : ConflictSet =
        let rec loop env =
          function
          | Production p -> Seq.singleton (p, env)
          | PTree(pat, tests, children) ->
            let matchingFacts = Seq.filter (matchFactPattern pat) facts
            let evalTestWithEnv newEnv =
              if Seq.forall (evalTest newEnv) tests
              then Seq.collect (loop newEnv) children
              else Seq.empty
            Seq.collect (fun fact -> evalTestWithEnv (fact::env)) matchingFacts
        Set.ofSeq <| loop [] ptree

    type PatternTreeState = PatternTree * Set<Fact> ref

    let mkEmptyState ptree : PatternTreeState = ptree, ref Set.empty

    let activate ((ptree, factSet):PatternTreeState) (fact:Fact) : ConflictSet =
      if Set.contains fact !factSet
      then failwith "Could not activate: fact already active."
      let currentConflictSet = matchTree ptree !factSet
      factSet := Set.add fact !factSet
      let newConflictSet = matchTree ptree !factSet
      Set.difference newConflictSet currentConflictSet

    let deactivate ((ptree, factSet):PatternTreeState) (fact:Fact) : ConflictSet =
      if not <| Set.contains fact !factSet
      then failwith "Could not deactivate: fact not active."
      let currentConflictSet = matchTree ptree !factSet
      factSet := Set.remove fact !factSet
      let newConflictSet = matchTree ptree !factSet
      Set.difference currentConflictSet newConflictSet
