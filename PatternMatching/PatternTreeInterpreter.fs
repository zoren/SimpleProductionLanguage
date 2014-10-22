namespace PatternMatching

module PatternTreeInterpreter =
  open PatternMatching.PatternTree

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

  let matchTree ptree facts : Set<'Action * Environment> =
    let rec loop (env:Environment) =
      function
      | Production p -> Seq.singleton (p, env)
      | PatternNode(pat, test, children) ->
        let matchingFacts = Set.ofSeq <| Seq.filter (matchFactPattern pat) facts
        let matchFact matchingFact =
          let testEnv : TestEnvironment = matchingFact, env
          match test matchingFacts testEnv with
          | Some tokenElement -> Seq.collect (loop (tokenElement::env)) children
          | None -> Seq.empty
        Seq.collect matchFact matchingFacts
    Set.ofSeq <| loop [] ptree

  type PatternTreeState<'Production> = PatternTree<'Production> * Set<Fact> ref

  let mkEmptyState ptree : PatternTreeState<_> = ptree, ref Set.empty

  let activate ((ptree, factSet):PatternTreeState<'Action>) (fact:Fact) : Set<'Action * Environment> =
    if Set.contains fact !factSet
    then failwith "Could not activate: fact already active."
    let currentConflictSet = matchTree ptree !factSet
    factSet := Set.add fact !factSet
    let newConflictSet = matchTree ptree !factSet
    Set.difference newConflictSet currentConflictSet

  let deactivate ((ptree, factSet):PatternTreeState<'Action>) (fact:Fact) : Set<'Action * Environment> =
    if not <| Set.contains fact !factSet
    then failwith "Could not deactivate: fact not active."
    let currentConflictSet = matchTree ptree !factSet
    factSet := Set.remove fact !factSet
    let newConflictSet = matchTree ptree !factSet
    Set.difference currentConflictSet newConflictSet
