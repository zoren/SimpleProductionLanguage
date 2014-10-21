namespace PatternMatching

module PatternTreeInterpreter =
  open PatternMatching.PatternTree

  let lookupEnv (env:Environment) (var:Variable) =
    match List.nth env var.tokenIndex with
    | FactTokenElement args -> Array.get args var.fieldIndex

  let matchTree ptree facts : Set<'Action * Environment> =
      let rec loop (env:Environment) =
        function
        | Production p -> Seq.singleton (p, env)
        | TestNode(test, ptree) ->
          if test (lookupEnv env)
          then loop env ptree
          else Seq.empty
        | PatternNode(pat, children) ->
          let matchingFacts = Seq.filter (matchFactPattern pat) facts
          Seq.collect (fun fact -> Seq.collect (loop ((FactTokenElement fact)::env)) children) matchingFacts
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
