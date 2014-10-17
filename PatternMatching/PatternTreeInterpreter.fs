namespace PatternMatching

module PatternTreeInterpreter =
  open PatternMatching.PatternTree

  let matchTree ptree facts =
      let rec loop env =
        function
        | Production p -> Seq.singleton (p, env)
        | TestNode(test, ptree) ->
          let lookup (var:Variable) =
            let _,args = List.nth env var.tokenIndex
            Array.get args var.fieldIndex
          if test lookup
          then loop env ptree
          else Seq.empty
        | PatternNode(pat, children) ->
          let matchingFacts = Seq.filter (matchFactPattern pat) facts
          Seq.collect (fun fact -> Seq.collect (loop (fact::env)) children) matchingFacts
      Set.ofSeq <| loop [] ptree

  type PatternTreeState<'Production> = PatternTree<'Production> * Set<Fact> ref

  let mkEmptyState ptree : PatternTreeState<_> = ptree, ref Set.empty

  let activate ((ptree, factSet):PatternTreeState<_>) (fact:Fact) =
    if Set.contains fact !factSet
    then failwith "Could not activate: fact already active."
    let currentConflictSet = matchTree ptree !factSet
    factSet := Set.add fact !factSet
    let newConflictSet = matchTree ptree !factSet
    Set.difference newConflictSet currentConflictSet

  let deactivate ((ptree, factSet):PatternTreeState<_>) (fact:Fact) =
    if not <| Set.contains fact !factSet
    then failwith "Could not deactivate: fact not active."
    let currentConflictSet = matchTree ptree !factSet
    factSet := Set.remove fact !factSet
    let newConflictSet = matchTree ptree !factSet
    Set.difference currentConflictSet newConflictSet


