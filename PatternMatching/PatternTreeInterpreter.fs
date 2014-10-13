namespace PatternMatching

module PatternTreeInterpreter =
  open PatternMatching.PatternTree

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

  let matchTree ptree facts =
      let rec loop env =
        function
        | Production p -> Seq.singleton (p, env)
        | TestNode(test, ptree) ->
          if evalTest env test
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


