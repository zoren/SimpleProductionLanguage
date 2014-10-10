namespace PatternMatching

module ReteNetwork =
  open PatternMatching.PatternTree

  type ReteGraph = unit

// interpretation
  let activate (rete:ReteGraph) (fact:Fact) : ConflictSet =
    failwith ""