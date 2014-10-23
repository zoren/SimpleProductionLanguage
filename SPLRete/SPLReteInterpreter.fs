namespace SPLRete

module SPLInterpreter =
  open SPLRete.Interpreter
  open SimpleProductionLanguage.AST
  open PatternMatching.ReteBuilder
  open SPLToRete

  type SPLInterpreter(rules:seq<Rule>) =
    let interp =
      let pTrees = Array.ofSeq <| Seq.map ruleToPTree rules
      let rete = reteGraphFromPatternTrees pTrees
      new Interpreter(rete)

    member __.Interpreter = interp

    member __.create className =
      let instId = interp.GetMaxInstId() + 1
      let fact = mkInstanceFact instId className
      ignore <| interp.Add fact
      instId

    member __.remove instId className =
      let fact = mkInstanceFact instId className
      ignore <| interp.Remove fact

    member __.assign instId cstic value =
      let fact = mkAssignFact instId cstic value
      ignore <| interp.Add fact

    member __.unassign instId cstic value =
      let fact = mkAssignFact instId cstic value
      ignore <| interp.Remove fact

    member __.partOf instId parentId =
      let fact = mkPartOfFact instId parentId
      ignore <| interp.Add fact

    member __.notPartOf instId parentId =
      let fact = mkPartOfFact instId parentId
      ignore <| interp.Remove fact
