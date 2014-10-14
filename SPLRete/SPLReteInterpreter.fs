namespace SPLRete

module SPLInterpreter =
  open SPLRete.Interpreter
  open SimpleProductionLanguage.AST
  open PatternMatching.PatternTree
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
      let fact = "class", [| Int instId; String className |]
      ignore <| interp.Add fact
      instId

    member __.assign instId cstic value =
      let fact = "assign", [| Int instId; String cstic; Int value |]
      ignore <| interp.Add fact
