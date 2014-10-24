namespace SPLToProjTree

module SPLToProjTree =
  open ProjectionTree.Tree
  open SimpleProductionLanguage.AST

  type SymbolTable = LValue list

  let lookupSymbolTable (symbolTable:SymbolTable) lvalue = List.findIndex lvalue symbolTable

  // naive version, uses local lval order
  let ruleToPTree ((abstrs, cond, action) as rule:Rule) : Node<_> =
    let lvalOrder = histogramToOrder <| histogramUnion (lvalDomCond cond) (lvalDomAction action)
    let condToTest (cond:Condition) : Test =
      failwith "not implemented"
    let loop _ =
      // todo abstract all lvals and then put in condition
      failwith "not implemented"
      //ActionNode action
    let rec loopAbstr env =
      function
      | [] -> loop env
      | (var, iType) :: abstrs -> PatternNode(InstanceNode iType, [loopAbstr (Variable var :: env) abstrs])
    loopAbstr [] abstrs