namespace SPLRete

module SPLToRete =
  open SimpleProductionLanguage.AST
  open PatternMatching.PatternTree

  let convertBinop =
    function
    | SimpleProductionLanguage.AST.BinOperator.Plus -> Plus
    | SimpleProductionLanguage.AST.BinOperator.Minus -> Minus
    | SimpleProductionLanguage.AST.BinOperator.Times -> Times

  let ruleToPTree ((abstrs, cond, action) as rule:Rule) : PatternTree =
    let abstrList = Seq.toList <| abstrToSeq abstrs
    let lvals = lvalDomRule rule
    let rec loopAbstr env =
      function
      | (varName, instType) :: abstrs ->
        let pattern = "class", [|PatternValue <| String instType|]
        PTree(pattern, [], [|loopAbstr (LValue.Variable varName :: env) abstrs|]) 
      | [] ->
        let loopLVal env' lval =
          let var, fields = lvalInsideOut lval
          Seq.fold (fun (lval, e) field -> let lval' = Proj(lval, field)
                                           lval', lval' :: e
          ) (LValue.Variable var, env') fields
        let loopLVal' e l = snd <| loopLVal e l
        let env' = Set.fold loopLVal' env lvals
        let findIndex lval = List.findIndex ((=)lval) env'
        let rec convertExp =
          function
          | Expression.Constant i -> Const <| Int i
          | Deref lval ->
            Variable(findIndex lval,2)
          | Expression.BinOp(el, binop, e2) ->
            BinOp(convertExp el, convertBinop binop, convertExp e2)
        let tests =
          match cond with
          | True -> []
          | LessThan(el, er) -> [Comparison(convertExp el, Lt, convertExp er)]
        let rec build =
          function
          | [lval] ->
            let index = findIndex lval
            let pattern = "assign", [| Anything IntType; PatternValue <| Int index; Anything IntType|]
            PTree(pattern, [], [| Production <| sprintf "%A" action |])
          | lval :: lvals ->
            let index = findIndex lval
            let pattern = "assign", [| Anything IntType; PatternValue <| Int index; Anything IntType|]
            PTree(pattern, tests, [| build lvals |])            
        build env
    loopAbstr [] abstrList