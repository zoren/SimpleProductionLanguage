namespace SPLRete

module SPLToRete =
  open SimpleProductionLanguage.AST
  open PatternMatching.PatternTree

  let convertBinop =
    function
    | SimpleProductionLanguage.AST.BinOperator.Plus -> Plus
    | SimpleProductionLanguage.AST.BinOperator.Minus -> Minus
    | SimpleProductionLanguage.AST.BinOperator.Times -> Times

  let ruleToPTree ((abstrs, cond, action) as rule:Rule) : PatternTree<_> =
    let abstrList = Seq.toList <| abstrToSeq abstrs
    let lvals = lvalDomRule rule
    let rec loopAbstr env =
      function
      | (varName, instType) :: abstrs ->
        let pattern = "class", [|Anything IntType; PatternValue <| String instType|]
        PatternNode(pattern, [|loopAbstr (LValue.Variable varName :: env) abstrs|])
      | [] ->
        let loopLVal env' lval =
          let var, fields = lvalInsideOut lval
          Seq.fold (fun (lval, e) field -> let lval' = Proj(lval, field)
                                           lval', lval' :: e
          ) (LValue.Variable var, env') fields
        let loopLVal' e l = snd <| loopLVal e l
        let env' = Set.fold loopLVal' env lvals
        let lvalsInCond = Set.fold loopLVal' [] lvals
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
        let rec build runningEnv =
          function
          | [] -> Production (runningEnv, action)
          | Proj(LValue.Variable _ as lval, cstic) as curLVal :: lvals ->
            let newEnv = curLVal::runningEnv
            let index = List.findIndex ((=)lval) newEnv
            let pattern = "assign", [| Anything IntType; PatternValue <| String cstic; Anything IntType|]
            let targetWMEOffset =
              match lval with
              | Proj _ -> 2
              | LValue.Variable _ -> 0
            let objEqTest = Comparison(Variable(0,0), Eq, Variable(index, targetWMEOffset))
            let pnode = build newEnv lvals
            let testsNode =
              match lvals with
              | [] ->
                Seq.fold (fun n test -> TestNode(test, n)) pnode << List.rev <| objEqTest :: tests
              | _ -> TestNode(objEqTest, pnode)
            PatternNode(pattern, [| testsNode |])
        build env (List.rev lvalsInCond)
    loopAbstr [] abstrList
