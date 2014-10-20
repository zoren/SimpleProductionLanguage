namespace SPLRete

module SPLToRete =
  open SimpleProductionLanguage.AST
  open PatternMatching.PatternTree

  let compExp env exp =
    let rec loop =
      function
      | Expression.Constant i -> fun _ -> Int i
      | Deref lval ->
        let index = List.findIndex ((=)lval) env
        fun testEnv -> testEnv {tokenIndex = index; fieldIndex = 3}
      | Expression.BinOp(el, binop, er) ->
        let cel = loop el
        let cer = loop er
        let opFunc = evalOp binop
        fun testEnv ->
          let vl = getInt <| cel testEnv
          let vr = getInt <| cer testEnv
          Int <| opFunc vl vr
    loop exp

  let ruleToPTree ((abstrs, cond, action) as rule:Rule) : PatternTree<_> =
    let abstrList = Seq.toList <| abstrToSeq abstrs
    let lvals = lvalDomRule rule
    let rec loopAbstr env =
      function
      | (varName, instType) :: abstrs ->
        let pattern = [|PatternValue <| String "class"; Anything IntType; PatternValue <| String instType|]
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

        let tests =
          match cond with
          | True -> []
          | LessThan(el, er) ->
            let cel = compExp env' el
            let cer = compExp env' er
            let lessThan (testEnv:TestEnvironment) =
              let vl = getInt <| cel testEnv
              let vr = getInt <| cer testEnv
              vl < vr
            [lessThan]
        let rec build runningEnv =
          function
          | [] -> Production (runningEnv, action)
          | Proj(LValue.Variable _ as lval, cstic) as curLVal :: lvals ->
            let newEnv = curLVal::runningEnv
            let index = List.findIndex ((=)lval) newEnv
            let pattern = [|PatternValue <| String "assign"; Anything IntType; PatternValue <| String cstic; Anything IntType|]
            let targetWMEOffset =
              match lval with
              | Proj _ -> 3
              | LValue.Variable _ -> 1
            let objEqTest (testEnv:TestEnvironment) =
              let thisVal = getInt <| testEnv {tokenIndex = 0; fieldIndex = 1}
              let tokenVal = getInt <| testEnv {tokenIndex = index; fieldIndex = targetWMEOffset}
              thisVal = tokenVal
            let pnode = build newEnv lvals
            let testsNode =
              match lvals with
              | [] ->
                Seq.fold (fun n test -> TestNode(test, n)) pnode << List.rev <| objEqTest :: tests
              | _ -> TestNode(objEqTest, pnode)
            PatternNode(pattern, [| testsNode |])
        build env (List.rev lvalsInCond)
    loopAbstr [] abstrList
