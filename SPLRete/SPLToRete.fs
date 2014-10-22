﻿namespace SPLRete

module SPLToRete =
  open SimpleProductionLanguage.AST
  open PatternMatching.PatternTree

  let targetWMEOffset lval =
    match lval with
    | Proj _ -> 3
    | LValue.Variable _ -> 1

  let mkClassPattern instType = [|PatternValue <| String "class"; Anything IntType; PatternValue <| String instType|]
  let mkAssignPattern cstic = [|PatternValue <| String "assign"; Anything IntType; PatternValue <| String cstic; Anything IntType|]
  let mkPartOfPattern() = [|PatternValue <| String "partOf"; Anything IntType; Anything IntType|]

  let mkInstanceFact instId instType : Fact = [| String "class"; Int instId; String instType|]
  let mkAssignFact instId var value : Fact = [| String "assign"; Int instId; String var; Int value|]
  let mkPartOfFact instId parentInstId : Fact = [| String "partOf"; Int instId; Int parentInstId|]

  type SymbolTable = Map<LValue, int> list

  let lookupSymbolTable (symbolTable:SymbolTable) lvalue =
    let rec loop i (st::sts) =
      match Map.tryFind lvalue st with
      | Some fieldIndex -> {tokenIndex = i; fieldIndex = fieldIndex }
      | None -> loop (i+1) sts
    loop 0 symbolTable

  let compExp st exp =
    let rec loop =
      function
      | Expression.Constant i -> fun _ -> Int i
      | Deref lval ->
        let var = lookupSymbolTable st lval
        fun testEnv -> testEnv var
      | Expression.BinOp(el, binop, er) ->
        let cel = loop el
        let cer = loop er
        let opFunc = evalOp binop
        fun testEnv ->
          let vl = getInt <| cel testEnv
          let vr = getInt <| cer testEnv
          Int <| opFunc vl vr
    loop exp

  let mkSingleChildPatternNode pattern test child = PatternNode(pattern, test, [| child |])

  let fieldIndexAssignmentValue = 3
  let fieldIndexInstance = 1
  let fieldIndexPartOfChild = 1
  let fieldIndexPartOfParent = 2

  let lvalsInSymbolTable (st:SymbolTable) =
    Set.unionMany <| List.map (fun (m:Map<LValue, int>) -> Set.ofSeq << Seq.map fst <| Map.toSeq m) st

  let buildTest pred : Test =
              fun s ((fact,_) as env:TestEnvironment) ->
                if pred env
                then Some <| FactTokenElement fact
                else None

  let mkGenericObjEqTest factOffset var (env:TestEnvironment) =
    let thisVal = getInt <| lookupTestEnv env {tokenIndex = 0; fieldIndex = factOffset}
    let tokenVal = getInt <| lookupTestEnv env var
    thisVal = tokenVal

  let mkObjEqTest var (env:TestEnvironment) = mkGenericObjEqTest fieldIndexInstance var env

  let forAllPreds preds x = Seq.forall (fun pred -> pred x) preds

  let ruleToPTree ((abstrs, cond, action) as rule:Rule) : PatternTree<_> =
    let lvalsInCond = Set.ofSeq << Seq.map fst << Map.toSeq <| lvalDomCond cond
    let histogram = lvalDomRule rule
    let lvalOrder = Seq.toList <| histogramToOrder histogram

    let rec loop (st:SymbolTable) =
      function
      | [] -> Production (st, action)
      | lval :: lvals ->
        let loopLVals st = loop st lvals
        let buildTreeFromCondition pattern newST objEqTestOpt =
          let abstractedLVals = lvalsInSymbolTable newST
          let testOpt, child =
            if Set.isProperSubset lvalsInCond abstractedLVals then
              match cond with
              | PartOf(Deref childLVal, Deref parentLVal) ->
                let partOfPattern = mkPartOfPattern()
                let newSTPartOf = Map.empty :: newST
                let childVar = lookupSymbolTable newSTPartOf childLVal
                let parentVar = lookupSymbolTable newSTPartOf parentLVal
                let partOfObjEqTests = forAllPreds
                                              [mkGenericObjEqTest fieldIndexPartOfChild childVar
                                               mkGenericObjEqTest fieldIndexPartOfParent parentVar]
                let partOfNode = mkSingleChildPatternNode partOfPattern (buildTest partOfObjEqTests) (loopLVals newSTPartOf)
                None, partOfNode
              | Comparison(el, compOp, er) ->
                let cel = compExp newST el
                let cer = compExp newST er
                let cf = compOpToFunc compOp
                let compTest =
                  fun env ->
                    let vl = getInt << cel <| lookupTestEnv env
                    let vr = getInt << cer <| lookupTestEnv env
                    cf vl vr
                Some compTest, loopLVals newST
              | True -> None, loopLVals newST
            else
              None, loopLVals newST
          let preds = Seq.append (Option.toArray testOpt) (Option.toArray objEqTestOpt)
          mkSingleChildPatternNode pattern (buildTest <| forAllPreds preds) child
        match lval with
        | Variable v ->
          let iType = getType v abstrs
          let pattern = mkClassPattern iType
          let newST = (Map.ofList [lval,fieldIndexInstance]) :: st
          buildTreeFromCondition pattern newST None
        | Proj(lval', cstic) ->
          let pattern = mkAssignPattern cstic
          let newST = (Map.ofList[lval',fieldIndexInstance;lval, fieldIndexAssignmentValue]) :: st
          let var = lookupSymbolTable newST lval'
          let objEqTest = mkObjEqTest var
          buildTreeFromCondition pattern newST (Some objEqTest)
    loop [] lvalOrder
