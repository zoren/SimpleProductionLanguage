namespace SPLRete

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
    let rec loop i (wmeST::st) =
      match Map.tryFind lvalue wmeST with
      | Some fieldIndex -> {tokenIndex = i; fieldIndex = fieldIndex }
      | None -> loop (i+1) st
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

  let FieldIndexAssignmentValue = 3
  let FieldIndexInstance = 1
  let FieldIndexPartOfChild = 1
  let FieldIndexPartOfParent = 2

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

  let mkObjEqTest var (env:TestEnvironment) = mkGenericObjEqTest FieldIndexInstance var env

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
            if Set.isSubset lvalsInCond abstractedLVals then
              match cond with
              | PartOf(childLVal, parentLVal) ->
                let partOfPattern = mkPartOfPattern()
                let newSTPartOf = Map.empty :: newST
                let childVar = lookupSymbolTable newSTPartOf childLVal
                let parentVar = lookupSymbolTable newSTPartOf parentLVal
                let partOfObjEqTests = forAllPreds
                                              [mkGenericObjEqTest FieldIndexPartOfChild childVar
                                               mkGenericObjEqTest FieldIndexPartOfParent parentVar]
                let partOfNode = mkSingleChildPatternNode partOfPattern (buildTest partOfObjEqTests) (loopLVals newSTPartOf)
                None, partOfNode
              | Comparison(el, compOp, er) ->
                let cel = compExp newST el
                let cer = compExp newST er
                let cf = compOpToFunc compOp
                let compTest =
                  fun testEnv ->
                    let vl = getInt << cel <| lookupTestEnv testEnv
                    let vr = getInt << cer <| lookupTestEnv testEnv
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
          let newST = (Map.ofList [lval,FieldIndexInstance]) :: st
          buildTreeFromCondition pattern newST None
        | Proj(lval', cstic) ->
          let pattern = mkAssignPattern cstic
          let var = lookupSymbolTable st lval'
          let newST = (Map.ofList[lval',FieldIndexInstance;lval, FieldIndexAssignmentValue]) :: st
          let objEqTest = mkObjEqTest {var with tokenIndex = var.tokenIndex + 1}
          buildTreeFromCondition pattern newST (Some objEqTest)
    loop [] lvalOrder
