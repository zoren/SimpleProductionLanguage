namespace Tests

open NUnit.Framework

[<TestFixture>]
module ReteRay =
  open Swensen.Unquote
  open FParsec.CharParsers

  let runP p s =
    match run p s with
    | Success(result, _, pos) ->
      if pos.Index <> int64 s.Length
      then failwith "incomplete parse"
      result
    | Failure(msg,_,_) -> printfn "%s" msg; raise <| System.NotImplementedException()

  open SPLRete.SPLToRete
  open SPLRete.Interpreter
  open SPLRete.SPLInterpreter
  open SimpleProductionLanguage.Parser
  open PatternMatching.ReteNetwork
  open PatternMatching.ReteBuilder
  open PatternMatching.PatternTree
  open PatternMatching.ReteInterpreter

  let parseRules s = runP SimpleProductionLanguage.Parser.rules s

  let rulesToRete s =
    let rules = parseRules s
    let pTrees = Array.ofSeq <| Seq.map ruleToPTree rules
    reteGraphFromPatternTrees pTrees

  let createSPLInterp s = new SPLInterpreter(parseRules s)

  [<Test>]
  let testFind()=
    let s = @"
\ a: A ->
true ?
find_or_create B(b := a.a)

\ b: B ->
true ?
find_or_create C(c := b.b + 1)
"
    let interp = createSPLInterp s

    let aId = interp.create "A"
    interp.assign aId "a" 0
    let cs = interp.Interpreter.GetInstancesOfType("C")
    test <@ not <| Seq.isEmpty cs @>
    let assignments = interp.Interpreter.GetAssignments <| Seq.exactlyOne cs
    test <@ Seq.toArray assignments = [|"c", Int 1|] @>

  [<Test>]
  let testList()=
    let s = @"
\ s: Setup ->
true ?
find_or_create Element(n := 0)

\ s : Setup, e : Element ->
e.n < s.n ?
find_or_create Element(n := e.n + 1)
"
    let interp = createSPLInterp s

    let setup = interp.create "Setup"
    interp.assign setup "n" 5
    let cs = interp.Interpreter.GetInstancesOfType("Element")
    test <@ Seq.length cs = 6 @>

  let mkClassPat className = "class",[|Anything IntType; PatternValue <| String className|]
  let mkCsticPat cstic = "assign",[|Anything IntType; PatternValue <| String cstic; Anything IntType|]
  let mkPatternNode pat ptree = PatternNode(pat, [| ptree |])
  let mkTestNodes tests ptree = Seq.fold (fun pt t -> TestNode(t, pt)) ptree tests

  [<Test>]
  let testPat()=
    let s = @"
\ s : Setup, e : Element ->
e.x < s.n ?
find_or_create Result()
"
    let rule = Seq.exactlyOne <| parseRules s
    let ptree = ruleToPTree rule
    let prod = SimpleProductionLanguage.AST.FindOrCreate("Result", [])
    let prodNode = Production ([SimpleProductionLanguage.AST.Proj (SimpleProductionLanguage.AST.Variable "s","n");SimpleProductionLanguage.AST.Proj (SimpleProductionLanguage.AST.Variable "e","x"); SimpleProductionLanguage.AST.Variable "e";SimpleProductionLanguage.AST.Variable "s"], prod)
    let expectedPTree =
      mkPatternNode(mkClassPat "Setup") <<
        mkPatternNode (mkClassPat "Element") <<
          mkPatternNode (mkCsticPat "x") << // e.x
            mkTestNodes[Comparison(Variable(0,0), Eq, Variable (1,0))] <<
              mkPatternNode (mkCsticPat "n") << // s.n
                mkTestNodes[Comparison(Variable(0,0), Eq, Variable (3,0))] <|
                  (mkTestNodes [Comparison(Variable(1,2), Lt, Variable (0,2))] prodNode)
    let reteGraph = reteGraphFromPatternTrees [| expectedPTree |]
    let interp = new Interpreter(reteGraph)
    interp.Add ("class", [| Int 0; String "Setup"|])
    interp.Add ("assign", [| Int 0; String "n"; Int 1|])
    interp.Add ("class", [| Int 1; String "Element"|])
    interp.Add ("assign", [| Int 1; String "x"; Int 0|])
    let instances = interp.GetInstancesOfType "Result"
    test <@ Seq.length instances = 1 @>
    test <@ expectedPTree = ptree @>
    printf "\n\nexpected:\n%A\n" expectedPTree
    printf "actual:\n%A" ptree

  [<Test>]
  let testTracingManualRayCreation()=
    let s = @"
\ c:Circle, r:Ray ->
(r.x - c.x) * (r.x - c.x) +
(r.y - c.y) * (r.y - c.y) < c.radius ?
find_or_create Pixel(x := r.x, y := r.y, color := c.color)
"
    let interp = createSPLInterp s

    let rayId = interp.create "Ray"
    interp.assign rayId "x" 1
    interp.assign rayId "y" 1

    let colorid = interp.create "Color"

    let circleid = interp.create "Circle"
    interp.assign circleid "x" 1
    interp.assign circleid "y" 1
    interp.assign circleid "radius" 2
    interp.assign circleid "color" colorid

    let pixels = Array.ofSeq <| interp.Interpreter.GetInstancesOfType("Pixel")
    test <@ not <| Seq.isEmpty pixels @>

  open RayTracer

  [<Test>]
  let testTracing()=
    let pixels = trace (10, 10) (5,5) 1
    test <@ Seq.length pixels = 1@>
