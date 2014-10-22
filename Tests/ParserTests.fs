namespace Tests

open NUnit.Framework

[<TestFixture>]
module C =
    open Swensen.Unquote
    open FParsec.CharParsers
    let runP p s =
        match run p s with
        | Success(result, _, _) -> result
        | Failure(msg,_,_) -> printfn "%s" msg; raise <| System.NotImplementedException()


    open SimpleProductionLanguage.AST
    let parseRules s = runP SimpleProductionLanguage.Parser.rules s

    [<Test>]
    let t() =
        let s = @"\ s:Setup -> true?
	find_or_create Ray(x := 0, y := 0)"
        let [ast] = parseRules s
        test <@ ast = (["s","Setup"], True, FindOrCreate("Ray",["x",Constant 0;"y",Constant 0])) @>

    [<Test>]
    let t2() =
        let s = @"// draw circle
\ c:Circle, r:Ray ->
	a + b * c < d ?
		find_or_create pixel(x := 5)"
        let [ast] = parseRules s
        test <@ ast = (["c", "Circle"; "r", "Ray"],
                 Comparison
                   (BinOp
                      (Deref (Variable "a"),Plus,
                       BinOp (Deref (Variable "b"),Times,Deref (Variable "c"))), LT,
                    Deref (Variable "d")), FindOrCreate ("pixel",[("x", Constant 5)])) @>

    [<Test>]
    let testPartOf () =
      let s = @"\ r:Root, p:Part ->
  part_of(p, r) ?
    find_or_create FoundInstance()"
      let [ast] = parseRules s
      test <@ ast = (["r", "Root";"p", "Part"],
                PartOf(Deref(Variable "p"), Deref(Variable "r")),
                  FindOrCreate ("FoundInstance",[])) @>

    [<Test>]
    let parseFile() =
        let s = System.IO.File.ReadAllText @"2dRayTraceRules.txt"
        test <@ List.length <| parseRules s = 4 @>
