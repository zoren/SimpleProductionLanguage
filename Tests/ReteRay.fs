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
  open SimpleProductionLanguage.Parser
  open PatternMatching.ReteNetwork
  open PatternMatching.ReteBuilder
  open PatternMatching.PatternTree
  open PatternMatching.ReteInterpreter
  
  let parseRules s = runP SimpleProductionLanguage.Parser.rules s

  [<Test>]
  let testTracing()=
    let s = @"
\ s:Setup ->
true ?
find_or_create Ray(x := 0, y := 0)

\ s:Setup, r:Ray ->
r.x < s.x ?
find_or_create Ray(x := r.x + 1, y := r.y)

\ s:Setup, r:Ray ->
r.y < s.y ?
find_or_create Ray(x := r.x, y := r.y + 1)

// draw circle
\ c:Circle, r:Ray ->
(r.x - c.x) * (r.x - c.x) +
(r.y - c.y) * (r.y - c.y) < c.radius ? 
find_or_create Pixel(x := r.x, y := r.y, color := c.color)
"
    let rules = parseRules s
    let pTrees = Array.ofSeq <| Seq.map ruleToPTree rules
    let rete = reteGraphFromPatternTrees pTrees
    let currentInstId = ref 0
    let interp = new Interpreter(rete)
    let create className =
      let instId = !currentInstId
      currentInstId := !currentInstId + 1
      let fact = "class", [| Int instId; String className |]
      ignore <| interp.Add fact
      instId
      
    let assign instId cstic value =
      let fact = "assign", [| Int instId; String cstic; Int value |]
      ignore <| interp.Add fact

    let setupId = create "Setup"
    assign setupId "x" 3
    assign setupId "y" 3

    let colorId = create "Color"

    let circleId = create "Circle"
    assign circleId "x" 1
    assign circleId "y" 1
    assign circleId "radius" 1
    assign circleId "color" colorId

    let out = interp.GetFacts()

    let pixels = interp.GetInstancesOfType("Pixel")
    test <@ not <| Seq.isEmpty pixels @>

    let findAssignments instId =
        Map.ofSeq <| Seq.choose 
            (function
            | "assign",[|Int instId'; String field'; value'|] when instId = instId' -> Some (field', value')
            | _ -> None) out
    printf "%A" <| Seq.map findAssignments pixels

    ()

