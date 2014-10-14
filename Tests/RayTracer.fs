namespace Tests

module RayTracer =
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

  let trace (width, height) (circleX, circleY) radius =
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
    let interp = createSPLInterp s

    let setupId = interp.create "Setup"
    interp.assign setupId "x" (width-1)
    interp.assign setupId "y" (height-1)

    let colorid = interp.create "Color"

    let circleid = interp.create "Circle"
    interp.assign circleid "x" circleX
    interp.assign circleid "y" circleY
    interp.assign circleid "radius" radius
    interp.assign circleid "color" colorid

    let pixels = interp.Interpreter.GetInstancesOfType("Pixel")

    let intValue (Int i) = i

    let findAssignments instId =
      let map = Map.ofSeq <| interp.Interpreter.GetAssignments instId
      intValue <| Map.find "x" map, intValue <| Map.find "y" map
    Seq.map findAssignments pixels
