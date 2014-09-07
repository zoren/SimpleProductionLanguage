namespace Tests

open NUnit.Framework

[<TestFixture>]
module InterpreterTests =

    open Swensen.Unquote
    open FParsec.CharParsers
    let runP p s = 
        match run p s with
        | Success(result, _, _) -> result
        | Failure(msg,_,_) -> printfn "%s" msg; raise <| System.NotImplementedException()
    
    
    open SimpleProductionLanguage.AST
    let parseRules s = runP SimpleProductionLanguage.Parser.rules s

    open SimpleProductionLanguage.Interpreter

    [<Test>]
    let testFindOrCreate()=
        let s = @"\ s:Setup -> true? find_or_create Ray(x := 0, y := 0)"
        let rules = parseRules s
        let interp = new Interpreter(rules)
        interp.Add(Instance(0, "Setup"))
        let out = interp.GetFacts()
        test <@ Set.contains (Instance(1,"Ray")) out @>

    [<Test>]
    let testFixPoint()=
        let s = @"
\ s:Setup ->
	true ?
		find_or_create Ray(x := 0, y := 0)

\ s:Setup, r:Ray ->
	r.x < s.x ?
		find_or_create Ray(x := r.x + 1, y := r.y)"
        let rules = parseRules s
        let interp = new Interpreter(rules)
        interp.Add(Instance(0, "Setup"))
        interp.Add(Assignment(0, "x", Int 4))
        let out = interp.GetFacts()

        test <@ 5 = (Seq.length <| interp.GetInstancesOfType "Ray") @>


    [<Test>]
    let testFixPoint2()=
        let s = @"
\ s:Setup ->
	true ?
		find_or_create Ray(x := 0, y := 0)

\ s:Setup, r:Ray ->
	r.x < s.x ?
		find_or_create Ray(x := r.x + 1, y := r.y)

\ s:Setup, r:Ray ->
	r.y < s.y ?
		find_or_create Ray(x := r.x, y := r.y + 1)"
        let rules = parseRules s
        let interp = new Interpreter(rules)
        interp.Add(Instance(0, "Setup"))
        interp.Add(Assignment(0, "x", Int 3))
        interp.Add(Assignment(0, "y", Int 3))
        let out = interp.GetFacts()
        for fact in out do
            printfn "%A" fact
        test <@ 16 = (Seq.length <| interp.GetInstancesOfType "Ray") @>


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
		find_or_create Pixel(x := r.x, y := r.y, color := c.color)"
        let rules = parseRules s
        let interp = new Interpreter(rules)

        interp.Add(Instance(0, "Setup"))
        interp.Add(Assignment(0, "x", Int 3))
        interp.Add(Assignment(0, "y", Int 3))
        
        let colorId = interp.GetFreshInstanceId()
        interp.Add(Instance(colorId, "Color"))
        
        let circleId = interp.GetFreshInstanceId()
        interp.Add(Instance(circleId, "Circle"))
        interp.Add(Assignment(circleId, "x", Int 1))
        interp.Add(Assignment(circleId, "y", Int 1))
        interp.Add(Assignment(circleId, "radius", Int 1))
        interp.Add(Assignment(circleId, "color", InstanceRef colorId))
                
        let out = interp.GetFacts()
        let pixels = interp.GetInstancesOfType("Pixel")
        test <@ not <| Seq.isEmpty pixels @>

        let findAssignments instId =
            Map.ofSeq <| Seq.choose 
                (function
                | Assignment(instId', field', value') when instId = instId' -> Some (field', value')
                | _ -> None) out
        printf "%A" <| Seq.map findAssignments pixels

    [<Test>]
    let testRemove()=
        let s = @"
\ s:Setup ->
	true ?
		find_or_create Ray(x := 0, y := 0)

\ s:Setup, r:Ray ->
	r.x < s.x ?
		find_or_create Ray(x := r.x + 1, y := r.y)

\ s:Setup, r:Ray ->
	r.y < s.y ?
		find_or_create Ray(x := r.x, y := r.y + 1)"
        let rules = parseRules s
        let interp = new Interpreter(rules)
        interp.Add(Instance(0, "Setup"))
        interp.Add(Assignment(0, "x", Int 3))
        interp.Add(Assignment(0, "y", Int 3))
        let out = interp.GetFacts()
        for fact in out do
            printfn "%A" fact
        test <@ 16 = (Seq.length <| interp.GetInstancesOfType "Ray") @>
        interp.Remove(Instance(0, "Setup"))
        test <@ 0 = (Seq.length <| interp.GetInstancesOfType "Ray") @>

    [<Test>]
    let testModify()=
        let s = @"
\ s:Setup ->
	true ?
		find_or_create Ray(x := 0, y := 0)

\ s:Setup, r:Ray ->
	r.x < s.x ?
		find_or_create Ray(x := r.x + 1, y := r.y)

\ s:Setup, r:Ray ->
	r.y < s.y ?
		find_or_create Ray(x := r.x, y := r.y + 1)"
        let rules = parseRules s
        let interp = new Interpreter(rules)
        interp.Add(Instance(0, "Setup"))
        interp.Add(Assignment(0, "x", Int 3))
        interp.Add(Assignment(0, "y", Int 3))
        let out = interp.GetFacts()
//        for fact in out do
//            printfn "%A" fact
        test <@ 16 = (Seq.length <| interp.GetInstancesOfType "Ray") @>
        let added, removed = interp.Modify(Assignment(0, "y", Int 3), Assignment(0, "y", Int 2))
        test <@ Set.singleton (Assignment(0,"y", Int 2)) = added @>
        test <@ Set.contains (Assignment(0,"y", Int 3))  removed @>
        let isInstanceFact =
            function
            | Instance _ -> true
            | _ -> false
        let removedInstance = Set.filter isInstanceFact removed
        test <@ Set.count removedInstance = 4 @>
        test <@ 12 = (Seq.length <| interp.GetInstancesOfType "Ray") @>
