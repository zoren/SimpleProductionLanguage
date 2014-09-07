namespace SimpleProductionLanguage

open System.Drawing
open System.Windows.Forms
module M =
    let form = new Form(Visible = true, Text = "A Simple F# Form",
                        TopMost = true, Size = Size(600,600))
 
    let drawWhitePix (form:Form) x y =
        let aBrush = Brushes.White
        let g = form.CreateGraphics()
        g.FillRectangle(aBrush, x, y, 1, 1)

    let drawcircle (form:Form) radius cx cy =
        let coords = seq {for x in 0 .. form.Size.Width do
                            for y in 0 .. form.Size.Height -> (x, y) }
        let radiusSquared = radius * radius
        let dist (x,y) =
            let xd = cx - x
            let yd = cy - y
            xd * xd + yd * yd < radiusSquared
        Seq.iter (fun (x,y) -> if dist(x,y) then drawWhitePix form x y ) coords

