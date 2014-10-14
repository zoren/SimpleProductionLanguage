open System.Drawing
open System.Windows.Forms
module DrawUtil =
  let createForm width height =
    let form = new Form(Visible = true, Text = "A Simple F# Form",
                        TopMost = true, Size = Size(width, height))
    form.BackColor <- Color.Black
    form

  let drawWhitePix (form:Form) x y =
    let aBrush = Brushes.White
    let g = form.CreateGraphics()
    g.FillRectangle(aBrush, x, y, 1, 1)

let form = DrawUtil.createForm 100 100

let setPix (x, y) = DrawUtil.drawWhitePix form x y

#r @"C:\Users\ss\git\SimpleProductionLanguage\Tests\bin\Debug\Tests.dll"

open Tests.RayTracer

Seq.iter setPix <| trace (20, 20) (10, 10) 40
