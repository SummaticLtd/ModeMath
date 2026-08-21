module ModeMath.Tests.PainterTests

open System.Collections.Immutable
open System.Drawing
open FSUtils
open SimpleTests
open SkiaSharp
open ModeMath

let private size = 60f<px>
let private margin = 2f<px>
let private row(elements: MA list) = MA.Row(elements.ToImmutableArray())
let private c(character: char) = MA.Char character

/// Every pixel a formula is drawn onto, in black on white, with the atom it was drawn from.
let private drawn(ma: MA) =
    let placed = (Layout size).Of ma
    let whole(length: float32<px>) = int (ceil (Measure.removeFloat32Unit<px> length)) + 4
    use painter = Painter.Embedded()
    use bitmap = new SKBitmap(whole placed.Width, whole placed.Height)
    use canvas = new SKCanvas(bitmap)
    canvas.Clear SKColors.White
    use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
    painter.Draw(placed, canvas, margin, margin + placed.Ascent, paint)
    placed, Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

/// Ink in a red of its own, which antialiasing lightens but leaves the reddest of the three.
let private red(pixel: SKColor) =
    int pixel.Red > int pixel.Green + 60 && int pixel.Red > int pixel.Blue + 60

/// Ink in the black paint the formula was given, which antialiasing lightens without tinting.
let private black(pixel: SKColor) =
    let highest = max (max pixel.Red pixel.Green) pixel.Blue
    let lowest = min (min pixel.Red pixel.Green) pixel.Blue
    int highest < 120 && int highest - int lowest < 60

/// The rightmost column holding ink of a kind, or -1 where none was drawn.
let private lastColumn(kind: SKColor -> bool, pixels: SKColor[,]) =
    let mutable last = -1
    for x in 0 .. Array2D.length1 pixels - 1 do
        for y in 0 .. Array2D.length2 pixels - 1 do
            if kind pixels.[x, y] then last <- max last x
    last

let private painting =
    TestList(
        "Painting",
        [   Test.Sync(
                "aFormulaIsDrawnInThePaintItIsGiven",
                fun () ->
                    let _, pixels = drawn(c 'x')
                    Assert.True(lastColumn(black, pixels) >= 0, "nothing was drawn in the black paint")
                    Assert.True(lastColumn(red, pixels) < 0, "something was drawn in a colour")
            )
            Test.Sync(
                "aColourStopsAtTheEndOfTheAtomItWasGivenTo",
                fun () ->
                    let placed, pixels = drawn(row [ MA.Coloured(Color.Red, c 'b'); c 'c' ])
                    let after =
                        match placed.Pma with
                        | PlacedMA.Row children -> children.[1].X
                        | other -> failwith $"not a row: {other}"
                    let last = lastColumn(red, pixels)
                    Assert.True(last >= 0, "the coloured atom was not drawn in its colour")
                    Assert.True(
                        float32 last < Measure.removeFloat32Unit<px>(margin + after),
                        "the colour ran on past the atom it was given to")
                    Assert.True(lastColumn(black, pixels) >= 0, "the atom after it lost the black paint")
            )
            Test.Sync(
                "aColourReachesTheRulesAnAtomDrawsAndNotOnlyItsGlyphs",
                fun () ->
                    let _, pixels = drawn(MA.Coloured(Color.Red, MA.Frac(c 'a', c 'b')))
                    Assert.True(lastColumn(red, pixels) >= 0, "nothing was drawn in the colour")
                    Assert.True(lastColumn(black, pixels) < 0, "the fraction rule kept the black paint")
            )
        ]
    )

let tests = TestFolder("Painter", [ painting ])
