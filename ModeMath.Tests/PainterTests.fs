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

/// Every pixel a formula and the cursor in it are drawn onto, sized to hold both.
let private drawnWithCursor(curs: MACurs) =
    let placed = (Layout size).Of curs
    let bounds = placed.Bounds
    let whole(length: float32<px>) = int (ceil (Measure.removeFloat32Unit<px> length)) + 4
    use painter = Painter.Embedded()
    use bitmap = new SKBitmap(whole bounds.Width, whole bounds.Thickness)
    use canvas = new SKCanvas(bitmap)
    canvas.Clear SKColors.White
    use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
    painter.Draw(placed, canvas, margin - bounds.X, margin + bounds.Y + bounds.Thickness, paint)
    placed, Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

/// Every pixel a painter draws a formula onto, moved along the line by a fraction of one.
let private drawnBy(painter: Painter, ma: MA, offset: float32<px>) =
    let placed = (Layout size).Of ma
    let whole(length: float32<px>) = int (ceil (Measure.removeFloat32Unit<px> length)) + 4
    use bitmap = new SKBitmap(whole placed.Width, whole placed.Height)
    use canvas = new SKCanvas(bitmap)
    canvas.Clear SKColors.White
    use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
    painter.Draw(placed, canvas, margin + offset, margin + placed.Ascent, paint)
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

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
                "aCursorIsDrawnWhereItSaysItIs",
                fun () ->
                    // At the end of the formula, so that only the cursor can be inking the column.
                    let formula = (MA.String "abc").Flatten
                    let placed, withCursor = drawnWithCursor(MACurs.AtEnd formula)
                    let _, without = drawn formula
                    let column = int (Measure.removeFloat32Unit<px>(margin - placed.Bounds.X + placed.Caret.X))
                    let inked(pixels: SKColor[,]) =
                        seq { 0 .. Array2D.length2 pixels - 1 }
                        |> Seq.filter (fun y -> black pixels.[column, y])
                        |> Seq.length
                    let bar = int (Measure.removeFloat32Unit<px> placed.Caret.Thickness)
                    Assert.True(
                        inked withCursor >= bar - 1,
                        $"the caret column held {inked withCursor} of the {bar} pixels the bar is tall")
                    // The last letter reaches into that column too, so a gain is what proves the bar.
                    Assert.True(
                        inked withCursor > inked without,
                        $"the cursor added nothing: {inked without} then {inked withCursor}")
            )
            Test.Sync(
                "aCursorTakesAPaintOfItsOwnWhereItIsGivenOne",
                fun () ->
                    let curs = (Layout size).Of(MACurs.AtEnd((MA.String "ab").Flatten))
                    let bounds = curs.Bounds
                    let whole(length: float32<px>) = int (ceil (Measure.removeFloat32Unit<px> length)) + 4
                    use painter = Painter.Embedded()
                    use bitmap = new SKBitmap(whole bounds.Width, whole bounds.Thickness)
                    use canvas = new SKCanvas(bitmap)
                    canvas.Clear SKColors.White
                    use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
                    use cursor = new SKPaint(Color = SKColors.Red, IsAntialias = true)
                    painter.Draw(
                        curs, canvas, margin - bounds.X, margin + bounds.Y + bounds.Thickness, paint, cursor)
                    let pixels = Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))
                    Assert.True(lastColumn(red, pixels) >= 0, "the cursor was not drawn in the paint it was given")
                    Assert.True(lastColumn(black, pixels) >= 0, "the formula went into the cursor's paint too")
                    Assert.True(
                        lastColumn(black, pixels) < lastColumn(red, pixels),
                        "the formula reached past the cursor at the end of it")
            )
            Test.Sync(
                "aFormulaWithoutACursorDrawsNoneOfIt",
                fun () ->
                    let formula = (MA.String "abc").Flatten
                    let curs = MACurs.Positions formula |> Seq.item 2
                    let placed, withCursor = drawnWithCursor curs
                    let _, without = drawn formula
                    let ink(pixels: SKColor[,]) =
                        seq {
                            for x in 0 .. Array2D.length1 pixels - 1 do
                                for y in 0 .. Array2D.length2 pixels - 1 do
                                    yield pixels.[x, y]
                        }
                        |> Seq.filter black
                        |> Seq.length
                    Assert.True(
                        ink withCursor > ink without,
                        $"the cursor added no ink: {ink withCursor} against {ink without}")
                    Assert.True(placed.Caret.Width > 0f<px>, "the cursor had no width to draw")
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
                "aGlyphIsDrawnTheSameHoweverManyTimesItHasBeenDrawnBefore",
                fun () ->
                    use painter = Painter.Embedded()
                    use fresh = Painter.Embedded()
                    let formula = (MA.String "abc").Flatten
                    let first = drawnBy(painter, formula, 0f<px>)
                    let again = drawnBy(painter, formula, 0f<px>)
                    let cold = drawnBy(fresh, formula, 0f<px>)
                    Assert.True((again = first), "the second drawing differed from the first")
                    Assert.True((cold = first), "a painter that had drawn before differed from one that had not")
            )
            Test.Sync(
                "aPainterDrawnAtEverMoreSizesDropsTheBlobsItKeptRatherThanHoldingThemAll",
                fun () ->
                    use painter = Painter.Embedded()
                    use bitmap = new SKBitmap(200, 200)
                    use canvas = new SKCanvas(bitmap)
                    use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
                    let drawAt(em: float32<px>) =
                        let placed = (Layout em).Of(c 'x')
                        painter.Draw(placed, canvas, margin, margin + placed.Ascent, paint)
                    let mutable highest = 0
                    for i in 1 .. 6000 do
                        drawAt(float32 i * 0.01f<px>)
                        highest <- max highest painter.KeptBlobs
                    Assert.True(highest > 0, "no blob was ever kept")
                    Assert.True(
                        painter.KeptBlobs < highest,
                        $"every one of the {highest} blobs was still kept after 6,000 sizes")
                    // The formula still draws once the kept blobs have gone.
                    let after = drawnBy(painter, (MA.String "abc").Flatten, 0f<px>)
                    use fresh = Painter.Embedded()
                    Assert.True(
                        (after = drawnBy(fresh, (MA.String "abc").Flatten, 0f<px>)),
                        "a painter that had dropped its blobs drew differently")
            )
            Test.Sync(
                "aFormulaMovedHalfAPixelAlongIsDrawnOnDifferentPixels",
                fun () ->
                    // Glyphs are positioned to the subpixel, which drawing from a kept blob must keep.
                    use painter = Painter.Embedded()
                    let formula = (MA.String "abc").Flatten
                    let atWhole = drawnBy(painter, formula, 0f<px>)
                    let atHalf = drawnBy(painter, formula, 0.5f<px>)
                    Assert.True(atHalf <> atWhole, "half a pixel over gave the very same pixels")
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
