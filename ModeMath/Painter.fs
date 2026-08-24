namespace ModeMath

open System
open System.Collections.Generic
open System.Drawing
open SkiaSharp
open FSUtils

/// Draws a Placed onto an SKCanvas, whose y grows downwards where a Placed's grows upwards. One draw at a time.
type Painter(math: SKTypeface, blackboard: SKTypeface) =
    let fonts = Dictionary<struct(Face * float32<px>), SKFont>()
    /// The glyphs waiting to go onto the canvas together, under the font each is set in.
    let waiting = Dictionary<SKFont, ResizeArray<struct(uint16 * SKPoint)>>()
    /// Which paint the glyphs waiting are to be drawn in, since one blob is drawn in the one paint.
    let mutable pendingInk = Ink.Solid

    /// SkiaSharp takes the numbers themselves, so the measure comes off here and nowhere else.
    let number(value: float32<px>) = Measure.removeFloat32Unit<px> value

    let typeface(face: Face) =
        match face with
        | Face.Math -> math
        | Face.Blackboard -> blackboard

    let font(face: Face, size: float32<px>) =
        let key = struct(face, size)
        match fonts |> Dictionary.tryFind key with
        | ValueSome found -> found
        | ValueNone ->
            let created = new SKFont(typeface face, number size)
            created.Hinting <- SKFontHinting.None
            created.Subpixel <- true
            created.Edging <- SKFontEdging.SubpixelAntialias
            fonts.[key] <- created
            created

    let glyphsIn(font: SKFont) =
        match waiting |> Dictionary.tryFind font with
        | ValueSome found -> found
        | ValueNone ->
            let created = ResizeArray<struct(uint16 * SKPoint)>()
            waiting.[font] <- created
            created

    /// Every glyph waiting drawn as one blob, which is one run for each font they are set in.
    let flush(canvas: SKCanvas, paint: SKPaint) =
        let mutable any = false
        for run in waiting.Values do
            if run.Count > 0 then any <- true
        if any then
            use builder = new SKTextBlobBuilder()
            for entry in waiting do
                let glyphs = entry.Value
                if glyphs.Count > 0 then
                    let run = builder.AllocatePositionedRun(entry.Key, glyphs.Count)
                    let ids = run.Glyphs
                    let points = run.Positions
                    for i in 0 .. glyphs.Count - 1 do
                        let struct(id, point) = glyphs.[i]
                        ids.[i] <- id
                        points.[i] <- point
                    glyphs.Clear()
            use blob = builder.Build()
            canvas.DrawText(blob, 0f, 0f, paint)

    /// A painter over the files the metrics were generated from.
    static member Embedded() =
        let opened(face: Face) =
            use stream = MathFont.OpenFontFile face
            SKTypeface.FromStream stream
        new Painter(opened Face.Math, opened Face.Blackboard)

    /// Draws what is only offered at a third of the given paint's opacity.
    member t.Draw(placed: Placed, canvas: SKCanvas, x: float32<px>, baseline: float32<px>, paint: SKPaint) =
        use tentative = paint.Clone()
        tentative.Color <- paint.Color.WithAlpha(byte (int paint.Color.Alpha / 3))
        t.Draw(placed, canvas, x, baseline, paint, tentative)

    /// Draws a formula with its cursor, which is drawn over the formula rather than among it.
    member t.Draw(curs: PlacedCurs, canvas: SKCanvas, x: float32<px>, baseline: float32<px>, paint: SKPaint) =
        t.Draw(curs.Placed, canvas, x, baseline, paint)
        let caret = curs.Caret
        canvas.DrawRect(
            SKRect.Create(
                number(x + caret.X),
                number(baseline - caret.Y - caret.Thickness),
                number caret.Width,
                number caret.Thickness),
            paint)

    member t.Draw
        (
            placed: Placed,
            canvas: SKCanvas,
            x: float32<px>,
            baseline: float32<px>,
            solid: SKPaint,
            tentative: SKPaint
        ) =
        t.Walk(placed, canvas, x, baseline, solid, tentative)
        flush(canvas, (if pendingInk = Ink.Tentative then tentative else solid))

    /// Everything an atom draws, with the glyphs left waiting for whoever asked to put them down.
    member private t.Walk
        (
            placed: Placed,
            canvas: SKCanvas,
            x: float32<px>,
            baseline: float32<px>,
            solid: SKPaint,
            tentative: SKPaint
        ) =
        let paint(ink: Ink) = if ink = Ink.Tentative then tentative else solid
        // Glyphs go down in the paint that was standing when they were met, so a change puts them down.
        let changing(ink: Ink) = if ink <> pendingInk then flush(canvas, paint pendingInk)
        for part in placed.Parts do
            match part with
            | Part.Glyph(glyph, ink) ->
                changing ink
                pendingInk <- ink
                glyphsIn(font(glyph.Glyph.Face, glyph.Size))
                    .Add(struct(uint16 glyph.Glyph.Id, SKPoint(number(x + glyph.X), number(baseline - glyph.Y))))
            | Part.Rule(rule, ink) ->
                // Subpixel antialiasing blends with what is under it, so a rule waits for the glyphs.
                flush(canvas, paint pendingInk)
                canvas.DrawRect(
                    SKRect.Create(
                        number(x + rule.X),
                        number(baseline - rule.Y - rule.Thickness),
                        number rule.Width,
                        number rule.Thickness),
                    paint ink)
            | Part.Child child ->
                t.Walk(child, canvas, x + child.X, baseline - child.Y, solid, tentative)
            | Part.Painted(colour, child) ->
                // The paints belong to the caller, so the colours go back once the child is drawn.
                flush(canvas, paint pendingInk)
                let wasSolid = solid.Color
                let wasTentative = tentative.Color
                let painted = SKColor(colour.R, colour.G, colour.B, colour.A)
                solid.Color <- painted
                tentative.Color <- painted.WithAlpha(byte (int colour.A / 3))
                t.Walk(child, canvas, x + child.X, baseline - child.Y, solid, tentative)
                flush(canvas, paint pendingInk)
                solid.Color <- wasSolid
                tentative.Color <- wasTentative

    interface IDisposable with
        member _.Dispose() =
            for font in fonts.Values do
                font.Dispose()
            fonts.Clear()
            math.Dispose()
            blackboard.Dispose()
