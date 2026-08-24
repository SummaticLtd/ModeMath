namespace ModeMath

open System
open System.Collections.Generic
open System.Drawing
open SkiaSharp
open FSUtils

/// Draws a Placed onto an SKCanvas, whose y grows downwards where a Placed's grows upwards. One draw at a time.
type Painter(math: SKTypeface, blackboard: SKTypeface) =
    let fonts = Dictionary<struct(Face * float32<px>), SKFont>()
    let blobs = Dictionary<struct(Face * float32<px> * int), SKTextBlob | null>()

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

    /// The blob one glyph goes down as, built at the origin so a draw anywhere is the same pixels.
    let blob(glyph: Glyph, size: float32<px>) =
        let key = struct(glyph.Face, size, glyph.Id)
        match blobs |> Dictionary.tryFind key with
        | ValueSome found -> found
        | ValueNone ->
            let id = BitConverter.GetBytes(uint16 glyph.Id)
            let created =
                SKTextBlob.Create(
                    ReadOnlySpan<byte> id,
                    SKTextEncoding.GlyphId,
                    font(glyph.Face, size),
                    SKPoint.Empty)
            blobs.[key] <- created
            created

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
        t.Draw(curs, canvas, x, baseline, paint, paint)

    /// The same, with the cursor drawn in a paint of its own.
    member t.Draw
        (
            curs: PlacedCurs,
            canvas: SKCanvas,
            x: float32<px>,
            baseline: float32<px>,
            paint: SKPaint,
            cursor: SKPaint
        ) =
        t.Draw(curs.Placed, canvas, x, baseline, paint)
        let caret = curs.Caret
        canvas.DrawRect(
            SKRect.Create(
                number(x + caret.X),
                number(baseline - caret.Y - caret.Thickness),
                number caret.Width,
                number caret.Thickness),
            cursor)

    /// A colour in the formula is set on the paints and put back, so nothing may use them meanwhile.
    member t.Draw
        (
            placed: Placed,
            canvas: SKCanvas,
            x: float32<px>,
            baseline: float32<px>,
            solid: SKPaint,
            tentative: SKPaint
        ) =
        let paint(ink: Ink) = if ink = Ink.Tentative then tentative else solid
        for part in placed.Parts do
            match part with
            | Part.Glyph(glyph, ink) ->
                canvas.DrawText(
                    blob(glyph.Glyph, glyph.Size),
                    number(x + glyph.X),
                    number(baseline - glyph.Y),
                    paint ink)
            | Part.Rule(rule, ink) ->
                canvas.DrawRect(
                    SKRect.Create(
                        number(x + rule.X),
                        number(baseline - rule.Y - rule.Thickness),
                        number rule.Width,
                        number rule.Thickness),
                    paint ink)
            | Part.Child child ->
                t.Draw(child, canvas, x + child.X, baseline - child.Y, solid, tentative)
            | Part.Painted(colour, child) ->
                // The paints belong to the caller, so the colours go back once the child is drawn.
                let wasSolid = solid.Color
                let wasTentative = tentative.Color
                let painted = SKColor(colour.R, colour.G, colour.B, colour.A)
                solid.Color <- painted
                tentative.Color <- painted.WithAlpha(byte (int colour.A / 3))
                t.Draw(child, canvas, x + child.X, baseline - child.Y, solid, tentative)
                solid.Color <- wasSolid
                tentative.Color <- wasTentative

    interface IDisposable with
        member _.Dispose() =
            for blob in blobs.Values do
                match blob with
                | NonNull blob -> blob.Dispose()
                | Null -> ()
            blobs.Clear()
            for font in fonts.Values do
                font.Dispose()
            fonts.Clear()
            math.Dispose()
            blackboard.Dispose()
