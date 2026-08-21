namespace ModeMath

open System
open System.Collections.Generic
open SkiaSharp
open FSUtils

/// Draws a Display onto an SKCanvas, whose y grows downwards where a Display's grows upwards.
type Painter(typeface: SKTypeface) =
    let fonts = Dictionary<float32, SKFont>()

    let font(size: float32) =
        match fonts |> Dictionary.tryFind size with
        | ValueSome found -> found
        | ValueNone ->
            let created = new SKFont(typeface, size)
            created.Hinting <- SKFontHinting.None
            created.Subpixel <- true
            created.Edging <- SKFontEdging.SubpixelAntialias
            fonts.[size] <- created
            created

    /// A painter over the font the metrics were generated from.
    static member Embedded() =
        use stream = MathFont.OpenFontFile()
        new Painter(SKTypeface.FromStream stream)

    /// Draws what is only offered at a third of the given paint's opacity.
    member t.Draw(placed: Placed, canvas: SKCanvas, x: float32, baseline: float32, paint: SKPaint) =
        use tentative = paint.Clone()
        tentative.Color <- paint.Color.WithAlpha(byte (int paint.Color.Alpha / 3))
        t.Draw(placed, canvas, x, baseline, paint, tentative)

    member t.Draw
        (placed: Placed, canvas: SKCanvas, x: float32, baseline: float32, solid: SKPaint, tentative: SKPaint) =
        let paint(ink: Ink) = if ink = Ink.Tentative then tentative else solid
        for part in placed.Pma.Parts do
            match part with
            | Part.Glyph(glyph, ink) ->
                let id = BitConverter.GetBytes(uint16 glyph.Glyph.Id)
                use blob =
                    SKTextBlob.Create(
                        ReadOnlySpan<byte> id,
                        SKTextEncoding.GlyphId,
                        font glyph.Size,
                        SKPoint(x + glyph.X, baseline - glyph.Y))
                canvas.DrawText(blob, 0f, 0f, paint ink)
            | Part.Rule(rule, ink) ->
                canvas.DrawRect(
                    SKRect.Create(x + rule.X, baseline - rule.Y - rule.Thickness, rule.Width, rule.Thickness),
                    paint ink)
            | Part.Child child ->
                t.Draw(child, canvas, x + child.X, baseline - child.Y, solid, tentative)

    interface IDisposable with
        member _.Dispose() =
            for font in fonts.Values do
                font.Dispose()
            fonts.Clear()
            typeface.Dispose()
