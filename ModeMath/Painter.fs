namespace ModeMath

open System
open System.Collections.Generic
open SkiaSharp

/// Draws a Display onto an SKCanvas, whose y grows downwards where a Display's grows upwards.
type Painter(typeface: SKTypeface) =
    let fonts = Dictionary<float32, SKFont>()

    let font(size: float32) =
        match fonts.TryGetValue size with
        | true, found -> found
        | false, _ ->
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
    member t.Draw(display: Display, canvas: SKCanvas, x: float32, baseline: float32, paint: SKPaint) =
        use tentative = paint.Clone()
        tentative.Color <- paint.Color.WithAlpha(byte (int paint.Color.Alpha / 3))
        t.Draw(display, canvas, x, baseline, paint, tentative)

    member t.Draw
        (display: Display, canvas: SKCanvas, x: float32, baseline: float32, solid: SKPaint, tentative: SKPaint) =
        let paint(ink: Ink) = if ink = Ink.Tentative then tentative else solid
        match display.Content with
        | Content.Glyph(glyph, size, ink) ->
            let id = BitConverter.GetBytes(uint16 glyph.Id)
            use blob =
                SKTextBlob.Create(
                    ReadOnlySpan<byte> id, SKTextEncoding.GlyphId, font size, SKPoint(x, baseline))
            canvas.DrawText(blob, 0f, 0f, paint ink)
        | Content.Rule ink ->
            canvas.DrawRect(
                SKRect.Create(x, baseline - display.Ascent, display.Width, display.Height),
                paint ink)
        | Content.Children children ->
            for child in children do
                t.Draw(child.Display, canvas, x + child.X, baseline - child.Y, solid, tentative)

    interface IDisposable with
        member _.Dispose() =
            for font in fonts.Values do
                font.Dispose()
            fonts.Clear()
            typeface.Dispose()
