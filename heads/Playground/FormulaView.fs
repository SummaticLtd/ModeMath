namespace Playground

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Platform
open Avalonia.Rendering.SceneGraph
open Avalonia.Skia
open SkiaSharp
open ModeMath

/// Hands a Skia canvas to whatever wants to draw on it, or draws nothing where there is none.
type private SkiaDrawing(bounds: Rect, draw: SKCanvas -> unit) =
    interface ICustomDrawOperation with
        member _.Bounds = bounds
        // Everything drawn on takes clicks, or the canvas would let them fall through it.
        member _.HitTest(point) = bounds.Contains point
        member _.Equals(_) = false
        member _.Dispose() = ()

        member _.Render(context: ImmediateDrawingContext) =
            match context.TryGetFeature<ISkiaSharpApiLeaseFeature>() with
            | null -> ()
            | feature ->
                use lease = feature.Lease()
                draw lease.SkCanvas

/// The formula being edited, drawn at a margin from the top left and taking the keys typed at it.
type FormulaView() as t =
    inherit Control()

    let painter = Painter.Embedded()
    let margin = 24f<px>
    let mutable size = 48f<px>
    let mutable editor = Editor(Layout size, MA.Empty)

    let redraw() =
        t.InvalidateVisual()

    do
        t.Focusable <- true
        t.Cursor <- new Cursor(StandardCursorType.Ibeam)

    /// What the formula stands at, which loading LaTeX or a font size replaces.
    member _.Editor
        with get () = editor
        and set (value: Editor) =
            editor <- value
            redraw()

    /// The em the formula is drawn at, which lays it out again where the cursor stays put.
    member _.FontSize
        with get () = size
        and set (value: float32<px>) =
            size <- value
            editor <- Editor(Layout value, editor.Formula)
            redraw()

    member _.Formula = editor.Formula

    override _.Render(context: DrawingContext) =
        let bounds = t.Bounds
        let draw(canvas: SKCanvas) =
            use paint = new SKPaint(Color = SKColors.Black, IsAntialias = true)
            let top = editor.Bounds
            painter.Draw(
                editor.Cursor,
                canvas,
                margin - top.X,
                margin + top.Y + top.Thickness,
                paint)
        context.Custom(new SkiaDrawing(Rect(0.0, 0.0, bounds.Width, bounds.Height), draw))

    override _.OnPointerPressed(e: PointerPressedEventArgs) =
        let point = e.GetPosition(t)
        let top = editor.Bounds
        let x = float32 point.X * 1f<px> - margin + top.X
        let y = -(float32 point.Y * 1f<px> - margin - top.Y - top.Thickness)
        editor <- editor.Click(x, y)
        t.Focus() |> ignore
        redraw()
        e.Handled <- true

    override _.OnTextInput(e: TextInputEventArgs) =
        match e.Text with
        | null -> ()
        | text ->
            for character in text do
                editor <- t.Typed character
            redraw()
            e.Handled <- true

    /// One character typed, which the keys that build an atom take before the formula sees them.
    member private _.Typed(character: char) =
        match character with
        | '/' -> editor.InsertFraction
        | '^' -> editor.InsertSuperscript
        | '_' -> editor.InsertSubscript
        | '(' -> editor.InsertBracket(Brackets.Matching Bracket.Normal)
        | '[' -> editor.InsertBracket(Brackets.Matching Bracket.Square)
        | '|' -> editor.InsertBracket(Brackets.Matching Bracket.Line)
        | _ -> editor.Type character

    override _.OnKeyDown(e: KeyEventArgs) =
        let moved(direction: Direction) =
            match editor.Move direction with
            | ValueSome moved -> editor <- moved
            | ValueNone -> ()
        let removed(gone: Editor voption) =
            match gone with
            | ValueSome gone -> editor <- gone
            | ValueNone -> ()
        match e.Key with
        | Key.Left -> moved Direction.Left
        | Key.Right -> moved Direction.Right
        | Key.Up -> moved Direction.Up
        | Key.Down -> moved Direction.Down
        | Key.Back -> removed editor.BackSpace
        | Key.Delete -> removed editor.Delete
        | _ -> ()
        match e.Key with
        | Key.Left | Key.Right | Key.Up | Key.Down | Key.Back | Key.Delete ->
            redraw()
            e.Handled <- true
        | _ -> ()
