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
            | NonNull feature ->
                use lease = feature.Lease()
                draw lease.SkCanvas
            | Null -> ()

/// What a character typed stands for, which is the key itself where it builds no atom.
module private Character =
    let key(character: char) =
        match character with
        | '/' -> MathKey.Fraction
        | '^' -> MathKey.Superscript
        | '_' -> MathKey.Subscript
        | '(' -> MathKey.Open Bracket.Normal
        | '[' -> MathKey.Open Bracket.Square
        | '{' -> MathKey.Open Bracket.Curly
        | ')' -> MathKey.Close Bracket.Normal
        | ']' -> MathKey.Close Bracket.Square
        | '}' -> MathKey.Close Bracket.Curly
        | '|' -> MathKey.Bar
        | _ -> MathKey.Character character

/// The formula being edited, drawn at a margin from the top left and taking the keys typed at it.
type FormulaView() as t =
    inherit Control()

    let painter = Painter.Embedded()
    let margin = 24f<px>
    let size = 48f<px>
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

    /// The em the formula is drawn at.
    member _.FontSize = size

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

    /// A key given to the formula, which keeps what it makes of it and says whether it was wanted.
    member private _.Press(key: MathKey) =
        match editor.Press key with
        | ValueSome pressed ->
            editor <- pressed
            redraw()
            true
        | ValueNone -> false

    override _.OnTextInput(e: TextInputEventArgs) =
        match e.Text with
        | NonNull text ->
            for character in text do
                if t.Press(Character.key character) then e.Handled <- true
        | Null -> ()

    override _.OnKeyDown(e: KeyEventArgs) =
        let key =
            match e.Key with
            | Key.Left -> ValueSome(MathKey.Move Direction.Left)
            | Key.Right -> ValueSome(MathKey.Move Direction.Right)
            | Key.Up -> ValueSome(MathKey.Move Direction.Up)
            | Key.Down -> ValueSome(MathKey.Move Direction.Down)
            | Key.Back -> ValueSome MathKey.Backspace
            | Key.Delete -> ValueSome MathKey.Delete
            | _ -> ValueNone
        match key with
        | ValueSome key -> e.Handled <- t.Press key
        | ValueNone -> ()
