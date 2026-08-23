# ModeMath

Mathematical formula display and editing for .NET, rendered with SkiaSharp.

A formula is an `MA`: a tree of what is written, not of what it means. `Layout` places one at a font size, and `Painter` draws what was placed onto an `SKCanvas`.

```fsharp
open ModeMath

let formula =
    match Latex.Read @"\frac{-b \pm \sqrt{b^2 - 4ac}}{2a}" with
    | Ok ma -> ma
    | Error error -> failwith error.Message

let placed = (Layout 24f<px>).Of formula
use painter = Painter.Embedded()
painter.Draw(placed, canvas, 0f<px>, placed.Ascent, paint)
```

`examples/` shows what renders, a picture of each formula against its name and size. The formulas themselves are in `tools/RenderExamples/Examples.fs`.

## Editing

`Editor` holds a formula with a cursor in it. Every key reaches it through `Press`, which answers with a new editor rather than changing this one, so a caller keeps whichever it likes and undo costs nothing. `ValueNone` says the key had nothing to do here, leaving the caller free to pass it on.

```fsharp
let pressing(editor: Editor, key: MathKey) =
    editor.Press key |> ValueOption.defaultValue editor

let typed =
    [ MathKey.Character 'x'; MathKey.Superscript; MathKey.Character '2' ]
    |> List.fold (fun editor key -> pressing(editor, key)) (Editor(Layout 24f<px>, MA.Empty))

let clicked = typed.Click(x, y)
```

An opening bracket is drawn faint until its closing one is typed, a run of letters spelling a function name becomes that function, and a formula can be built from `MA` directly instead.

## LaTeX

`Latex.Read` takes a math-mode string and `Latex.Write` gives one back, every argument in braces. What is not understood is refused with the position it stands at, rather than guessed at, and a character the font cannot draw is refused there too, so a formula that was read always lays out. One built from `MA` in code can hold a character the font has no glyph for, and `MA.Undrawable` is the characters it would fail on.

## Fonts

Latin Modern Math, cut down to the glyphs ModeMath draws, and AMS Capital Blackboard Bold are carried inside the assembly along with the licences they are redistributed under. `fonts/README.md` says where each came from and how the metrics beside them are generated.

## Acknowledgements

ModeMath is inspired by [CSharpMath](https://github.com/verybadcat/CSharpMath). Most of the formulas in `examples/` are taken from its rendering tests, and the typesetting is checked against those. ModeMath has no plan to work with any text outside of math mode, so for a LaTeX renderer that includes both text and maths you should use CSharpMath.

## Licence

MIT, in `LICENSE`. The two fonts keep their own: the GUST Font Licence and the SIL Open Font Licence, both carried in `fonts/`.
