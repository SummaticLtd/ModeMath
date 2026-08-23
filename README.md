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

`examples/` shows what renders, each picture beside the formula it came from.

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

`Latex.Read` takes a math-mode string and `Latex.Write` gives one back, every argument in braces. What is not understood is refused with the position it stands at, rather than guessed at, and a character the font cannot draw is refused there too, so laying a formula out cannot fail.

## Fonts

Latin Modern Math, cut down to the glyphs ModeMath draws, and AMS Capital Blackboard Bold are carried inside the assembly along with the licences they are redistributed under. `fonts/README.md` says where each came from and how the metrics beside them are generated.

## Acknowledgements

[CSharpMath](https://github.com/verybadcat/CSharpMath) is what this project was written in admiration of. Most of the formulas in `examples/` are taken from its rendering tests, which is where the typesetting here was checked against, and the blackboard bold face is the copy it ships.

## Licence

MIT, in `LICENSE`. The two fonts keep their own: the GUST Font Licence and the SIL Open Font Licence, both carried in `fonts/`.
