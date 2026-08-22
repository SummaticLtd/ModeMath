# ModeMath

Mathematical formula display and editing for .NET, rendered with SkiaSharp.

A formula is an `MA`: a tree of what is written, not of what it means. `Layout` places one at a font
size, and `Painter` draws what was placed onto an `SKCanvas`.

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

`Editor` holds a formula with a cursor in it and answers every key with a new one, so the caller
keeps whichever it likes and undo costs nothing.

```fsharp
let editor = Editor(Layout 24f<px>, MA.Empty)
let typed = editor.Type 'x' // ValueNone for a key the font cannot draw
let divided = editor.InsertFraction // over the term the cursor stands after
let moved = editor.Move Direction.Left // ValueNone at the start of the formula
let clicked = editor.Click(x, y) // the cursor put where a point is
```

## LaTeX

`Latex.Read` takes a math-mode string and `Latex.Write` gives one back, every argument in braces.
What is not understood is refused with the position it stands at, rather than guessed at, and a
character the font cannot draw is refused there too — so laying a formula out cannot fail.

## Fonts

Latin Modern Math and AMS Capital Blackboard Bold are carried inside the assembly, along with the
licences they are redistributed under.

## Licence

MIT. The two fonts keep their own: the GUST Font Licence and the SIL Open Font Licence.
