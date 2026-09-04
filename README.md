# ModeMath

[![NuGet](https://img.shields.io/nuget/v/ModeMath.svg)](https://www.nuget.org/packages/ModeMath)

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

`Painter.Embedded()` draws glyphs in greyscale.
`Painter.Embedded SKFontEdging.SubpixelAntialias` draws them for an RGB-striped display, which shows only where the `SKSurface` was built with an `SKPixelGeometry` and reaches the screen unscaled.
Asked for it anywhere else, Skia draws a softer mask holding no subpixel information, so the default is the better of the two.

`examples/` shows what renders, a picture of each formula against its name and size. The formulas themselves are in `tools/RenderExamples/Examples.fs`.

## Editing

`Editor` holds a formula with a cursor in it, and the states it stood at before. `Press` answers whether the key was wanted, leaving a caller free to pass on the ones it turns down, and `Undo` and `Redo` step through the edits, a run of characters going in one go.

```fsharp
let editor = Editor(Layout 24f<px>, MA.Empty)

for key in [ MathKey.Character 'x'; MathKey.Superscript; MathKey.Character '2' ] do
    editor.Press key |> ignore

editor.Click(x, y)

let bounds = editor.State.Bounds
painter.Draw(editor.State.Cursor, canvas, -bounds.X, bounds.Y + bounds.Thickness, paint)
```

A formula with a cursor in it is placed by its `Bounds` rather than by what it draws, since the cursor stands where no atom does: at either end of the line, and above a numerator or below a denominator. They hold both, and hold still as the cursor moves through the formula.

`EditorState` is the value underneath: every key answers with a new state rather than changing this one, so a caller that would rather keep its own history holds whichever states it likes.

An opening bracket is drawn faint until its closing one is typed, a run of letters spelling a name becomes what it names — a function, the radical `sqrt` opens, or the mark `degree` gives — and `MathKey.Function` puts a function in from a keypad, brackets opened for what it is called on. A formula can be built from `MA` directly instead.

## LaTeX

`Latex.Read` takes a math-mode string and `Latex.Write` gives one back, every argument in braces and characters as themselves. What is not understood is refused with the position it stands at, rather than guessed at, and a character the font cannot draw is refused there too, so a formula that was read always lays out. One built from `MA` in code can hold a character the font has no glyph for, and `MA.Undrawable` is the characters it would fail on.

`\color` and `\textcolor` name their colours from a `Palette`, which a caller may replace to give a name a colour of its own.

```fsharp
let palette = Palette.Default.With("green", Color.FromArgb(255, 130, 212, 20))
let formula = Latex.Read(@"\color{green}{x}", palette)
```

## Fonts

Latin Modern Math, cut down to the glyphs ModeMath draws, and AMS Capital Blackboard Bold are carried inside the assembly along with the licences they are redistributed under. `fonts/README.md` says where each came from and how the metrics beside them are generated.

## Acknowledgements

ModeMath is inspired by [CSharpMath](https://github.com/verybadcat/CSharpMath). Most of the formulas in `examples/` are taken from its rendering tests, and the typesetting is checked against those. ModeMath has no plan to work with any text outside of math mode, so for a LaTeX renderer that includes both text and maths you should use CSharpMath.

## Licence

MIT, in `LICENSE`. The two fonts keep their own: the GUST Font Licence and the SIL Open Font Licence, both carried in `fonts/`.
