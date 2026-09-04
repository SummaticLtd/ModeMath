# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Editing

`Editor` covers typing, clicking, moving, backspace and delete, undo and redo, and putting in a fraction, root, bracket or script. What is left:

| Addition | Why |
|---|---|
| Selection | Tracked separately: #3 |
| Up and down by where the atoms are | `Move` is structural, so leaving a denominator ignores the point it left from |

## 2. LaTeX

`Latex.Read` reads a math-mode string into an `MA` and `Latex.Write` writes one back out, every argument in braces and characters as themselves.
Every one of the 17,085 formulas the content library reads is written back out and read again as the same formula.

It reads and draws 99.2% of the 17,220 distinct formulas in the SummaticApp content library.
Every formula it reads, it draws: a character the font cannot draw is refused where it stands, so
that laying a formula out cannot fail. `EditorState.Type` turns such a key down the same way, and
`MA.Undrawable` answers for a formula built in code. What the reader turns down:

| Turned down | Formulas | Why |
|---|---|---|
| Alignment markers outside any environment | 112 | A house convention rather than LaTeX, and not one to read |
| Malformed content | 31 | A trailing `\`, `s_{stop}_{b}`, `\l(`, an unclosed group, `$$` |
| `\red` | 1 | A text macro caught in a formula by the extraction |

## 3. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none
needs a `MACurs` case.

| Addition | LaTeX | Uses |
|---|---|---|
| `Styled` | `\it`, `\mathcal`, `\mathfrak`, `\mathtt` | 0 |
| Limits on the operators that take them | `\det`, `\sup`, `\inf`, `\max`, `\min`, `\Pr`, `\gcd`, `\liminf`, `\limsup` | 0 |
| Accents below | `\underdot` | 0 |
| The double bar | `\Vert`, and the outer pair of a `Vmatrix` | 0 |

`MathFunction` names every function LaTeX defines, which `\operatorname` resolves against as well.
A name outside it is refused rather than guessed at, so a new one is a case here and nowhere else.
TeX sets the limits of the operators above them in display style, as `MA.BigOp` does for `\lim`;
those named above take scripts beside them instead, which is what `\max` and `\min` did before.

Relations, binary operators and arrows are done, each carrying the TeX atom class that spaces it.
`MA.Char` covers the symbols a formula names, `MA.BoldVar` covers `\mathbf`, `MA.Blackboard` reaches
all 26 capitals with `ℂ ℍ ℕ ℙ ℚ ℝ ℤ` drawn from the same face, and `MA.Text` covers `\text` as
well as the letters `\mathrm` sets upright, whose argument stays mathematics.

Uses are counts in the SummaticApp content library, which asks for no `\mathcal`, `\mathfrak`,
`\mathtt` or `\mathsf` at all.

## TODO: an efficient colour

`MA.Coloured` carries a `System.Drawing.Color`, as CSharpMath's `Colored` does. It is 24 bytes for
what is four bytes of RGBA, and one of its four fields is a string reference, so every colour in a
formula is a pointer the collector tracks. `SKColor` is four bytes but would put SkiaSharp in the
semantic model, and `Summatic.Drawing` is not published.

## Tracked separately

Selection: #3.
