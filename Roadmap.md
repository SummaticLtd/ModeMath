# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Editing

`Editor` covers typing, clicking, moving, backspace and delete, and putting in a fraction, root,
bracket or script. What is left:

| Addition | Why |
|---|---|
| Selection | Tracked separately: #3 |
| Up and down by where the atoms are | `Move` is structural, so leaving a denominator ignores the point it left from |
| Undo | The editor is a value, so a caller can keep the old ones. Nothing here does it for them |

## 2. LaTeX

`Latex.Read` reads a math-mode string into an `MA`. Writing one back out is still to come.

It reads and draws 98.4% of the 17,220 distinct formulas in the SummaticApp content library.
Every formula it reads, it draws: a character the font cannot draw is refused where it stands, so
that laying a formula out cannot fail. `Editor.Type` turns such a key down the same way, and
`MA.Undrawable` answers for a formula built in code. What the reader turns down:

| Turned down | Formulas | Why |
|---|---|---|
| Alignment markers outside any environment | 100 | A house convention rather than LaTeX |
| `\overbar` | 30 | A house macro for `\overline` |
| `\bf` | 30 | Needs `Styled` |
| `\arg`, `\det`, `\operatorname`, `\inf`, `\sup` | 58 | Needs open-ended function names |
| `\choose` | 16 | The one infix command |
| Malformed content | 38 | A trailing `\`, `s_{stop}_{b}`, `\l(`, an unclosed group |

## 3. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none
needs a `MACurs` case.

| Addition | LaTeX | Uses |
|---|---|---|
| Open-ended function names | `\operatorname`, `\det`, `\arg` | 99 |
| `Styled` | `\bf`, `\it`, `\mathcal`, `\mathfrak` | 31 |
| Accents below | `\underdot` | 0 |
| The double bar | `\Vert`, and the outer pair of a `Vmatrix` | 0 |

Relations, binary operators and arrows are done, each carrying the TeX atom class that spaces it.
`MA.Char` covers the symbols a formula names, `MA.BoldVar` covers `\mathbf`, `MA.Blackboard` reaches
all 26 capitals with `ℂ ℍ ℕ ℙ ℚ ℝ ℤ` drawn from the same face, and `MA.Text` covers `\mathrm` over a
word as well as `\text`.

Uses are counts in the SummaticApp content library, which asks for no `\mathcal`, `\mathfrak`,
`\mathtt` or `\mathsf` at all.

## TODO: an efficient colour

`MA.Coloured` carries a `System.Drawing.Color`, as CSharpMath's `Colored` does. It is 24 bytes for
what is four bytes of RGBA, and one of its four fields is a string reference, so every colour in a
formula is a pointer the collector tracks. `SKColor` is four bytes but would put SkiaSharp in the
semantic model, and `Summatic.Drawing` is not published.

## Tracked separately

Selection: #3.
