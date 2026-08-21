# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Editing

`Editor` covers typing, clicking, moving, backspace and delete, and putting in a fraction, root,
bracket or script. What is left:

| Addition | Why |
|---|---|
| Selection | Tracked separately: #3 |
| Up and down by where the atoms are | `Move` is structural, so leaving a denominator ignores the point it left from |
| Putting a bracket around what is already there | Typing `(` before a formula should be able to take it in |
| Undo | The editor is a value, so a caller can keep the old ones. Nothing here does it for them |

## 2. LaTeX

`Latex.Read` reads a math-mode string into an `MA`. Writing one back out is still to come.

It reads 98.0% of the 17,220 distinct formulas in the SummaticApp content library, and lays out
97.4%. What it turns down, by how often the library asks for it:

| Turned down | Uses | Why |
|---|---|---|
| Alignment markers outside any environment | 97 | A house convention rather than LaTeX |
| `\overbar`, `\blue`, `\green`, `\gray` | 37 | House macros |
| `\bf`, `\it` | 33 | Needs `Styled` |
| `\arg`, `\det`, `\operatorname`, `\inf`, `\sup` | 60 | Needs open-ended function names |
| `\circ`, `\triangle`, `\uparrow`, `\downarrow`, `\longrightarrow` | 74 | No glyph, see below |
| `\choose` | 14 | The one infix command |

## TODO: glyphs the font data leaves out

`MathFont.OfChar` finds no glyph for `∘ ∣ ↑ ↓ △ ⟶ ⌈ ⌊ £ – µ`, and laying one out throws. The
stretchy delimiters among them are in the font under another mechanism; the rest may be a gap in
what `MathTableGen` emits.

## 3. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none
needs a `MACurs` case.

Ordered by use in the SummaticApp content library.

| Addition | LaTeX |
|---|---|
| `Styled` | `\mathbf`, `\mathcal`, `\mathfrak`, `\it` |
| Relations and arrows | `\leq`, `\Rightarrow`, `\approx`, `\in`, `\to` |
| Binary operators | `\ast`, `\cap`, `\pm`, `\cup`, `\div` |
| Accents below | `\underdot` |
| The double bar | `\Vert`, and the outer pair of a `Vmatrix` |
| Open-ended function names | `\operatorname`, `\det`, `\arg` |

Relations and binary operators carry a TeX atom class, which sets the spacing around them.

`MA.Char` already covers `\infty`, `\partial`, `\emptyset`, `\therefore` and the
ellipses, and `\mathbb` is done: `MA.Blackboard` reaches all 26 capitals and `ℂ ℍ ℕ ℙ ℚ ℝ ℤ` are drawn
from the same face. `MA.Text` covers `\mathrm` over a word as well as `\text`.

## TODO: an efficient colour

`MA.Coloured` carries a `System.Drawing.Color`, as CSharpMath's `Colored` does. It is 24 bytes for
what is four bytes of RGBA, and one of its four fields is a string reference, so every colour in a
formula is a pointer the collector tracks. `SKColor` is four bytes but would put SkiaSharp in the
semantic model, and `Summatic.Drawing` is not published.

## Tracked separately

Selection: #3.
