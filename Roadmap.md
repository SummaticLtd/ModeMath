# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Hit-testing

A point to a `MICurs`, built by descending the display tree and the `MA` together.

## 2. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none
needs a `MICurs` case.

Ordered by use in the SummaticApp content library.

| Addition | LaTeX |
|---|---|
| `Styled` | `\mathrm`, `\mathbf`, `\it` |
| `Coloured` | `\color`, `\red`, `\blue` and the rest |
| Relations and arrows | `\leq`, `\Rightarrow`, `\approx`, `\in`, `\to` |
| `Text` | `\text` |
| Binary operators | `\ast`, `\cap`, `\pm`, `\cup`, `\div` |
| Spacing | `\quad`, `\qquad` |
| Marks that stretch | `\widehat`, `\overbrace`, `\underbrace` |
| Accents below | `\underdot` |
| The double bar | `\Vert`, and the outer pair of a `Vmatrix` |
| Open-ended function names | `\operatorname`, `\det`, `\arg` |

Relations and binary operators carry a TeX atom class, which sets the spacing around them.

`MA.Char` already covers `\infty`, `\partial`, `\circ`, `\emptyset`, `\therefore`, `\mid` and the
ellipses, and `\mathbb` is done: `MA.Blackboard` reaches all 26 capitals and `ℂ ℍ ℕ ℙ ℚ ℝ ℤ` are drawn
from the same face.

The marks that stretch need the `MATH` table's horizontal constructions, which MathTableGen parses but
does not yet emit.

## Tracked separately

Selection: #3.
