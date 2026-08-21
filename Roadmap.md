# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. An editor over PlacedCurs

`PlacedCurs.Nearest` takes a point to a cursor and `Layout.Of` draws one, so what is left is to put
the editing operations on the laid-out cursor rather than the structural one: an edit goes
`ToMACurs`, edits, and lays out again, and a move goes the same way without laying out. Then the
caller holds a `PlacedCurs` and a `Layout`, and never names a `MACurs`.

Moving up and down is structural, so it ignores where the atoms are. A `PlacedCurs` knows, which is
what would let leaving a denominator land under the same point in the numerator.

## 2. Remaining mathematics

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

`MA.Char` already covers `\infty`, `\partial`, `\circ`, `\emptyset`, `\therefore`, `\mid` and the
ellipses, and `\mathbb` is done: `MA.Blackboard` reaches all 26 capitals and `ℂ ℍ ℕ ℙ ℚ ℝ ℤ` are drawn
from the same face. `MA.Text` covers `\mathrm` over a word as well as `\text`.

## TODO: an efficient colour

`MA.Coloured` carries a `System.Drawing.Color`, as CSharpMath's `Colored` does. It is 24 bytes for
what is four bytes of RGBA, and one of its four fields is a string reference, so every colour in a
formula is a pointer the collector tracks. `SKColor` is four bytes but would put SkiaSharp in the
semantic model, and `Summatic.Drawing` is not published.

## Tracked separately

Selection: #3.
