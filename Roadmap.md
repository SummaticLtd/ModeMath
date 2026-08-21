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
formula is a pointer the collector tracks.

`System.Numerics.Colors.Argb<byte>` replaces it: blittable, four bytes, and in the shared framework
rather than a rendering library. It was [merged for .NET 11](https://github.com/dotnet/runtime/pull/124663)
in March 2026, after the .NET 10 this targets shipped, so the swap waits on moving up. The same change
gives `Color` conversions either way, which leaves it a change of field type and two conversions at
the painter's edge.

## Tracked separately

Selection: #3.
