# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Hit-testing

A point to a `MICurs`, built by descending the display tree and the `MA` together.

## 2. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none needs a `MICurs` case.

Ordered by use in the SummaticApp content library.

| Addition | LaTeX |
|---|---|
| `Styled` | `\mathrm`, `\mathbf`, `\mathbb`, `\it` |
| `Coloured` | `\color`, `\red`, `\blue` and the rest |
| Relations and arrows | `\leq`, `\Rightarrow`, `\approx`, `\in`, `\to` |
| `Text` | `\text` |
| Accents and overline | `\vec`, `\hat`, `\bar`, `\overline`, `\tilde`, `\dot` |
| Tables | `pmatrix`, `eqnarray`, `cases`, `vmatrix`, `array` |
| Binary operators | `\ast`, `\cap`, `\pm`, `\cup`, `\div` |
| Spacing | `\quad`, `\qquad` |
| Fraction with no rule | `\binom`, `\choose` |
| Open-ended function names | `\operatorname`, `\det`, `\arg` |

Relations and binary operators carry a TeX atom class, which sets the spacing around them.

`MA.Char` already covers `\infty`, `\partial`, `\circ`, `\emptyset`, `\therefore`, `\mid` and the ellipses.

## 3. Units of measure

A `Display` carries every length as a bare `float32`, so points, ems and font units are one type and
nothing catches mixing them. `Summatic.FSUtils` supplies the conversions and typed maxima to give them
units.

## Tracked separately

Selection: #3.
