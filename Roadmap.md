# Roadmap

Editing and display work. `examples/` shows what renders today.

## 1. Delimiters

`Bracketed of Bracket * MA * BracketCompletion` gains independent left and right, so that `[0, 1)` is displayable.

`Bracket` gains `Square` and `Curly`. Values are side-agnostic: `Normal` is `(` on the left and `)` on the right.

`Bracketed` is editable, so this changes `MICurs` and every editing member. Entry always produces a matching pair.

## 2. Hit-testing

A point to a `MICurs`, built by descending the display tree and the `MA` together.

## 3. Remaining mathematics

All display-only. The cursor treats each as one unit, stepping over it and deleting it whole, so none needs a `MICurs` case.

Ordered by use in the SummaticApp content library.

| Addition | LaTeX |
|---|---|
| `Styled` | `\mathrm`, `\mathbf`, `\mathbb`, `\it` |
| `Coloured` | `\color`, `\red`, `\blue` and the rest |
| Relations and arrows | `\leq`, `\Rightarrow`, `\approx`, `\in`, `\to` |
| `Text` | `\text` |
| Accents and overline | `\vec`, `\hat`, `\bar`, `\overline`, `\tilde`, `\dot` |
| Large operators with limits | `\int`, `\sum`, `\lim` |
| Tables | `pmatrix`, `eqnarray`, `cases`, `vmatrix`, `array` |
| Binary operators | `\ast`, `\cap`, `\pm`, `\cup`, `\div` |
| Spacing | `\quad`, `\qquad` |
| Fraction with no rule | `\binom`, `\choose` |
| Open-ended function names | `\operatorname`, `\det`, `\arg` |

Relations and binary operators carry a TeX atom class, which sets the spacing around them.

`MA.Char` already covers `\infty`, `\partial`, `\circ`, `\emptyset`, `\therefore`, `\mid` and the ellipses.

## 4. Adopt FSUtils

SummaticApp's `FSUtils` is not open source yet. Once it is, ModeMath should take it rather than keep its
own `ImmArray` copy, and use its type-specific maxima in place of `max`, its `Dictionary.tryGet` in place
of matching on `TryGetValue`, and its units of measure for the lengths a `Display` carries.

## Tracked separately

Selection: #3.
