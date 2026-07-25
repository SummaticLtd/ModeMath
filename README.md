# ModeMath

Mathematical formula display and editing for .NET, rendered with SkiaSharp.

## Projects

| | |
|---|---|
| `ModeMath` | `MA`, the formula tree, and `MICurs`, a formula with a cursor in it. |
| `ModeMath.Tests` | xunit. |

## Design

`MICurs` mirrors `MA`: every case holds one `MICurs` child and `MA` for the rest, bottoming
out at `CursorOrEmpty`. One immutable value therefore carries both the formula and the caret
position, and no invalid caret position is representable. `MICurs.ToMA` erases the cursor.

Adding a case to `MA` requires a corresponding case in `MICurs` per editable child.

## Status

Formula tree, cursor type, backspace and character entry. No rendering yet.
