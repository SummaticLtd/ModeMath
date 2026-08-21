namespace ModeMath

open FSUtils

/// A design unit of a face, which is drawn on MathConstants.UnitsPerEm of them to the em.
[<Measure>]
type du

/// The unit of the caller's canvas, which a font size is given in and a formula is laid out in.
[<Measure>]
type px

[<AutoOpen>]
module Units =
    /// Design units as a float, keeping the measure that `float32` alone would strip.
    let inline design(units: int<du>) = Measure.withFloat32Unit<du>(float32 units)
