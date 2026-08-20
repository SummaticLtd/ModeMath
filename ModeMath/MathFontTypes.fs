namespace ModeMath

/// A codepoint the font maps, with the glyph that draws it.
[<Struct>]
type internal CG(codepoint: int, glyph: int) =
    /// The Unicode codepoint, as a formula holds it.
    member _.Codepoint = codepoint
    /// Index of the glyph in the embedded font file.
    member _.Glyph = glyph

/// What one glyph occupies, in design units from its origin on the baseline, y upwards.
[<Struct>]
type internal GlyphMetrics(advance: int, left: int, right: int, top: int, bottom: int) =
    /// How far the pen moves along the line after drawing the glyph.
    member _.Advance = advance
    /// Where the ink begins, across from the origin.
    member _.Left = left
    /// Where the ink ends, across from the origin.
    member _.Right = right
    /// How far the ink rises above the baseline.
    member _.Top = top
    /// How far the ink falls below the baseline, negative downwards.
    member _.Bottom = bottom

/// How far a glyph leans past its advance.
[<Struct>]
type internal ItalicCorrection(glyph: int, correction: int) =
    /// Index of the glyph in the embedded font file.
    member _.Glyph = glyph
    /// Width to leave after the glyph, in design units.
    member _.Correction = correction

/// Where an accent is centred over a glyph.
[<Struct>]
type internal TopAccent(glyph: int, attachment: int) =
    /// Index of the glyph in the embedded font file.
    member _.Glyph = glyph
    /// Distance across from the glyph's origin, which may be negative.
    member _.Attachment = attachment

/// One ready-made size of a glyph that stretches.
[<Struct>]
type internal StretchSize(glyph: int, advance: int) =
    /// Index of the glyph in the embedded font file.
    member _.Glyph = glyph
    /// The size to choose on: a height along the vertical axis, a width along the horizontal.
    member _.Advance = advance

/// One piece of a glyph assembled to reach a size no ready-made glyph covers.
[<Struct>]
type internal AssemblyPart
    (glyph: int, startConnector: int, endConnector: int, fullAdvance: int, isExtender: bool) =
    /// Index of the glyph in the embedded font file.
    member _.Glyph = glyph
    /// How far the piece may overlap its predecessor, in design units.
    member _.StartConnector = startConnector
    /// How far the piece may overlap its successor, in design units.
    member _.EndConnector = endConnector
    /// The length the piece contributes, in design units.
    member _.FullAdvance = fullAdvance
    /// Repeatable, so that any length can be reached.
    member _.IsExtender = isExtender

/// How one glyph grows along an axis, as slices of that axis's size and part arrays.
[<Struct>]
type internal StretchConstruction
    (glyph: int, sizeStart: int, sizeCount: int, partStart: int, partCount: int, italicCorrection: int) =
    /// Index of the unstretched glyph in the embedded font file.
    member _.Glyph = glyph
    /// Where this glyph's sizes begin, smallest first.
    member _.SizeStart = sizeStart
    member _.SizeCount = sizeCount
    /// Where this glyph's assembly pieces begin, in order along the axis.
    member _.PartStart = partStart
    /// Zero where the font gives no assembly, leaving the sizes as the only choices.
    member _.PartCount = partCount
    /// The assembly's own italic correction, in design units.
    member _.ItalicCorrection = italicCorrection
