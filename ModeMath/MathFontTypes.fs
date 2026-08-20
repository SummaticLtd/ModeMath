namespace ModeMath

/// A glyph of the math font, measured in design units from the origin on the baseline, y upwards.
[<Struct>]
type Glyph internal (id: int, advance: int, top: int, bottom: int, italicCorrection: int) =
    /// Index in the embedded font file, which is what the painter draws.
    member _.Id = id
    /// How far the pen moves along the line after drawing the glyph.
    member _.Advance = advance
    /// How far the ink rises above the baseline.
    member _.Top = top
    /// How far the ink falls below the baseline, negative downwards.
    member _.Bottom = bottom
    /// How far the glyph leans past its advance, zero where it does not lean.
    member _.ItalicCorrection = italicCorrection

/// A codepoint the font maps, with the glyph that draws it.
[<Struct>]
type CG internal (codepoint: int, glyph: Glyph) =
    /// The Unicode codepoint, as a formula holds it.
    member _.Codepoint = codepoint
    member _.Glyph = glyph

/// One ready-made size of a glyph that stretches.
[<Struct>]
type StretchSize internal (glyph: Glyph, advance: int) =
    member _.Glyph = glyph
    /// The size to choose on: a height along the vertical axis, a width along the horizontal.
    member _.Advance = advance

/// One piece of a glyph assembled to reach a size no ready-made glyph covers.
[<Struct>]
type AssemblyPart
    internal (glyph: Glyph, startConnector: int, endConnector: int, fullAdvance: int, isExtender: bool) =
    member _.Glyph = glyph
    /// How far the piece may overlap its predecessor, in design units.
    member _.StartConnector = startConnector
    /// How far the piece may overlap its successor, in design units.
    member _.EndConnector = endConnector
    /// The length the piece contributes, in design units.
    member _.FullAdvance = fullAdvance
    /// Repeatable, so that any length can be reached.
    member _.IsExtender = isExtender

/// A glyph that grows along an axis, so that callers need not ask whether it does.
[<Struct>]
type StretchyGlyph internal (glyph: Glyph, sizes: StretchSize array, parts: AssemblyPart array) =
    /// The unstretched glyph, whose metrics apply until it is grown.
    member _.Glyph = glyph
    /// Sizes in increasing order, the first being the unstretched glyph where the font offers one.
    member _.SizeCount = sizes.Length
    member _.Size(i: int) = sizes.[i]
    /// Zero where the font gives no assembly, the sizes then being the only ones available.
    member _.PartCount = parts.Length
    member _.Part(i: int) = parts.[i]
