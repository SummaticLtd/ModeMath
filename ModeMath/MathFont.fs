namespace ModeMath

open System
open System.IO

module private Lookup =
    let value(keys: int array, values: int array, key: int) =
        let index = Array.BinarySearch(keys, key)
        if index >= 0 then ValueSome values.[index] else ValueNone

    let index(keys: int array, key: int) =
        let index = Array.BinarySearch(keys, key)
        if index >= 0 then ValueSome index else ValueNone

type internal StretchTable
    (
        glyphs: int array,
        variantStarts: int array,
        variantGlyphs: int array,
        variantAdvances: int array,
        partStarts: int array,
        partGlyphs: int array,
        partStartConnectors: int array,
        partEndConnectors: int array,
        partFullAdvances: int array,
        partExtenders: int array,
        assemblyItalicsCorrections: int array
    ) =
    member _.Glyphs = glyphs
    member _.VariantStarts = variantStarts
    member _.VariantGlyphs = variantGlyphs
    member _.VariantAdvances = variantAdvances
    member _.PartStarts = partStarts
    member _.PartGlyphs = partGlyphs
    member _.PartStartConnectors = partStartConnectors
    member _.PartEndConnectors = partEndConnectors
    member _.PartFullAdvances = partFullAdvances
    member _.PartExtenders = partExtenders
    member _.AssemblyItalicsCorrections = assemblyItalicsCorrections

    static member val Vertical =
        StretchTable(
            MathFontData.verticalGlyphs,
            MathFontData.verticalVariantStarts,
            MathFontData.verticalVariantGlyphs,
            MathFontData.verticalVariantAdvances,
            MathFontData.verticalPartStarts,
            MathFontData.verticalPartGlyphs,
            MathFontData.verticalPartStartConnectors,
            MathFontData.verticalPartEndConnectors,
            MathFontData.verticalPartFullAdvances,
            MathFontData.verticalPartExtenders,
            MathFontData.verticalAssemblyItalicsCorrections)

    static member val Horizontal =
        StretchTable(
            MathFontData.horizontalGlyphs,
            MathFontData.horizontalVariantStarts,
            MathFontData.horizontalVariantGlyphs,
            MathFontData.horizontalVariantAdvances,
            MathFontData.horizontalPartStarts,
            MathFontData.horizontalPartGlyphs,
            MathFontData.horizontalPartStartConnectors,
            MathFontData.horizontalPartEndConnectors,
            MathFontData.horizontalPartFullAdvances,
            MathFontData.horizontalPartExtenders,
            MathFontData.horizontalAssemblyItalicsCorrections)

/// A glyph of the math font, measured in design units from the origin on the baseline, y upwards.
[<Struct>]
type Glyph internal (id: int) =
    member _.Id = id
    member _.Advance = MathFontData.advances.[id]
    member _.Left = MathFontData.boundsLeft.[id]
    member _.Right = MathFontData.boundsRight.[id]
    member _.Top = MathFontData.boundsTop.[id]
    member _.Bottom = MathFontData.boundsBottom.[id]

    /// Zero where the font gives none.
    member _.ItalicCorrection =
        Lookup.value(MathFontData.italicsCorrectionGlyphs, MathFontData.italicsCorrections, id)
        |> ValueOption.defaultValue 0

    /// Where an accent sits over this glyph. ValueNone where the font gives none, meaning the accent is centred.
    member _.TopAccentAttachment =
        Lookup.value(MathFontData.topAccentGlyphs, MathFontData.topAccentAttachments, id)

    /// Tall enough that a following superscript is not raised to clear it.
    member _.IsExtendedShape = (Lookup.index(MathFontData.extendedShapeGlyphs, id)).IsSome

    /// How this glyph grows taller, if it does.
    member _.VerticalStretch: Stretch voption =
        Lookup.index(StretchTable.Vertical.Glyphs, id)
        |> ValueOption.map (fun index -> Stretch(StretchTable.Vertical, index))

    /// How this glyph grows wider, if it does.
    member _.HorizontalStretch: Stretch voption =
        Lookup.index(StretchTable.Horizontal.Glyphs, id)
        |> ValueOption.map (fun index -> Stretch(StretchTable.Horizontal, index))

/// A ready-drawn glyph of one size in a stretched sequence.
and [<Struct>] StretchVariant internal (glyph: Glyph, advance: int) =
    member _.Glyph = glyph
    /// The height of a vertical variant, or the width of a horizontal one.
    member _.Advance = advance

/// A piece from which an arbitrarily long stretched glyph is assembled.
and [<Struct>] StretchPart
    internal (glyph: Glyph, startConnector: int, endConnector: int, fullAdvance: int, isExtender: bool) =
    member _.Glyph = glyph
    /// How far the part may overlap its predecessor.
    member _.StartConnector = startConnector
    /// How far the part may overlap its successor.
    member _.EndConnector = endConnector
    member _.FullAdvance = fullAdvance
    /// Repeatable, so that any length can be reached.
    member _.IsExtender = isExtender

/// How one glyph is enlarged along an axis: by larger variants, then by assembly from parts.
and [<Struct>] Stretch internal (table: StretchTable, index: int) =
    /// Sizes in increasing order, the first being the unstretched glyph.
    member _.VariantCount = table.VariantStarts.[index + 1] - table.VariantStarts.[index]

    member _.Variant(i: int) =
        let at = table.VariantStarts.[index] + i
        StretchVariant(Glyph table.VariantGlyphs.[at], table.VariantAdvances.[at])

    /// Zero where the font gives no assembly, the variants then being the only sizes available.
    member _.PartCount = table.PartStarts.[index + 1] - table.PartStarts.[index]

    member _.Part(i: int) =
        let at = table.PartStarts.[index] + i
        StretchPart(
            Glyph table.PartGlyphs.[at],
            table.PartStartConnectors.[at],
            table.PartEndConnectors.[at],
            table.PartFullAdvances.[at],
            table.PartExtenders.[at] = 1)

    member _.AssemblyItalicCorrection = table.AssemblyItalicsCorrections.[index]

[<AbstractClass; Sealed>]
type MathFont =
    /// ValueNone where the font has no glyph for the codepoint.
    static member OfCodepoint(codepoint: int) =
        Lookup.value(MathFontData.codepoints, MathFontData.codepointGlyphs, codepoint)
        |> ValueOption.map Glyph

    static member OfChar(c: char) = MathFont.OfCodepoint(int c)

    /// The font file the metrics were generated from, whose glyph ids Glyph.Id refers to.
    static member OpenFontFile(): Stream =
        typeof<Glyph>.Assembly.GetManifestResourceStream "ModeMath.latinmodern-math.otf"
