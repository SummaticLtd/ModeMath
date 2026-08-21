namespace MathTableGen

open System.Collections.Generic

/// Big-endian cursor over the bytes of one font table.
type internal Reader(data: byte array) =
    let mutable position = 0

    member _.Position
        with get () = position
        and set (value: int) = position <- value

    member _.UInt16At(offset: int) = (int data.[offset] <<< 8) ||| int data.[offset + 1]

    member t.Int16At(offset: int) =
        let value = t.UInt16At offset
        if value >= 0x8000 then value - 0x10000 else value

    member t.ReadUInt16() =
        let value = t.UInt16At position
        position <- position + 2
        value

    member t.ReadInt16() =
        let value = t.Int16At position
        position <- position + 2
        value

    /// A MathValueRecord, whose device table is for hinting and is dropped.
    member t.ReadMathValue() =
        let value = t.ReadInt16()
        position <- position + 2
        value

    /// The glyph ids of a coverage table, in coverage order.
    member t.Coverage(offset: int): int array =
        let count = t.UInt16At(offset + 2)
        match t.UInt16At offset with
        | 1 -> Array.init count (fun i -> t.UInt16At(offset + 4 + i * 2))
        | 2 ->
            let glyphs = List<int>()
            for i in 0 .. count - 1 do
                let record = offset + 4 + i * 6
                for glyph in t.UInt16At record .. t.UInt16At(record + 2) do
                    glyphs.Add glyph
            glyphs.ToArray()
        | format -> failwith $"Unknown coverage format {format}"

/// The MathConstants, in table order.
module MathConstantNames =
    /// The constants that are ratios rather than lengths, which carry no design unit.
    let percentages =
        set [ "ScriptPercentScaleDown"; "ScriptScriptPercentScaleDown"; "RadicalDegreeBottomRaisePercent" ]

    let table =
        [|
            "ScriptPercentScaleDown"
            "ScriptScriptPercentScaleDown"
            "DelimitedSubFormulaMinHeight"
            "DisplayOperatorMinHeight"
            "MathLeading"
            "AxisHeight"
            "AccentBaseHeight"
            "FlattenedAccentBaseHeight"
            "SubscriptShiftDown"
            "SubscriptTopMax"
            "SubscriptBaselineDropMin"
            "SuperscriptShiftUp"
            "SuperscriptShiftUpCramped"
            "SuperscriptBottomMin"
            "SuperscriptBaselineDropMax"
            "SubSuperscriptGapMin"
            "SuperscriptBottomMaxWithSubscript"
            "SpaceAfterScript"
            "UpperLimitGapMin"
            "UpperLimitBaselineRiseMin"
            "LowerLimitGapMin"
            "LowerLimitBaselineDropMin"
            "StackTopShiftUp"
            "StackTopDisplayStyleShiftUp"
            "StackBottomShiftDown"
            "StackBottomDisplayStyleShiftDown"
            "StackGapMin"
            "StackDisplayStyleGapMin"
            "StretchStackTopShiftUp"
            "StretchStackBottomShiftDown"
            "StretchStackGapAboveMin"
            "StretchStackGapBelowMin"
            "FractionNumeratorShiftUp"
            "FractionNumeratorDisplayStyleShiftUp"
            "FractionDenominatorShiftDown"
            "FractionDenominatorDisplayStyleShiftDown"
            "FractionNumeratorGapMin"
            "FractionNumDisplayStyleGapMin"
            "FractionRuleThickness"
            "FractionDenominatorGapMin"
            "FractionDenomDisplayStyleGapMin"
            "SkewedFractionHorizontalGap"
            "SkewedFractionVerticalGap"
            "OverbarVerticalGap"
            "OverbarRuleThickness"
            "OverbarExtraAscender"
            "UnderbarVerticalGap"
            "UnderbarRuleThickness"
            "UnderbarExtraDescender"
            "RadicalVerticalGap"
            "RadicalDisplayStyleVerticalGap"
            "RadicalRuleThickness"
            "RadicalExtraAscender"
            "RadicalKernBeforeDegree"
            "RadicalKernAfterDegree"
            "RadicalDegreeBottomRaisePercent"
        |]

type GlyphVariant(glyph: int, advance: int) =
    member _.Glyph = glyph
    /// Height for a vertical variant, width for a horizontal one.
    member _.Advance = advance

type GlyphPart(glyph: int, startConnector: int, endConnector: int, fullAdvance: int, isExtender: bool) =
    member _.Glyph = glyph
    member _.StartConnector = startConnector
    member _.EndConnector = endConnector
    member _.FullAdvance = fullAdvance
    member _.IsExtender = isExtender

type GlyphAssembly(italicsCorrection: int, parts: GlyphPart array) =
    member _.ItalicsCorrection = italicsCorrection
    member _.Parts = parts

/// How one glyph stretches: by substitution with a larger variant, or by assembly from parts.
type GlyphConstruction(glyph: int, variants: GlyphVariant array, assembly: GlyphAssembly voption) =
    member _.Glyph = glyph
    member _.Variants = variants
    member _.Assembly = assembly

type MathTable(data: byte array) =
    let r = Reader data

    let constantsOffset = r.UInt16At 4
    let glyphInfoOffset = r.UInt16At 6
    let variantsOffset = r.UInt16At 8

    let constants =
        r.Position <- constantsOffset
        let values = Array.zeroCreate<int> MathConstantNames.table.Length
        values.[0] <- r.ReadInt16()
        values.[1] <- r.ReadInt16()
        values.[2] <- r.ReadUInt16()
        values.[3] <- r.ReadUInt16()
        for i in 4 .. 54 do
            values.[i] <- r.ReadMathValue()
        values.[55] <- r.ReadInt16()
        values

    /// A coverage table paired with its parallel array of MathValueRecords.
    let coveredValues(tableOffset: int): (int * int) array =
        let coverage = r.Coverage(tableOffset + r.UInt16At tableOffset)
        let count = r.UInt16At(tableOffset + 2)
        r.Position <- tableOffset + 4
        let pairs = Array.zeroCreate<int * int> count
        for i in 0 .. count - 1 do
            pairs.[i] <- coverage.[i], r.ReadMathValue()
        pairs

    let italicsCorrections = coveredValues(glyphInfoOffset + r.UInt16At glyphInfoOffset)
    let topAccentAttachments = coveredValues(glyphInfoOffset + r.UInt16At(glyphInfoOffset + 2))

    let extendedShapes =
        match r.UInt16At(glyphInfoOffset + 4) with
        | 0 -> Array.empty
        | offset -> r.Coverage(glyphInfoOffset + offset)

    let kernedGlyphCount =
        match r.UInt16At(glyphInfoOffset + 6) with
        | 0 -> 0
        | offset -> r.UInt16At(glyphInfoOffset + offset + 2)

    let assembly(offset: int) =
        r.Position <- offset
        let italicsCorrection = r.ReadMathValue()
        let partCount = r.ReadUInt16()
        let parts = Array.zeroCreate<GlyphPart> partCount
        for i in 0 .. partCount - 1 do
            let glyph = r.ReadUInt16()
            let startConnector = r.ReadUInt16()
            let endConnector = r.ReadUInt16()
            let fullAdvance = r.ReadUInt16()
            parts.[i] <- GlyphPart(glyph, startConnector, endConnector, fullAdvance, r.ReadUInt16() &&& 1 = 1)
        GlyphAssembly(italicsCorrection, parts)

    let construction(glyph: int, offset: int) =
        let assemblyOffset = r.UInt16At offset
        let variantCount = r.UInt16At(offset + 2)
        r.Position <- offset + 4
        let variants = Array.zeroCreate<GlyphVariant> variantCount
        for i in 0 .. variantCount - 1 do
            let variantGlyph = r.ReadUInt16()
            variants.[i] <- GlyphVariant(variantGlyph, r.ReadUInt16())
        GlyphConstruction(
            glyph,
            variants,
            (if assemblyOffset = 0 then ValueNone else ValueSome(assembly(offset + assemblyOffset))))

    let constructions(coverageOffset: int, count: int, firstOffset: int) =
        let coverage = r.Coverage(variantsOffset + coverageOffset)
        Array.init count (fun i ->
            construction(coverage.[i], variantsOffset + r.UInt16At(firstOffset + i * 2)))

    let verticalCount = r.UInt16At(variantsOffset + 6)
    let horizontalCount = r.UInt16At(variantsOffset + 8)

    let verticalConstructions =
        constructions(r.UInt16At(variantsOffset + 2), verticalCount, variantsOffset + 10)

    let horizontalConstructions =
        constructions(
            r.UInt16At(variantsOffset + 4),
            horizontalCount,
            variantsOffset + 10 + verticalCount * 2)

    member _.Constants = constants
    member _.ItalicsCorrections = italicsCorrections
    member _.TopAccentAttachments = topAccentAttachments
    member _.ExtendedShapes = extendedShapes
    member _.KernedGlyphCount = kernedGlyphCount
    member _.MinConnectorOverlap = r.UInt16At variantsOffset
    member _.VerticalConstructions = verticalConstructions
    member _.HorizontalConstructions = horizontalConstructions
