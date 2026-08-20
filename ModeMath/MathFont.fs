namespace ModeMath

open System
open System.IO

module private Lookup =
    /// The position of a key in a sorted array, or -1. Inline, so that keyAt costs no closure.
    let inline search(count: int, key: int, keyAt: int -> int) =
        let mutable low = 0
        let mutable high = count - 1
        let mutable found = -1
        while found < 0 && low <= high do
            let middle = low + (high - low) / 2
            let candidate = keyAt middle
            if candidate = key then found <- middle
            elif candidate < key then low <- middle + 1
            else high <- middle - 1
        found

/// One axis along which glyphs stretch, as the three arrays generated for it.
type internal StretchTable
    (
        constructions: StretchConstruction array,
        sizes: StretchSize array,
        parts: AssemblyPart array
    ) =
    member _.Constructions = constructions
    member _.Sizes = sizes
    member _.Parts = parts

    /// The construction for a glyph, or -1.
    member _.IndexOf(glyph: int) =
        Lookup.search(constructions.Length, glyph, fun i -> constructions.[i].Glyph)

    static member val Vertical =
        StretchTable(
            MathFontData.verticalConstructions,
            MathFontData.verticalSizes,
            MathFontData.verticalParts)

    static member val Horizontal =
        StretchTable(
            MathFontData.horizontalConstructions,
            MathFontData.horizontalSizes,
            MathFontData.horizontalParts)

/// A glyph of the math font, measured in design units from the origin on the baseline, y upwards.
[<Struct>]
type Glyph internal (id: int) =
    member _.Id = id
    member _.Advance = MathFontData.glyphMetrics.[id].Advance
    member _.Left = MathFontData.glyphMetrics.[id].Left
    member _.Right = MathFontData.glyphMetrics.[id].Right
    member _.Top = MathFontData.glyphMetrics.[id].Top
    member _.Bottom = MathFontData.glyphMetrics.[id].Bottom

    /// Zero where the font gives none.
    member _.ItalicCorrection =
        let corrections = MathFontData.italicCorrections
        match Lookup.search(corrections.Length, id, fun i -> corrections.[i].Glyph) with
        | -1 -> 0
        | found -> corrections.[found].Correction

    /// Where an accent sits over this glyph. ValueNone where the font gives none, meaning the accent is centred.
    member _.TopAccentAttachment =
        let accents = MathFontData.topAccents
        match Lookup.search(accents.Length, id, fun i -> accents.[i].Glyph) with
        | -1 -> ValueNone
        | found -> ValueSome accents.[found].Attachment

    /// Tall enough that a following superscript is not raised to clear it.
    member _.IsExtendedShape =
        let shapes = MathFontData.extendedShapeGlyphs
        Lookup.search(shapes.Length, id, fun i -> shapes.[i]) >= 0

    /// How this glyph grows taller, if it does.
    member _.VerticalStretch: Stretch voption =
        match StretchTable.Vertical.IndexOf id with
        | -1 -> ValueNone
        | found -> ValueSome(Stretch(StretchTable.Vertical, found))

    /// How this glyph grows wider, if it does.
    member _.HorizontalStretch: Stretch voption =
        match StretchTable.Horizontal.IndexOf id with
        | -1 -> ValueNone
        | found -> ValueSome(Stretch(StretchTable.Horizontal, found))

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
    member private _.Construction = table.Constructions.[index]

    /// Sizes in increasing order, the first being the unstretched glyph.
    member t.VariantCount = t.Construction.SizeCount

    member t.Variant(i: int) =
        let size = table.Sizes.[t.Construction.SizeStart + i]
        StretchVariant(Glyph size.Glyph, size.Advance)

    /// Zero where the font gives no assembly, the variants then being the only sizes available.
    member t.PartCount = t.Construction.PartCount

    member t.Part(i: int) =
        let part = table.Parts.[t.Construction.PartStart + i]
        StretchPart(
            Glyph part.Glyph,
            part.StartConnector,
            part.EndConnector,
            part.FullAdvance,
            part.IsExtender)

    member t.AssemblyItalicCorrection = t.Construction.ItalicCorrection

[<AbstractClass; Sealed>]
type MathFont =
    /// ValueNone where the font has no glyph for the codepoint.
    static member OfCodepoint(codepoint: int) =
        let mapped = MathFontData.codepointGlyphs
        match Lookup.search(mapped.Length, codepoint, fun i -> mapped.[i].Codepoint) with
        | -1 -> ValueNone
        | found -> ValueSome(Glyph mapped.[found].Glyph)

    static member OfChar(c: char) = MathFont.OfCodepoint(int c)

    /// The font file the metrics were generated from, whose glyph ids Glyph.Id refers to.
    static member OpenFontFile(): Stream =
        typeof<Glyph>.Assembly.GetManifestResourceStream "ModeMath.latinmodern-math.otf"

/// A glyph that grows along an axis, so that callers need not ask whether it does.
[<Struct>]
type StretchyGlyph internal (glyph: Glyph, stretch: Stretch) =
    /// The unstretched glyph, whose metrics apply until it is grown.
    member _.Glyph = glyph
    /// Sizes in increasing order, the first being the unstretched glyph where the font offers one.
    member _.SizeCount = stretch.VariantCount
    member _.Size(i: int) = stretch.Variant i
    /// Zero where the font gives no assembly, the sizes then being the only ones available.
    member _.PartCount = stretch.PartCount
    member _.Part(i: int) = stretch.Part i

module private Resolve =
    let glyph(codepoint: int) =
        match MathFont.OfCodepoint codepoint with
        | ValueSome found -> found
        | ValueNone -> failwith $"the font has no glyph for U+{codepoint:X4}"

    let vertical(codepoint: int) =
        let found = glyph codepoint
        match found.VerticalStretch with
        | ValueSome stretch -> StretchyGlyph(found, stretch)
        | ValueNone -> failwith $"U+{codepoint:X4} does not stretch vertically"

/// The alphabets a variable is set in. ValueNone means not a letter, never a gap in the font.
module Letters =
    /// Italic h is unassigned in its block and lives among the letterlike symbols instead.
    let private italicSmall =
        Array.init 26 (fun i -> Resolve.glyph(if i = int 'h' - int 'a' then 0x210E else 0x1D44E + i))

    let private italicCapital = Array.init 26 (fun i -> Resolve.glyph(0x1D434 + i))
    let private boldSmall = Array.init 26 (fun i -> Resolve.glyph(0x1D482 + i))
    let private boldCapital = Array.init 26 (fun i -> Resolve.glyph(0x1D468 + i))
    let private italicGreek = Array.init 25 (fun i -> Resolve.glyph(0x1D6FC + i))

    /// The shapes that Unicode keeps outside the alphabet, which follow omega in the italic block.
    let private italicShapes =
        dict [
            '∂', Resolve.glyph 0x1D715
            'ϵ', Resolve.glyph 0x1D716
            'ϑ', Resolve.glyph 0x1D717
            'ϰ', Resolve.glyph 0x1D718
            'ϕ', Resolve.glyph 0x1D719
            'ϱ', Resolve.glyph 0x1D71A
            'ϖ', Resolve.glyph 0x1D71B
        ]

    /// Math italic, in which variables are set. Capital Greek is absent, being set upright.
    let italic(c: char) =
        if c >= 'a' && c <= 'z' then ValueSome italicSmall.[int c - int 'a']
        elif c >= 'A' && c <= 'Z' then ValueSome italicCapital.[int c - int 'A']
        elif c >= 'α' && c <= 'ω' then ValueSome italicGreek.[int c - 0x03B1]
        else
            match italicShapes.TryGetValue c with
            | true, glyph -> ValueSome glyph
            | _ -> ValueNone

    /// Math bold italic, in which bold variables are set.
    let bold(c: char) =
        if c >= 'a' && c <= 'z' then ValueSome boldSmall.[int c - int 'a']
        elif c >= 'A' && c <= 'Z' then ValueSome boldCapital.[int c - int 'A']
        else ValueNone

/// Operators whose codepoints are easy to mistake for the keys that resemble them.
module Operators =
    let plus = Resolve.glyph(int '+')
    /// The minus sign, which is not the hyphen.
    let minus = Resolve.glyph 0x2212
    let times = Resolve.glyph 0x00D7
    let divide = Resolve.glyph 0x00F7
    let equals = Resolve.glyph(int '=')
    /// The centred dot of a product.
    let cdot = Resolve.glyph 0x22C5

/// Delimiters, which grow to the height of what they hold.
module Delimiters =
    let roundLeft = Resolve.vertical(int '(')
    let roundRight = Resolve.vertical(int ')')
    /// The vertical bar of an absolute value, used on both sides.
    let bar = Resolve.vertical(int '|')

module Radicals =
    /// The tick and bar of a root, which grows to cover the radicand.
    let surd = Resolve.vertical 0x221A
