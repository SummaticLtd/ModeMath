module ModeMath.Tests.MathFontTests

open System
open System.IO
open System.Security.Cryptography
open SimpleTests
open ModeMath

let private glyph(c: char) =
    match MathFont.OfChar c with
    | ValueSome g -> g
    | ValueNone -> failwith $"the font has no glyph for {c}"

let private verticalStretch(codepoint: int) =
    match MathFont.OfCodepoint codepoint with
    | ValueNone -> failwith $"the font has no glyph for U+{codepoint:X4}"
    | ValueSome g ->
        match g.VerticalStretch with
        | ValueSome stretch -> stretch
        | ValueNone -> failwith $"U+{codepoint:X4} does not stretch vertically"

let private isSorted(name: string, values: int array) =
    for i in 1 .. values.Length - 1 do
        Assert.True(values.[i - 1] < values.[i], $"{name} is not sorted and unique at index {i}")

let private inGlyphRange(name: string, values: int array) =
    for value in values do
        Assert.True(
            value >= 0 && value < MathFontData.glyphCount,
            $"{name} holds {value}, which is not a glyph id")

let private perGlyph =
    [
        "advances", MathFontData.advances
        "boundsLeft", MathFontData.boundsLeft
        "boundsRight", MathFontData.boundsRight
        "boundsTop", MathFontData.boundsTop
        "boundsBottom", MathFontData.boundsBottom
    ]

let private coverages =
    [
        "codepoints", MathFontData.codepoints
        "italicsCorrectionGlyphs", MathFontData.italicsCorrectionGlyphs
        "topAccentGlyphs", MathFontData.topAccentGlyphs
        "extendedShapeGlyphs", MathFontData.extendedShapeGlyphs
        "verticalGlyphs", MathFontData.verticalGlyphs
        "horizontalGlyphs", MathFontData.horizontalGlyphs
    ]

let private pairs =
    [
        "codepoints", MathFontData.codepoints, MathFontData.codepointGlyphs
        "italicsCorrections", MathFontData.italicsCorrectionGlyphs, MathFontData.italicsCorrections
        "topAccentAttachments", MathFontData.topAccentGlyphs, MathFontData.topAccentAttachments
        "verticalVariants", MathFontData.verticalVariantGlyphs, MathFontData.verticalVariantAdvances
        "horizontalVariants", MathFontData.horizontalVariantGlyphs, MathFontData.horizontalVariantAdvances
    ]

let private glyphIdArrays =
    [
        "codepointGlyphs", MathFontData.codepointGlyphs
        "italicsCorrectionGlyphs", MathFontData.italicsCorrectionGlyphs
        "topAccentGlyphs", MathFontData.topAccentGlyphs
        "extendedShapeGlyphs", MathFontData.extendedShapeGlyphs
        "verticalGlyphs", MathFontData.verticalGlyphs
        "verticalVariantGlyphs", MathFontData.verticalVariantGlyphs
        "verticalPartGlyphs", MathFontData.verticalPartGlyphs
        "horizontalGlyphs", MathFontData.horizontalGlyphs
        "horizontalVariantGlyphs", MathFontData.horizontalVariantGlyphs
        "horizontalPartGlyphs", MathFontData.horizontalPartGlyphs
    ]

/// Index arrays that partition a flattened array, one entry per owner plus a final total.
let private partitions =
    [
        "verticalVariantStarts", MathFontData.verticalVariantStarts, MathFontData.verticalGlyphs.Length,
            MathFontData.verticalVariantGlyphs.Length
        "verticalPartStarts", MathFontData.verticalPartStarts, MathFontData.verticalGlyphs.Length,
            MathFontData.verticalPartGlyphs.Length
        "horizontalVariantStarts", MathFontData.horizontalVariantStarts, MathFontData.horizontalGlyphs.Length,
            MathFontData.horizontalVariantGlyphs.Length
        "horizontalPartStarts", MathFontData.horizontalPartStarts, MathFontData.horizontalGlyphs.Length,
            MathFontData.horizontalPartGlyphs.Length
    ]

let private data =
    TestList(
        "Data",
        [   Test.CasesSync(
                "perGlyphArraysCoverEveryGlyph",
                perGlyph |> List.map (fun (name, values) -> name, (name, values)),
                fun (name, values) -> Assert.Equal(MathFontData.glyphCount, values.Length, name)
            )
            Test.CasesSync(
                "coverageArraysAreSortedAndUnique",
                coverages |> List.map (fun (name, values) -> name, (name, values)),
                isSorted
            )
            Test.CasesSync(
                "pairedArraysHaveMatchingLengths",
                pairs |> List.map (fun (name, keys, values) -> name, (name, keys, values)),
                fun (name, keys, values) -> Assert.Equal(keys.Length, values.Length, name)
            )
            Test.CasesSync(
                "glyphIdsAreInRange",
                glyphIdArrays |> List.map (fun (name, values) -> name, (name, values)),
                inGlyphRange
            )
            Test.CasesSync(
                "partitionsAreMonotonicAndComplete",
                partitions |> List.map (fun (name, starts, owners, total) -> name, (name, starts, owners, total)),
                fun (name, starts, owners, total) ->
                    Assert.Equal(owners + 1, starts.Length, $"{name} length")
                    Assert.Equal(0, starts.[0], $"{name} start")
                    Assert.Equal(total, starts.[starts.Length - 1], $"{name} total")
                    for i in 1 .. starts.Length - 1 do
                        Assert.True(starts.[i - 1] <= starts.[i], $"{name} decreases at index {i}")
            )
            Test.Sync(
                "embeddedFontIsTheOneTheMetricsCameFrom",
                fun () ->
                    use stream = MathFont.OpenFontFile()
                    use copy = new MemoryStream()
                    stream.CopyTo copy
                    let bytes = copy.ToArray()
                    Assert.Equal(MathFontData.fontByteLength, bytes.Length, "font byte length")
                    Assert.Equal(
                        MathFontData.fontSha256,
                        Convert.ToHexStringLower(SHA256.HashData bytes),
                        "font SHA-256")
            )
        ]
    )

let private metrics =
    TestList(
        "Metrics",
        [   Test.Sync(
                "constantsComeFromLatinModernMath",
                fun () ->
                    Assert.Equal(1000, MathConstants.UnitsPerEm)
                    Assert.Equal(250, MathConstants.AxisHeight)
                    Assert.Equal(40, MathConstants.FractionRuleThickness)
                    Assert.Equal(70, MathConstants.ScriptPercentScaleDown)
            )
            Test.Sync(
                "unmappedCodepointHasNoGlyph",
                fun () -> Assert.Equal(ValueNone, MathFont.OfCodepoint 0xE000)
            )
            Test.CasesSync(
                "commonCharactersHaveGlyphs",
                [ for c in "xy0123456789()[]+=" -> string c, c ],
                fun c -> Assert.True((MathFont.OfChar c).IsSome, $"no glyph for {c}")
            )
            Test.Sync(
                "digitsShareOneAdvance",
                fun () ->
                    let advance = (glyph '0').Advance
                    for c in "123456789" do
                        Assert.Equal(advance, (glyph c).Advance, $"advance of {c}")
            )
            Test.Sync(
                "boundsOfXLieAboveTheBaseline",
                fun () ->
                    let x = glyph 'x'
                    Assert.True(x.Advance > 0, "advance")
                    Assert.True(x.Left < x.Right, "horizontal bounds")
                    Assert.True(x.Top > 0, "top above the baseline")
                    Assert.True(x.Bottom <= 0, "bottom on or below the baseline")
                    Assert.True(x.Top < MathConstants.UnitsPerEm, "top within the em")
            )
            Test.Sync(
                "italicFLeansPastItsAdvance",
                fun () ->
                    match MathFont.OfCodepoint 0x1D453 with
                    | ValueNone -> Assert.Fail "the font has no italic f"
                    | ValueSome f -> Assert.True(f.ItalicCorrection > 0, "italic correction")
            )
            Test.Sync(
                "asciiLettersAreNotTheItalicOnes",
                fun () ->
                    match MathFont.OfCodepoint 0x1D465 with
                    | ValueNone -> Assert.Fail "the font has no italic x"
                    | ValueSome italicX -> Assert.True((glyph 'x').Id <> italicX.Id, "same glyph")
            )
            Test.Sync(
                "digitsHaveNoItalicCorrection",
                fun () -> Assert.Equal(0, (glyph '0').ItalicCorrection)
            )
            Test.CasesSync(
                "delimitersAndRadicalStretchVertically",
                [ "(", int '('; ")", int ')'; "[", int '['; "|", int '|'; "sqrt", 0x221A ],
                fun codepoint ->
                    let stretch = verticalStretch codepoint
                    Assert.True(stretch.VariantCount > 1, "variant count")
                    let mutable previous = 0
                    for i in 0 .. stretch.VariantCount - 1 do
                        let variant = stretch.Variant i
                        Assert.True(variant.Advance > previous, $"variant {i} is no taller than variant {i - 1}")
                        previous <- variant.Advance
            )
            Test.Sync(
                "theParenthesisAssemblyHasARepeatablePart",
                fun () ->
                    let stretch = verticalStretch(int '(')
                    Assert.True(stretch.PartCount > 2, "part count")
                    let mutable extenders = 0
                    for i in 0 .. stretch.PartCount - 1 do
                        if (stretch.Part i).IsExtender then extenders <- extenders + 1
                    Assert.True(extenders > 0, "no extender among the parts")
            )
            Test.Sync(
                "theSmallestVariantIsTheUnstretchedGlyph",
                fun () ->
                    let stretch = verticalStretch(int '(')
                    Assert.Equal((glyph '(').Id, (stretch.Variant 0).Glyph.Id)
            )
            Test.Sync(
                "tallDelimitersAreExtendedShapes",
                fun () ->
                    let stretch = verticalStretch(int '(')
                    let tallest = stretch.Variant(stretch.VariantCount - 1)
                    Assert.True(tallest.Glyph.IsExtendedShape, "tallest parenthesis is not an extended shape")
                    Assert.True(not (glyph 'x').IsExtendedShape, "x is an extended shape")
            )
        ]
    )

let tests = TestFolder("MathFont", [ data; metrics ])
