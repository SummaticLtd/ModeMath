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

let private horizontalStretch(codepoint: int) =
    match MathFont.OfCodepoint codepoint with
    | ValueNone -> failwith $"the font has no glyph for U+{codepoint:X4}"
    | ValueSome g ->
        match g.HorizontalStretch with
        | ValueSome stretch -> stretch
        | ValueNone -> failwith $"U+{codepoint:X4} does not stretch horizontally"

let private sortedKeys(name: string, count: int, keyAt: int -> int) =
    for i in 1 .. count - 1 do
        Assert.True(keyAt (i - 1) < keyAt i, $"{name} is not sorted and unique at index {i}")

let private glyphIds(name: string, count: int, glyphAt: int -> int) =
    for i in 0 .. count - 1 do
        let glyph = glyphAt i
        Assert.True(
            glyph >= 0 && glyph < MathFontData.glyphCount,
            $"{name} holds {glyph} at index {i}, which is not a glyph id")

/// Every array whose entries are keyed by a glyph id or a codepoint, in ascending order.
let private keyed =
    [
        "codepointGlyphs", MathFontData.codepointGlyphs.Length, fun i -> MathFontData.codepointGlyphs.[i].Codepoint
        "italicCorrections", MathFontData.italicCorrections.Length, fun i -> MathFontData.italicCorrections.[i].Glyph
        "topAccents", MathFontData.topAccents.Length, fun i -> MathFontData.topAccents.[i].Glyph
        "extendedShapeGlyphs", MathFontData.extendedShapeGlyphs.Length, fun i -> MathFontData.extendedShapeGlyphs.[i]
        "verticalConstructions", MathFontData.verticalConstructions.Length,
            fun i -> MathFontData.verticalConstructions.[i].Glyph
        "horizontalConstructions", MathFontData.horizontalConstructions.Length,
            fun i -> MathFontData.horizontalConstructions.[i].Glyph
    ]

/// Every array whose entries name a glyph to draw.
let private drawn =
    [
        "codepointGlyphs", MathFontData.codepointGlyphs.Length, fun i -> MathFontData.codepointGlyphs.[i].Glyph
        "italicCorrections", MathFontData.italicCorrections.Length, fun i -> MathFontData.italicCorrections.[i].Glyph
        "topAccents", MathFontData.topAccents.Length, fun i -> MathFontData.topAccents.[i].Glyph
        "extendedShapeGlyphs", MathFontData.extendedShapeGlyphs.Length, fun i -> MathFontData.extendedShapeGlyphs.[i]
        "verticalSizes", MathFontData.verticalSizes.Length, fun i -> MathFontData.verticalSizes.[i].Glyph
        "verticalParts", MathFontData.verticalParts.Length, fun i -> MathFontData.verticalParts.[i].Glyph
        "horizontalSizes", MathFontData.horizontalSizes.Length, fun i -> MathFontData.horizontalSizes.[i].Glyph
        "horizontalParts", MathFontData.horizontalParts.Length, fun i -> MathFontData.horizontalParts.[i].Glyph
    ]

/// Each axis, with the arrays its constructions slice.
let private axes =
    [
        "vertical", MathFontData.verticalConstructions, MathFontData.verticalSizes.Length,
            MathFontData.verticalParts.Length
        "horizontal", MathFontData.horizontalConstructions, MathFontData.horizontalSizes.Length,
            MathFontData.horizontalParts.Length
    ]

let private data =
    TestList(
        "Data",
        [   Test.Sync(
                "everyGlyphHasMetrics",
                fun () -> Assert.Equal(MathFontData.glyphCount, MathFontData.glyphMetrics.Length)
            )
            Test.CasesSync(
                "keysAreSortedAndUnique",
                keyed |> List.map (fun (name, count, keyAt) -> name, (name, count, keyAt)),
                sortedKeys
            )
            Test.CasesSync(
                "glyphIdsAreInRange",
                drawn |> List.map (fun (name, count, glyphAt) -> name, (name, count, glyphAt)),
                glyphIds
            )
            Test.CasesSync(
                "everySliceLiesWithinItsArray",
                axes |> List.map (fun (name, constructions, sizes, parts) -> name, (name, constructions, sizes, parts)),
                fun (name, constructions: StretchConstruction array, sizes, parts) ->
                    for i in 0 .. constructions.Length - 1 do
                        let construction = constructions.[i]
                        Assert.True(
                            construction.SizeCount > 0 || construction.PartCount > 0,
                            $"{name} {i} offers neither a size nor an assembly")
                        Assert.True(
                            construction.SizeStart >= 0 && construction.SizeStart + construction.SizeCount <= sizes,
                            $"{name} {i} slices sizes out of range")
                        Assert.True(
                            construction.PartStart >= 0 && construction.PartStart + construction.PartCount <= parts,
                            $"{name} {i} slices parts out of range")
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
                [ for c in "xy0123456789()[]+=" do yield string c, c ],
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
            Test.CasesSync(
                "bracesAndArrowsStretchHorizontally",
                [ "overbrace", 0x23DE; "underbrace", 0x23DF; "leftarrow", 0x2190; "widehat", 0x0302 ],
                fun codepoint ->
                    let stretch = horizontalStretch codepoint
                    Assert.True(stretch.VariantCount > 1, "variant count")
                    let mutable previous = 0
                    for i in 0 .. stretch.VariantCount - 1 do
                        let variant = stretch.Variant i
                        Assert.True(variant.Advance > previous, $"variant {i} is no wider than variant {i - 1}")
                        previous <- variant.Advance
            )
            Test.Sync(
                "theOverbraceAssemblyHasARepeatablePart",
                fun () ->
                    let stretch = horizontalStretch 0x23DE
                    Assert.True(stretch.PartCount > 2, "part count")
                    let mutable extenders = 0
                    for i in 0 .. stretch.PartCount - 1 do
                        if (stretch.Part i).IsExtender then extenders <- extenders + 1
                    Assert.True(extenders > 0, "no extender among the parts")
            )
            Test.Sync(
                "aLetterStretchesNeitherWay",
                fun () ->
                    let x = glyph 'x'
                    Assert.True(x.VerticalStretch.IsNone, "x stretches vertically")
                    Assert.True(x.HorizontalStretch.IsNone, "x stretches horizontally")
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
