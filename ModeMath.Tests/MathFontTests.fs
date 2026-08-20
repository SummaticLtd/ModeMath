module ModeMath.Tests.MathFontTests

open System
open System.IO
open System.Security.Cryptography
open SimpleTests
open ModeMath

let private glyph(c: char) =
    match MathFont.OfChar c with
    | ValueSome g -> g
    | ValueNone -> failwith $"the repertoire has no {c}"

let private digit(c: char) =
    match Digits.glyph c with
    | ValueSome g -> g
    | ValueNone -> failwith $"no digit {c}"

let private upright(c: char) =
    match Letters.upright c with
    | ValueSome g -> g
    | ValueNone -> failwith $"no upright {c}"

let private data =
    TestList(
        "Data",
        [   Test.Sync(
                "theEmbeddedFontIsTheOneTheMetricsCameFrom",
                fun () ->
                    use stream = MathFont.OpenFontFile()
                    use copy = new MemoryStream()
                    stream.CopyTo copy
                    let bytes = copy.ToArray()
                    Assert.Equal(FontFile.byteLength, bytes.Length, "font byte length")
                    Assert.Equal(
                        FontFile.sha256,
                        Convert.ToHexStringLower(SHA256.HashData bytes),
                        "font SHA-256")
            )
            Test.Sync(
                "theRepertoireIsSortedAndUnique",
                fun () ->
                    let all = Repertoire.all
                    for i in 1 .. all.Length - 1 do
                        Assert.True(
                            all.[i - 1].Codepoint < all.[i].Codepoint,
                            $"codepoints are not sorted and unique at index {i}")
            )
            Test.Sync(
                "constantsComeFromLatinModernMath",
                fun () ->
                    Assert.Equal(1000, MathConstants.UnitsPerEm)
                    Assert.Equal(250, MathConstants.AxisHeight)
                    Assert.Equal(40, MathConstants.FractionRuleThickness)
                    Assert.Equal(70, MathConstants.ScriptPercentScaleDown)
            )
        ]
    )

let private metrics =
    TestList(
        "Metrics",
        [   Test.Sync(
                "aCharacterOutsideTheRepertoireHasNoGlyph",
                fun () -> Assert.Equal(ValueNone, MathFont.OfChar '漢')
            )
            Test.CasesSync(
                "commonPunctuationIsInTheRepertoire",
                [ for c in "()[]+=<>,;:!?/" do yield string c, c ],
                fun c -> Assert.True((MathFont.OfChar c).IsSome, $"no glyph for {c}")
            )
            Test.Sync(
                "digitsShareOneAdvance",
                fun () ->
                    let advance = (digit '0').Advance
                    for c in "123456789" do
                        Assert.Equal(advance, (digit c).Advance, $"advance of {c}")
            )
            Test.Sync(
                "theInkOfXSitsOnTheBaseline",
                fun () ->
                    let x = upright 'x'
                    Assert.True(x.Advance > 0, "advance")
                    Assert.True(x.Top > 0, "top above the baseline")
                    Assert.True(x.Bottom <= 0, "bottom on or below the baseline")
                    Assert.True(x.Top < MathConstants.UnitsPerEm, "top within the em")
            )
            Test.Sync(
                "italicFLeansPastItsAdvance",
                fun () ->
                    match Letters.italic 'f' with
                    | ValueNone -> Assert.Fail "the font has no italic f"
                    | ValueSome f -> Assert.True(f.ItalicCorrection > 0, "italic correction")
            )
            Test.Sync(
                "digitsHaveNoItalicCorrection",
                fun () -> Assert.Equal(0, (digit '0').ItalicCorrection)
            )
        ]
    )

let private named =
    TestList(
        "Named",
        [   Test.CasesSync(
                "everyLetterOfEveryAlphabetIsPresent",
                [   "italic latin small", [ 'a' .. 'z' ], Letters.italic
                    "italic latin capital", [ 'A' .. 'Z' ], Letters.italic
                    "italic greek", [ for c in 'α' .. 'ω' -> c ], Letters.italic
                    "italic shapes", [ '∂'; 'ϵ'; 'ϑ'; 'ϰ'; 'ϕ'; 'ϱ'; 'ϖ' ], Letters.italic
                    "bold latin small", [ 'a' .. 'z' ], Letters.bold
                    "bold latin capital", [ 'A' .. 'Z' ], Letters.bold ]
                |> List.map (fun (name, letters, alphabet) -> name, (name, letters, alphabet)),
                fun (name, letters, alphabet) ->
                    for c in letters do
                        Assert.True((alphabet c).IsSome, $"{name} has no {c}")
            )
            Test.CasesSync(
                "whatIsNotALetterHasNoAlphabetEntry",
                [ for c in "0123456789+=()" do yield string c, c ],
                fun c ->
                    Assert.True((Letters.italic c).IsNone, $"italic accepted {c}")
                    Assert.True((Letters.bold c).IsNone, $"bold accepted {c}")
            )
            Test.Sync(
                "capitalGreekIsLeftUpright",
                fun () ->
                    Assert.True((Letters.italic 'Γ').IsNone, "italic accepted a capital Greek letter")
                    Assert.True((Letters.upright 'Γ').IsSome, "upright has no capital Greek")
            )
            Test.Sync(
                "italicLettersDifferFromTheUprightOnes",
                fun () ->
                    for c in [ 'a' .. 'z' ] do
                        match Letters.italic c with
                        | ValueSome italic -> Assert.True(italic.Id <> (upright c).Id, $"italic {c} is upright")
                        | ValueNone -> Assert.Fail $"no italic {c}"
            )
            Test.Sync(
                "theMinusSignIsNotTheHyphen",
                fun () -> Assert.True(Operators.minus.Id <> (glyph '-').Id, "same glyph")
            )
            Test.CasesSync(
                "namedDelimitersStretch",
                [   "roundLeft", Delimiters.roundLeft
                    "roundRight", Delimiters.roundRight
                    "bar", Delimiters.bar
                    "surd", Radicals.surd ]
                |> List.map (fun (name, stretchy) -> name, (name, stretchy)),
                fun (name, stretchy: StretchyGlyph) ->
                    Assert.True(stretchy.SizeCount > 1, $"{name} offers one size only")
                    Assert.Equal(stretchy.Glyph.Id, (stretchy.Size 0).Glyph.Id, $"{name} size 0")
                    let mutable previous = 0
                    for i in 0 .. stretchy.SizeCount - 1 do
                        let size = stretchy.Size i
                        Assert.True(size.Advance >= previous, $"{name} size {i} shrank")
                        previous <- size.Advance
                    Assert.True(stretchy.PartCount > 0, $"{name} has no assembly")
                    let mutable extenders = 0
                    for i in 0 .. stretchy.PartCount - 1 do
                        if (stretchy.Part i).IsExtender then extenders <- extenders + 1
                    Assert.True(extenders > 0, $"{name} has no repeatable part")
            )
        ]
    )

let tests = TestFolder("MathFont", [ data; metrics; named ])
