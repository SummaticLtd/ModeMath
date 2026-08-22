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
        [   Test.CasesSync(
                "theEmbeddedFontIsTheOneTheMetricsCameFrom",
                [   "math", (Face.Math, FontFile.byteLength, FontFile.sha256)
                    "blackboard", (Face.Blackboard, FontFile.blackboardByteLength, FontFile.blackboardSha256) ],
                fun (face, byteLength, sha256) ->
                    use stream = MathFont.OpenFontFile face
                    use copy = new MemoryStream()
                    stream.CopyTo copy
                    let bytes = copy.ToArray()
                    Assert.Equal(byteLength, bytes.Length, $"{face} byte length")
                    Assert.Equal(sha256, Convert.ToHexStringLower(SHA256.HashData bytes), $"{face} SHA-256")
            )
            Test.CasesSync(
                "theLicenceOfEveryFaceTravelsWithIt",
                [   "math", (Face.Math, "GUST Font License")
                    "blackboard", (Face.Blackboard, "SIL OPEN FONT LICENSE") ],
                fun (face, wording) ->
                    use stream = MathFont.OpenLicenceFile face
                    use reader = new StreamReader(stream)
                    let text = reader.ReadToEnd()
                    Assert.True(text.Contains wording, $"the {face} licence does not read as one")
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
                    Assert.Equal(1000f<du>, MathConstants.UnitsPerEm)
                    Assert.Equal(250f<du>, MathConstants.AxisHeight)
                    Assert.Equal(40f<du>, MathConstants.FractionRuleThickness)
                    Assert.Equal(70f, MathConstants.ScriptPercentScaleDown)
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
                    Assert.True(x.Advance > 0f<du>, "advance")
                    Assert.True(x.Top > 0f<du>, "top above the baseline")
                    Assert.True(x.Bottom <= 0f<du>, "bottom on or below the baseline")
                    Assert.True(x.Top < MathConstants.UnitsPerEm, "top within the em")
            )
            Test.Sync(
                "italicFLeansPastItsAdvance",
                fun () ->
                    match Letters.italic 'f' with
                    | ValueNone -> Assert.Fail "the font has no italic f"
                    | ValueSome f -> Assert.True(f.ItalicCorrection > 0f<du>, "italic correction")
            )
            Test.Sync(
                "digitsHaveNoItalicCorrection",
                fun () -> Assert.Equal(0f<du>, (digit '0').ItalicCorrection)
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
                    "italic greek", [ 'α' .. 'ω' ], Letters.italic
                    "italic shapes", [ '∂'; 'ϵ'; 'ϑ'; 'ϰ'; 'ϕ'; 'ϱ'; 'ϖ' ], Letters.italic
                    "bold latin small", [ 'a' .. 'z' ], Letters.bold
                    "bold latin capital", [ 'A' .. 'Z' ], Letters.bold
                    "blackboard capital", [ 'A' .. 'Z' ], Letters.blackboard ]
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
                "everyBlackboardCapitalComesFromTheBlackboardFace",
                fun () ->
                    for letter in 'A' .. 'Z' do
                        match Letters.blackboard letter with
                        | ValueSome glyph ->
                            Assert.Equal(Face.Blackboard, glyph.Face, $"the face of {letter}")
                            Assert.True(glyph.Advance > 0f<du>, $"{letter} has no advance")
                        | ValueNone -> Assert.Fail $"no blackboard {letter}"
            )
            Test.Sync(
                "accentsAreCombiningMarksDrawnLeftOfTheOrigin",
                fun () ->
                    for name, accent in
                        [   "hat", Accents.hat; "tilde", Accents.tilde; "bar", Accents.bar
                            "vec", Accents.vec; "dot", Accents.dot; "doubleDot", Accents.doubleDot
                            "check", Accents.check; "acute", Accents.acute; "grave", Accents.grave
                            "breve", Accents.breve ] do
                        Assert.Equal(0f<du>, accent.Advance, $"{name} takes an advance")
                        Assert.True(accent.TopAccentAttachment < 0f<du>, $"{name} attaches right of its origin")
                        Assert.True(accent.Top > 0f<du>, $"{name} draws no ink above the baseline")
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
                    let mutable previous = 0f<du>
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
