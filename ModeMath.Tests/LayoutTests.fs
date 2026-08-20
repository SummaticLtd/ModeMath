module ModeMath.Tests.LayoutTests

open System.Collections.Immutable
open SimpleTests
open ModeMath

let private layout = Layout 20f
let private laid(ma: MA) = layout.Of ma
let private row(elements: MA list) = MA.Row(elements.ToImmutableArray())
let private c(character: char) = MA.Char character

let private nearly(expected: float32, actual: float32, message: string) =
    Assert.True(abs (expected - actual) < 0.01f, $"{message}: expected {expected} but was {actual}")

let private measurement =
    TestList(
        "Measurement",
        [   Test.Sync(
                "aCharacterHasWidthAndRisesAboveTheBaseline",
                fun () ->
                    let x = laid(c 'x')
                    Assert.True(x.Width > 0f, "width")
                    Assert.True(x.Ascent > 0f, "ascent")
                    Assert.True(x.Descent >= 0f, "descent")
            )
            Test.Sync(
                "anEmptyFormulaHasNoSize",
                fun () ->
                    let empty = laid MA.Empty
                    nearly(0f, empty.Width, "width")
                    nearly(0f, empty.Height, "height")
            )
            Test.Sync(
                "aRowOfOrdinariesIsAsWideAsItsParts",
                fun () ->
                    let parts = "abc" |> Seq.sumBy (fun character -> (laid(c character)).Width)
                    nearly(parts, (laid(MA.String "abc")).Width, "row width")
            )
            Test.Sync(
                "aBinaryOperatorIsGivenRoomOnBothSides",
                fun () ->
                    let spaced = laid(row [ c 'a'; MA.Operator Operator.Plus; c 'b' ])
                    let crowded =
                        [ c 'a'; MA.Operator Operator.Plus; c 'b' ]
                        |> List.sumBy (fun ma -> (laid ma).Width)
                    Assert.True(spaced.Width > crowded + 1f, $"{spaced.Width} is no wider than {crowded}")
            )
            Test.Sync(
                "aRelationIsGivenMoreRoomThanABinaryOperator",
                fun () ->
                    let binary = laid(row [ c 'a'; MA.Operator Operator.Plus; c 'b' ])
                    let relation = laid(row [ c 'a'; MA.Operator Operator.Equals; c 'b' ])
                    let plus = (laid(MA.Operator Operator.Plus)).Width
                    let equals = (laid(MA.Operator Operator.Equals)).Width
                    Assert.True(
                        relation.Width - equals > binary.Width - plus,
                        "a relation is spaced no wider than a binary operator")
            )
            Test.Sync(
                "aLeadingMinusSignIsNotBinary",
                fun () ->
                    let parts = [ c '-'; c 'b' ] |> List.sumBy (fun ma -> (laid ma).Width)
                    nearly(parts, (laid(row [ c '-'; c 'b' ])).Width, "unary minus")
            )
        ]
    )

let private structures =
    TestList(
        "Structures",
        [   Test.Sync(
                "aSuperscriptRisesAboveItsBase",
                fun () ->
                    let e = laid(c 'e')
                    let raised = laid(MA.ScriptSuper(c 'e', c '2', ValueNone))
                    Assert.True(raised.Ascent > e.Ascent, "superscript does not rise")
                    Assert.True(raised.Width > e.Width, "superscript takes no width")
            )
            Test.Sync(
                "aSubscriptFallsBelowItsBase",
                fun () ->
                    let a = laid(c 'a')
                    let lowered = laid(MA.ScriptSub(c 'a', c '0'))
                    Assert.True(lowered.Descent > a.Descent, "subscript does not fall")
            )
            Test.Sync(
                "scriptsAreSetSmallerThanTheirBase",
                fun () ->
                    let two = laid(c '2')
                    let raised = laid(MA.ScriptSuper(c 'e', c '2', ValueNone))
                    let e = laid(c 'e')
                    Assert.True(raised.Width - e.Width < two.Width, "the superscript was not shrunk")
            )
            Test.Sync(
                "aFractionIsAsWideAsItsWiderPart",
                fun () ->
                    let fraction = laid(MA.Frac(c '2', MA.String "34"))
                    let denominator = laid(MA.String "34")
                    Assert.True(fraction.Width >= denominator.Width, "fraction is narrower than its denominator")
            )
            Test.Sync(
                "aFractionStraddlesTheBaseline",
                fun () ->
                    let fraction = laid(MA.Frac(c '2', c '3'))
                    Assert.True(fraction.Ascent > 0f, "ascent")
                    Assert.True(fraction.Descent > 0f, "descent")
            )
            Test.Sync(
                "aFractionIsSetShorterInlineThanOnItsOwnLine",
                fun () ->
                    let fraction = MA.Frac(c '2', c '3')
                    let inline' = layout.Of(fraction, MathSize.Text)
                    Assert.True(inline'.Height < (laid fraction).Height, "inline is no shorter")
            )
            Test.Sync(
                "bracketsGrowWithWhatTheyHold",
                fun () ->
                    let small = laid(MA.RoundBracket(c 'x'))
                    let large = laid(MA.RoundBracket(MA.Frac(c '2', c '3')))
                    Assert.True(large.Height > small.Height, "brackets did not grow")
            )
            Test.Sync(
                "aRadicalCoversItsRadicand",
                fun () ->
                    let x = laid(c 'x')
                    let rooted = laid(MA.Sqrt(c 'x'))
                    Assert.True(rooted.Ascent > x.Ascent, "the rule does not clear the radicand")
                    Assert.True(rooted.Width > x.Width, "the surd takes no width")
            )
            Test.Sync(
                "aNarrowDegreeTucksIntoTheSurd",
                fun () ->
                    let square = laid(MA.Sqrt(c 'x'))
                    let cube = laid(MA.RootN(c '3', c 'x'))
                    nearly(square.Width, cube.Width, "a single-digit degree fits in the notch")
            )
            Test.Sync(
                "aWideDegreeWidensARadical",
                fun () ->
                    let square = laid(MA.Sqrt(c 'x'))
                    let rooted = laid(MA.RootN(MA.String "123", c 'x'))
                    Assert.True(rooted.Width > square.Width, "the degree takes no width")
            )
            Test.Sync(
                "deeplyNestedRadicalsStayFinite",
                fun () ->
                    let rec nest(depth: int) = if depth = 0 then c 'x' else MA.Sqrt(nest (depth - 1))
                    let deep = laid(nest 8)
                    Assert.True(deep.Height > 0f && deep.Height < 10000f, $"height {deep.Height}")
            )
            Test.Sync(
                "anUnclosedBracketIsStillLaidOut",
                fun () ->
                    let tentative =
                        laid(MA.Bracketed(Bracket.Normal, MA.String "x+1", BracketCompletion.Left))
                    let complete =
                        laid(MA.Bracketed(Bracket.Normal, MA.String "x+1", BracketCompletion.Completed))
                    nearly(complete.Width, tentative.Width, "an offered bracket takes the same room")
            )
        ]
    )

let tests = TestFolder("Layout", [ measurement; structures ])
