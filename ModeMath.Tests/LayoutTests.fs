module ModeMath.Tests.LayoutTests

open System.Collections.Immutable
open System.Drawing
open FSUtils
open SimpleTests
open ModeMath

let private layout = Layout 20f<px>
let private laid(ma: MA) = layout.Of ma

/// The tree an editor holds, which shows the box in an empty slot that a displayed one does not.
let private edited(ma: MA) = (layout.Of(MACurs.AtStart ma)).Placed
let private row(elements: MA list) = MA.Row(elements.ToImmutableArray())
let private c(character: char) = MA.Char character

let private grid(cells: MA list list, alignments: Alignment list) =
    MA.Table(ImmA2D.fromJagged cells, alignments.ToImmutableArray())

/// The units of the font at the size the tests lay out.
let private units = 20f<px> / MathConstants.UnitsPerEm

/// The rules an atom draws itself, which is one for a fraction and none for a stack.
let private rules(placed: Placed) =
    [
        for part in placed.Parts do
            match part with
            | Part.Rule(rule, _) -> yield rule
            | Part.Glyph _ | Part.Child _ | Part.Painted _ -> ()
    ]

/// Every glyph an atom draws, in the order it draws them.
let private drawnGlyphs(placed: Placed) =
    [
        for part in placed.Parts do
            match part with
            | Part.Glyph(glyph, _) -> yield glyph.Glyph.Id
            | Part.Rule _ | Part.Child _ | Part.Painted _ -> ()
    ]

let private glyphOf(placed: Placed) =
    match placed.Pma.SingleGlyph with
    | ValueSome glyph -> glyph
    | ValueNone -> failwith $"not one glyph: {placed.Pma}"

let private nearly(expected: float32<px>, actual: float32<px>, message: string) =
    Assert.True(abs (expected - actual) < 0.01f<px>, $"{message}: expected {expected} but was {actual}")

let private measurement =
    TestList(
        "Measurement",
        [   Test.Sync(
                "aCharacterHasWidthAndRisesAboveTheBaseline",
                fun () ->
                    let x = laid(c 'x')
                    Assert.True(x.Width > 0f<px>, "width")
                    Assert.True(x.Ascent > 0f<px>, "ascent")
                    Assert.True(x.Descent >= 0f<px>, "descent")
            )
            Test.Sync(
                "anEmptyFormulaDisplayedDrawsNothingAtAll",
                fun () ->
                    let empty = laid MA.Empty
                    Assert.Equal(0f<px>, empty.Width, "width")
                    Assert.Equal(0f<px>, empty.Height, "height")
            )
            Test.Sync(
                "anEmptySlotShowsTheBoxAFormulaCouldBeWrittenInOnlyWhileItIsEdited",
                fun () ->
                    let numerator(placed: Placed) =
                        match placed.Pma with
                        | PlacedMA.Frac(numerator, _, _) -> numerator
                        | other -> failwith $"not a fraction: {other}"
                    let formula = MA.Frac(MA.Empty, c 'c')
                    let shown = numerator(laid formula)
                    let box = numerator(edited formula)
                    Assert.Equal(0f<px>, shown.Width, "an empty slot displayed drew something")
                    Assert.True(box.Width > 0f<px>, "an empty slot being edited drew nothing")
                    match box.Pma with
                    | PlacedMA.Placeholder _ -> ()
                    | other -> failwith $"an empty slot being edited drew {other}"
            )
            Test.Sync(
                "anEmptyPartOfAnAtomNoCursorEntersShowsNoBoxEither",
                fun () ->
                    // Only a MACurs case names a slot, so nowhere else can a box be filled in.
                    let hat = edited(MA.Accented(Accent.Hat, MA.Empty))
                    match hat.Pma with
                    | PlacedMA.Accented(_, _, x) ->
                        Assert.Equal(0f<px>, x.Width, "an accent drew a box under itself")
                    | other -> failwith $"not an accent: {other}"
            )
            Test.Sync(
                "anEmptyFormulaBeingEditedIsNoSlotAndShowsNoBox",
                fun () ->
                    let placed = edited MA.Empty
                    Assert.Equal(0f<px>, placed.Width, "an empty formula drew a box to hold itself")
            )
            Test.Sync(
                "aRowOfOrdinariesIsAsWideAsItsPartsLessTheLeansTheySetUnder",
                fun () ->
                    let letters = [ for character in "abc" do yield laid(c character) ]
                    let parts = letters |> List.sumBy (fun letter -> letter.Width)
                    let tucked =
                        letters
                        |> List.take (letters.Length - 1)
                        |> List.sumBy (fun letter -> letter.ItalicCorrection)
                    Assert.True(tucked > 0f<px>, "the test proves nothing unless one of the letters leans")
                    nearly(parts - tucked, (laid(MA.String "abc")).Width, "row width")
            )
            Test.Sync(
                "aRowKeepsTheReachOfALeanThatAWidthlessAtomFollows",
                fun () ->
                    let f = laid(c 'f')
                    Assert.True(f.ItalicCorrection > 0f<px>, "italic f does not lean")
                    let followed = laid(row [ c 'f'; MA.Text "" ])
                    nearly(f.Width, followed.Width, "the row stopped short of the lean it drew")
            )
            Test.Sync(
                "aLeanDoesNotPushTheNextLetterThoughTheRowStillClearsIt",
                fun () ->
                    let f = laid(c 'f')
                    let g = laid(c 'g')
                    Assert.True(f.ItalicCorrection > 0f<px>, "italic f does not lean")
                    match (laid(MA.String "fg")).Pma with
                    | PlacedMA.Row children ->
                        nearly(f.Width - f.ItalicCorrection, children.[1].X, "g does not set under the lean")
                    | other -> Assert.Fail $"not a row: {other}"
                    nearly(
                        g.Width - g.ItalicCorrection + f.Width,
                        (laid(MA.String "gf")).Width,
                        "a row ending in a lean does not reach past it")
            )
            Test.Sync(
                "aBinaryOperatorIsGivenRoomOnBothSides",
                fun () ->
                    let spaced = laid(row [ c 'a'; MA.Char '+'; c 'b' ])
                    let crowded =
                        [ c 'a'; MA.Char '+'; c 'b' ]
                        |> List.sumBy (fun ma -> (laid ma).Width)
                    Assert.True(spaced.Width > crowded + 1f<px>, $"{spaced.Width} is no wider than {crowded}")
            )
            Test.Sync(
                "aRelationIsGivenMoreRoomThanABinaryOperator",
                fun () ->
                    let binary = laid(row [ c 'a'; MA.Char '+'; c 'b' ])
                    let relation = laid(row [ c 'a'; MA.Char '='; c 'b' ])
                    let plus = (laid(MA.Char '+')).Width
                    let equals = (laid(MA.Char '=')).Width
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
                "aScriptSitsBesideALeanWhereTheNextLetterWouldSetUnderIt",
                fun () ->
                    let f = laid(c 'f')
                    match (laid(MA.ScriptSuper(c 'f', c '2', ValueSome(c '1')))).Pma with
                    | PlacedMA.ScriptSuper(_, super, ValueSome sub) ->
                        nearly(f.Width, super.X, "the superscript does not clear the lean")
                        nearly(f.Width - f.ItalicCorrection, sub.X, "the subscript does not step back under it")
                    | other -> Assert.Fail $"not a superscript with a subscript: {other}"
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
                    Assert.True(fraction.Ascent > 0f<px>, "ascent")
                    Assert.True(fraction.Descent > 0f<px>, "descent")
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
                    Assert.True(deep.Height > 0f<px> && deep.Height < 10000f<px>, $"height {deep.Height}")
            )
            Test.Sync(
                "anUnclosedBracketIsStillLaidOut",
                fun () ->
                    let tentative =
                        laid(MA.Bracketed(Brackets.Matching Bracket.Normal, MA.String "x+1", BracketCompletion.Left))
                    let complete =
                        laid(MA.Bracketed(Brackets.Matching Bracket.Normal, MA.String "x+1", BracketCompletion.Completed))
                    nearly(complete.Width, tentative.Width, "an offered bracket takes the same room")
            )
        ]
    )

let private repertoire =
    TestList(
        "Repertoire",
        [   Test.Sync(
                "aCharacterTheFontCannotDrawIsAnErrorRatherThanAGap",
                fun () ->
                    for character in "☃漢�" do
                        Assert.Throws(
                            (fun () -> laid(c character) |> ignore),
                            $"{character} is outside the repertoire and must not lay out as nothing")
            )
            Test.Sync(
                "aFormulaSaysWhichOfItsCharactersCannotBeDrawn",
                fun () ->
                    // What a caller building a formula in code asks before drawing it.
                    Assert.Equal(ImmutableArray<char>.Empty, (MA.String "x+1").Undrawable, "an ordinary formula")
                    let awkward =
                        MA.Row(
                            [ MA.Char '☃'; MA.BoldVar '≤'; MA.Blackboard 'a'; MA.Text "ϵ" ]
                                .ToImmutableArray())
                    Assert.Equal(
                        [ '☃'; '≤'; 'a'; 'ϵ' ],
                        List.ofSeq awkward.Undrawable,
                        "each character no alphabet of its kind holds")
            )
            Test.CasesSync(
                "everyFunctionNameCanBeSet",
                [ for struct(name, f) in MathFunctions.named do yield name, f ],
                fun f ->
                    // Not every name is letters: the indicator is 1 and the gamma function a capital.
                    Assert.True((laid(MA.Function f)).Width > 0f<px>, $"{f} is set as nothing")
            )
            Test.Sync(
                "aFactorialClosesWhatItStandsAfter",
                fun () ->
                    // A binary with a close after it has nothing to bind, which is where the class tells.
                    let gap(after: char) =
                        (laid(MA.String("a+" + string after))).Width - (laid(c after)).Width
                    nearly(gap ')', gap '!', "a factorial is spaced unlike a closing bracket")
                    Assert.True(gap 'x' > gap '!', "the operator before it bound as it would an ordinary")
            )
            Test.Sync(
                "aCharacterIsSpacedByTheClassItCarries",
                fun () ->
                    let gap(between: char) =
                        (laid(MA.String("x" + string between + "y"))).Width - (laid(c between)).Width
                    for standard, characters in [ '=', "↑↓↗↘⟶⟵←↦∣↻"; '+', "∘•" ] do
                        for character in characters do
                            nearly(gap standard, gap character, $"{character} is spaced unlike {standard}")
            )
            Test.Sync(
                "theCharactersATextBorrowsFromAreOrdinary",
                fun () ->
                    // A price, a unit prefix and a dash bind to their neighbours as any letter does.
                    for character in "£µ–" do
                        Assert.Equal(
                            (laid(c 'x')).Width + (laid(c character)).Width,
                            (laid(MA.String("x" + string character))).Width,
                            $"{character} was spaced as something other than an ordinary atom")
            )
            Test.CasesSync(
                "everyClassifiedCharacterCanBeDrawn",
                [   "relations", Conventions.relations
                    "binaries", Conventions.binaries
                    "opens", Conventions.opens
                    "closes", Conventions.closes
                    "punctuation", Conventions.punctuation ]
                |> List.map (fun (name, chars) -> name, (name, chars)),
                fun (name, chars) ->
                    for c in chars do
                        Assert.True(
                            (laid (c |> MA.Char)).Width > 0f<px>,
                            $"{name} holds {c}, which the repertoire cannot draw")
            )
        ]
    )

let private bigOperators =
    TestList(
        "BigOperators",
        [   Test.Sync(
                "displayStyleTakesATallerGlyphThanTextStyle",
                fun () ->
                    let sum = MA.BigOp(BigOperator.Sum, ValueNone, ValueNone)
                    let display = layout.Of(sum, MathSize.Display)
                    let text = layout.Of(sum, MathSize.Text)
                    Assert.True(
                        display.Height > text.Height,
                        $"display {display.Height} should exceed text {text.Height}")
            )
            Test.Sync(
                "displayStyleSetsLimitsAboveAndBelow",
                fun () ->
                    let bare = layout.Of(MA.BigOp(BigOperator.Sum, ValueNone, ValueNone), MathSize.Display)
                    let limited =
                        layout.Of(
                            MA.BigOp(BigOperator.Sum, ValueSome(c 'n'), ValueSome(c 'm')),
                            MathSize.Display)
                    Assert.True(limited.Ascent > bare.Ascent, "the upper limit rises above the operator")
                    Assert.True(limited.Descent > bare.Descent, "the lower limit drops below it")
                    nearly(bare.Width, limited.Width, "limits narrower than the operator leave it as wide")
            )
            Test.Sync(
                "textStyleSetsLimitsBeside",
                fun () ->
                    let bare = layout.Of(MA.BigOp(BigOperator.Sum, ValueNone, ValueNone), MathSize.Text)
                    let limited =
                        layout.Of(MA.BigOp(BigOperator.Sum, ValueSome(c 'n'), ValueSome(c 'm')), MathSize.Text)
                    Assert.True(limited.Width > bare.Width, "scripts take room to the right")
            )
            Test.Sync(
                "anIntegralKeepsItsLimitsBesideEvenInDisplayStyle",
                fun () ->
                    // A limit wider than the operator is centred over a sum but sits beside an integral.
                    let wide(op: BigOperator) =
                        (layout.Of(MA.BigOp(op, ValueSome(MA.String "n=1234567"), ValueNone), MathSize.Display)).Width
                    Assert.True(
                        wide BigOperator.Integral > wide BigOperator.Sum,
                        "the integral adds its limit to its own width, the sum does not")
            )
            Test.Sync(
                "aLimitWiderThanTheOperatorWidensTheWhole",
                fun () ->
                    let narrow = layout.Of(MA.BigOp(BigOperator.Sum, ValueSome(c 'n'), ValueNone), MathSize.Display)
                    let wide =
                        layout.Of(
                            MA.BigOp(BigOperator.Sum, ValueSome(MA.String "n=1234567"), ValueNone),
                            MathSize.Display)
                    Assert.True(wide.Width > narrow.Width, "the wider limit sets the width")
            )
            Test.Sync(
                "anIntegralsSubscriptStepsBackOverItsLean",
                fun () ->
                    let integral(lower, upper) =
                        layout.Of(MA.BigOp(BigOperator.Integral, lower, upper), MathSize.Display)
                    let one = (layout.Of(c '1', MathSize.Script)).Width
                    let above = integral(ValueNone, ValueSome(c '1'))
                    let below = integral(ValueSome(c '1'), ValueNone)
                    nearly(
                        below.Width + one,
                        above.Width,
                        "the superscript adds its width at the advance, the subscript none behind it")
            )
            Test.Sync(
                "anOperatorWithoutLimitsIsJustItsGlyph",
                fun () ->
                    let bare = layout.Of(MA.BigOp(BigOperator.Sum, ValueNone, ValueNone), MathSize.Text)
                    let advance = BigOperators.sum.Glyph.Advance * 20f<px> / MathConstants.UnitsPerEm
                    nearly(advance, bare.Width, "no limits, so none of the space that follows one")
            )
            Test.Sync(
                "limIsSetInLettersRatherThanAGlyph",
                fun () ->
                    let lim = layout.Of(MA.BigOp(BigOperator.Limit, ValueNone, ValueNone), MathSize.Display)
                    let sum = layout.Of(MA.BigOp(BigOperator.Sum, ValueNone, ValueNone), MathSize.Display)
                    Assert.True(lim.Width > 0f<px>, "lim is drawn")
                    Assert.True(lim.Height < sum.Height, "letters do not grow with display style as a glyph does")
            )
        ]
    )

/// The glyphs a mark is drawn from, so that a delimiter can be told from its neighbour.
let private glyphIds(mark: PlacedGlyphs) = mark.Glyphs |> ImmArray.map(fun g -> g.Glyph.Id)

/// The delimiters a bracketed formula was drawn with, which surround its content.
let private sides(placed: Placed) =
    match placed.Pma with
    | PlacedMA.Bracketed(_, left, _, right, _) -> left, right
    | other -> failwith $"not a bracketed atom: {other}"

let private brackets =
    TestList(
        "Brackets",
        [   Test.CasesSync(
                "everyShapeIsDrawnAndGrowsWithWhatItHolds",
                [
                    Bracket.Normal; Bracket.Line; Bracket.Square; Bracket.Curly; Bracket.Angle
                    Bracket.Floor; Bracket.Ceiling; Bracket.Slash
                ]
                |> List.map (fun b -> string b, b),
                fun bracket ->
                    let shortLeft, shortRight = sides(laid(MA.Paired(bracket, c 'x')))
                    let tallLeft, tallRight = sides(laid(MA.Paired(bracket, MA.Frac(MA.Frac(c 'a', c 'b'), c 'c'))))
                    Assert.True(shortLeft.Width > 0f<px>, $"{bracket} draws a left delimiter")
                    Assert.True(shortRight.Width > 0f<px>, $"{bracket} draws a right one")
                    Assert.True(tallLeft.Height > shortLeft.Height, $"{bracket}'s left delimiter grows")
                    Assert.True(tallRight.Height > shortRight.Height, $"{bracket}'s right one grows too")
            )
            Test.Sync(
                "theTwoSidesAreChosenIndependently",
                fun () ->
                    let inner = MA.String "0,1"
                    let square = sides(laid(MA.Paired(Bracket.Square, inner))) |> fst |> glyphIds
                    let round = sides(laid(MA.Paired(Bracket.Normal, inner))) |> snd |> glyphIds
                    let left, right =
                        sides(
                            laid(
                                MA.Bracketed(
                                    Brackets(Bracket.Square, Bracket.Normal),
                                    inner,
                                    BracketCompletion.Completed)))
                    Assert.CollectionEqual(square, glyphIds left, "[0, 1) opens with the square bracket's glyph")
                    Assert.CollectionEqual(round, glyphIds right, "and closes with the round one's")
            )
            Test.Sync(
                "anAbsentBracketDrawsNothingAtAll",
                fun () ->
                    let inner = MA.Frac(c 'a', c 'b')
                    let open_ =
                        laid(MA.Bracketed(Brackets(Bracket.None, Bracket.Line), inner, BracketCompletion.Completed))
                    let left, right = sides open_
                    Assert.Empty(glyphIds left, "the absent side drew a delimiter")
                    Assert.Greater((glyphIds right).Length, 0, "the bar was not drawn")
                    nearly((laid inner).Width + right.Width, open_.Width, "the absent side took width")
            )
            Test.Sync(
                "matchingGivesBothSidesTheSameShape",
                fun () ->
                    let pair = Brackets.Matching Bracket.Curly
                    Assert.Equal(Bracket.Curly, pair.Left)
                    Assert.Equal(Bracket.Curly, pair.Right)
            )
        ]
    )

/// The lowest ink of a mark, wherever its atom has since moved it to.
let private lowestInk(mark: PlacedGlyphs) = mark.Glyphs |> Seq.map (fun glyph -> glyph.Bottom) |> Seq.min

/// The grown mark and the atom it spans.
let private spanning(placed: Placed) =
    match placed.Pma with
    | PlacedMA.Spanned(_, mark, x) -> mark, x
    | other -> failwith $"not a spanning mark: {other}"

let private lettersOf(placed: Placed) =
    match placed.Pma with
    | PlacedMA.Text(_, letters) -> letters
    | other -> failwith $"not text: {other}"

/// The accent and the base it was placed over.
let private accented(placed: Placed) =
    match placed.Pma with
    | PlacedMA.Accented(_, mark, x) -> mark, x
    | other -> failwith $"not an accented atom: {other}"

let private cellsOf(placed: Placed) =
    match placed.Pma with
    | PlacedMA.Table(cells, _) -> cells
    | other -> failwith $"not a table: {other}"

let private marks =
    TestList(
        "Marks",
        [   Test.Sync(
                "anAccentRisesAboveItsBaseWithoutWideningIt",
                fun () ->
                    let bare = laid(c 'x')
                    let hatted = laid(MA.Accented(Accent.Hat, c 'x'))
                    nearly(bare.Width, hatted.Width, "an accent takes no width of its own")
                    Assert.True(hatted.Ascent > bare.Ascent, "the accent does not rise above the base")
            )
            Test.Sync(
                "anAccentSitsOverThePointItsBaseAttachesAt",
                fun () ->
                    // Italic d attaches well right of its middle, so a midpoint would place the hat wrong.
                    let d =
                        match Letters.italic 'd' with
                        | ValueSome glyph -> glyph
                        | ValueNone -> failwith "the font has no italic d"
                    let mark, _ = accented(laid(MA.Accented(Accent.Hat, c 'd')))
                    let expected = (d.TopAccentAttachment - Accents.hat.TopAccentAttachment) * units
                    nearly(expected, mark.X, "the accent is not placed by the attachments")
                    Assert.True(
                        d.TopAccentAttachment > d.Advance / 2f,
                        "the test proves nothing if the attachment is the midpoint")
            )
            Test.Sync(
                "anAccentClearsABaseTallerThanAccentsAreDrawnFor",
                fun () ->
                    let over(x: MA) = fst (accented(laid(MA.Accented(Accent.Hat, x))))
                    Assert.True(
                        (over(c 'b')).Y > (over(c 'x')).Y,
                        "the accent does not rise for the taller of the two letters")
                    nearly(0f<px>, (over(c '.')).Y, "a base shorter than the accent base height lifts nothing")
                    nearly(
                        (laid(c 'b')).Ascent - MathConstants.AccentBaseHeight * units,
                        (over(c 'b')).Y,
                        "a taller base lifts the accent by more than its excess")
            )
            Test.CasesSync(
                "everyAccentIsDrawn",
                [   Accent.Hat; Accent.Tilde; Accent.Bar; Accent.Vec; Accent.Dot
                    Accent.DoubleDot; Accent.Check; Accent.Acute; Accent.Grave; Accent.Breve ]
                |> List.map (fun accent -> string accent, accent),
                fun accent ->
                    let mark, _ = accented(laid(MA.Accented(accent, c 'x')))
                    Assert.True(mark.Top > mark.Bottom, $"{accent} draws no ink")
            )
            Test.Sync(
                "anOverlineRulesTheFullWidthAboveTheAtom",
                fun () ->
                    let bare = laid(MA.String "ab")
                    let ruled = laid(MA.Overline(MA.String "ab"))
                    nearly(bare.Width, ruled.Width, "an overline takes no width of its own")
                    Assert.True(ruled.Ascent > bare.Ascent, "the rule does not rise above the atom")
                    nearly(bare.Descent, ruled.Descent, "the rule reaches below the atom")
                    match rules ruled with
                    | [ rule ] ->
                        nearly(bare.Width, rule.Width, "the rule does not span the atom")
                        Assert.True(rule.Y > bare.Ascent, "the rule is not clear of the ink")
                    | drawn -> Assert.Fail $"an overline draws {drawn.Length} rules"
            )
            Test.Sync(
                "anUnderlineRulesTheFullWidthBelowTheAtom",
                fun () ->
                    let bare = laid(MA.String "ab")
                    let ruled = laid(MA.Underline(MA.String "ab"))
                    nearly(bare.Width, ruled.Width, "an underline takes no width of its own")
                    nearly(bare.Ascent, ruled.Ascent, "the rule reaches above the atom")
                    Assert.True(ruled.Descent > bare.Descent, "the rule does not fall below the atom")
                    match rules ruled with
                    | [ rule ] ->
                        nearly(bare.Width, rule.Width, "the rule does not span the atom")
                        Assert.True(rule.Y + rule.Thickness < -bare.Descent, "the rule is not clear of the ink")
                    | drawn -> Assert.Fail $"an underline draws {drawn.Length} rules"
            )
        ]
    )

let private colours =
    TestList(
        "Colours",
        [   Test.Sync(
                "aColourIsHandedToWhateverDrawsTheAtomInside",
                fun () ->
                    let parts = (laid(MA.Coloured(Color.Crimson, MA.String "ab"))).Parts
                    Assert.Equal(1, parts.Length, "a colour draws more than the atom inside it")
                    match parts.[0] with
                    | Part.Painted(colour, child) ->
                        Assert.Equal(Color.Crimson, colour, "colour")
                        Assert.True(child.Parts.Length > 0, "the atom inside draws nothing")
                    | other -> Assert.Fail $"not a painted child: {other}"
            )
            Test.Sync(
                "aColourChangesNothingAboutTheSize",
                fun () ->
                    let bare = laid(MA.String "ab")
                    let painted = laid(MA.Coloured(Color.Crimson, MA.String "ab"))
                    nearly(bare.Width, painted.Width, "width")
                    nearly(bare.Ascent, painted.Ascent, "ascent")
                    nearly(bare.Descent, painted.Descent, "descent")
            )
            Test.Sync(
                "aColourKeepsWhatTheAtomBindsToOnEitherSide",
                fun () ->
                    let plus = MA.Char '+'
                    let bare = laid(row [ c 'a'; plus; c 'b' ])
                    let painted = laid(row [ c 'a'; MA.Coloured(Color.Crimson, plus); c 'b' ])
                    nearly(bare.Width, painted.Width, "a coloured operator is spaced as a plain one")
            )
        ]
    )

let private words =
    TestList(
        "Words",
        [   Test.Sync(
                "textIsSetInTheUprightLetters",
                fun () ->
                    let drawn = [ for glyph in (lettersOf(laid(MA.Text "ab"))).Glyphs do yield glyph.Glyph.Id ]
                    let upright(character: char) =
                        match Letters.upright character with
                        | ValueSome glyph -> glyph.Id
                        | ValueNone -> failwith $"no upright {character}"
                    Assert.Equal([ upright 'a'; upright 'b' ], drawn, "the letters are not the upright ones")
            )
            Test.Sync(
                "smallGreekIsUprightInTextAndItalicInAFormula",
                fun () ->
                    // A unit is written \mathrm{μg}, and its prefix leans no more than its letter does.
                    let text = drawnGlyphs(laid(MA.Text "μ"))
                    Assert.True(text <> drawnGlyphs(laid(c 'μ')), "text was set in the italic small Greek")
            )
            Test.Sync(
                "textKeepsTheSpacesBetweenItsWords",
                fun () ->
                    let space =
                        match MathFont.OfChar ' ' with
                        | ValueSome glyph -> glyph.Advance * units
                        | ValueNone -> failwith "the repertoire has no space"
                    Assert.True(space > 0f<px>, "the space in the font is of no width")
                    nearly(
                        space,
                        (laid(MA.Text "a b")).Width - (laid(MA.Text "ab")).Width,
                        "the space between the words is not the one the font sets")
            )
            Test.CasesSync(
                "everySpaceIsAsWideAsTeXMakesIt",
                [   "thin", (Space.Thin, 3f)
                    "medium", (Space.Medium, 4f)
                    "thick", (Space.Thick, 5f)
                    "negativeThin", (Space.NegativeThin, -3f)
                    "quad", (Space.Quad, 18f)
                    "qquad", (Space.QQuad, 36f) ],
                fun (space, eighteenths) ->
                    nearly(eighteenths * 20f<px> / 18f, (laid(MA.Space space)).Width, $"{space}")
            )
            Test.Sync(
                "aSpaceDoesNotStandBetweenAnOperatorAndWhatItBinds",
                fun () ->
                    // TeX's spacing commands are kerns, not atoms, so \,-x is still a unary minus.
                    let thin = (laid(MA.Space Space.Thin)).Width
                    nearly(
                        (laid(row [ c '-'; c 'x' ])).Width + thin,
                        (laid(row [ MA.Space Space.Thin; c '-'; c 'x' ])).Width,
                        "the space before the minus made it binary")
                    nearly(
                        (laid(row [ c 'a'; c '+'; c 'b' ])).Width + thin,
                        (laid(row [ c 'a'; MA.Space Space.Thin; c '+'; c 'b' ])).Width,
                        "the space beside the plus changed what it binds to")
            )
            Test.Sync(
                "aSpaceDrawsNothingAndReachesNowhere",
                fun () ->
                    let quad = laid(MA.Space Space.Quad)
                    Assert.Equal(0, quad.Parts.Length, "a space draws something")
                    nearly(0f<px>, quad.Height, "height")
            )
            Test.Sync(
                "aNegativeSpaceClosesTheGapItIsPutIn",
                fun () ->
                    let apart = laid(row [ c 'a'; c 'b' ])
                    let pulled = laid(row [ c 'a'; MA.Space Space.NegativeThin; c 'b' ])
                    Assert.True(pulled.Width < apart.Width, "the negative space did not pull the letters together")
            )
        ]
    )

let private grown =
    TestList(
        "Grown",
        [   Test.Sync(
                "aWideAccentTakesASizeThatCoversItsBase",
                fun () ->
                    let over(x: MA) = fst (accented(laid(MA.Accented(Accent.WideHat, x))))
                    Assert.Equal(
                        Accents.hat.Id,
                        (over(c 'l')).Glyph.Id,
                        "a base narrower than the plain hat takes the plain hat")
                    let covered = MA.String "ab"
                    Assert.True(
                        (over covered).Glyph.Advance * units >= (laid covered).Width,
                        "the hat chosen does not reach across the base")
                    Assert.True(
                        (over(MA.String "ABcd")).Glyph.Advance > (over covered).Glyph.Advance,
                        "the wider of the two bases did not take the wider hat")
            )
            Test.Sync(
                "aSpanningMarkGrowsToCoverWhatItSpans",
                fun () ->
                    let braced(x: MA) = fst (spanning(laid(MA.Spanned(Spanning.Overbrace, x))))
                    let wide = MA.String "abcdefghijklmnop"
                    Assert.True((braced wide).Width > (braced(MA.String "ab")).Width, "the brace did not grow")
                    Assert.True(
                        (braced wide).Width >= (laid wide).Width,
                        "the brace does not cover what it spans")
            )
            Test.Sync(
                "aMarkBeyondItsLargestSizeIsBuiltFromParts",
                fun () ->
                    let long = MA.String(String.replicate 12 "abcdefghij")
                    let mark, _ = spanning(laid(MA.Spanned(Spanning.Overbrace, long)))
                    Assert.True(mark.Glyphs.Length > 1, "the brace was not assembled from its parts")
                    Assert.True(mark.Width >= (laid long).Width, "the assembly does not reach across")
            )
            Test.CasesSync(
                "aMarkSetsAboveOrBelowAsItsKindDecides",
                [   "overbrace", (Spanning.Overbrace, true)
                    "underbrace", (Spanning.Underbrace, false)
                    "overrightarrow", (Spanning.Overrightarrow, true) ],
                fun (kind, above) ->
                    let mark, x = spanning(laid(MA.Spanned(kind, MA.String "abc")))
                    if above then
                        nearly(
                            x.Ascent + MathConstants.StretchStackGapAboveMin * units,
                            lowestInk mark,
                            $"{kind} does not clear the ink below it by the gap the font names")
                    else
                        Assert.True(
                            (mark.Glyphs |> Seq.map (fun glyph -> glyph.Top) |> Seq.max) < -x.Descent,
                            $"{kind} does not sit below the ink")
            )
            Test.Sync(
                "whatASpanningMarkOverreachesIsCentredUnderIt",
                fun () ->
                    let spanned = MA.Spanned(Spanning.Overbrace, MA.String "ab")
                    let mark, x = spanning(laid spanned)
                    Assert.True(mark.Width > x.Width, "the test proves nothing unless the brace overreaches")
                    nearly((laid spanned).Width - mark.Width, 0f<px>, "the atom is not as wide as the mark")
                    nearly((mark.Width - x.Width) / 2f, x.X, "the base is not centred under the mark")
            )
        ]
    )

let private stacks =
    TestList(
        "Stacks",
        [   Test.Sync(
                "aStackIsAFractionWithoutTheRule",
                fun () ->
                    let stacked = laid(MA.Stack(c 'a', c 'b'))
                    let divided = laid(MA.Frac(c 'a', c 'b'))
                    Assert.Equal(0, (rules stacked).Length, "a stack draws a rule")
                    Assert.Equal(1, (rules divided).Length, "a fraction draws no rule")
                    nearly(divided.Width, stacked.Width, "the two are set to the same width")
            )
            Test.Sync(
                "aStackKeepsItsPartsApart",
                fun () ->
                    let stacked = laid(MA.Stack(c 'a', c 'b'))
                    match stacked.Pma with
                    | PlacedMA.Stack(top, bottom) ->
                        Assert.True(
                            top.Y - top.Descent > bottom.Y + bottom.Ascent,
                            "the two parts overlap")
                    | other -> Assert.Fail $"not a stack: {other}"
            )
            Test.Sync(
                "aBinomialIsAStackInRoundBrackets",
                fun () ->
                    match (laid(MA.Binom(c '6', c 'x'))).Pma with
                    | PlacedMA.Bracketed(brackets, _, inner, _, _) ->
                        Assert.Equal(Bracket.Normal, brackets.Left, "left bracket")
                        Assert.Equal(Bracket.Normal, brackets.Right, "right bracket")
                        match inner.Pma with
                        | PlacedMA.Stack _ -> ()
                        | other -> Assert.Fail $"the brackets hold {other}"
                    | other -> Assert.Fail $"not bracketed: {other}"
            )
        ]
    )

let private tables =
    TestList(
        "Tables",
        [   Test.Sync(
                "aColumnIsAsWideAsItsWidestCellAndAnEmFollowsIt",
                fun () ->
                    let wide = MA.String "abc"
                    let table = laid(grid([ [ c 'x'; c 'y' ]; [ wide; c 'z' ] ], []))
                    let expected = (laid wide).Width + 20f<px> + max (laid(c 'y')).Width (laid(c 'z')).Width
                    nearly(expected, table.Width, "the columns are not set by their widest cells")
            )
            Test.CasesSync(
                "alignmentDecidesWhereANarrowCellSitsInItsColumn",
                [   "left", (Alignment.Left, 0f)
                    "centre", (Alignment.Centre, 0.5f)
                    "right", (Alignment.Right, 1f) ],
                fun (alignment, fraction) ->
                    let wide = MA.String "abc"
                    let table = laid(grid([ [ c 'x' ]; [ wide ] ], [ alignment ]))
                    let narrow = (cellsOf table).[0, 0]
                    let slack = (laid wide).Width - (laid(c 'x')).Width
                    nearly(slack * fraction, narrow.X, $"{alignment} puts the cell in the wrong place")
            )
            Test.Sync(
                "alignmentsAreTakenAgainOnceTheyRunOut",
                fun () ->
                    let alignments = ImmutableArray.Create(Alignment.Right, Alignment.Left)
                    Assert.Equal(Alignment.Right, MA.AlignmentOf(alignments, 0), "column 0")
                    Assert.Equal(Alignment.Left, MA.AlignmentOf(alignments, 1), "column 1")
                    Assert.Equal(Alignment.Right, MA.AlignmentOf(alignments, 2), "column 2")
                    Assert.Equal(
                        Alignment.Centre,
                        MA.AlignmentOf(ImmutableArray<Alignment>.Empty, 3),
                        "a table naming no alignment centres its columns")
            )
            Test.Sync(
                "rowsAreStackedAndTheGridIsCentredOnTheAxis",
                fun () ->
                    let one = laid(grid([ [ c 'x' ] ], []))
                    let two = laid(grid([ [ c 'x' ]; [ c 'x' ] ], []))
                    Assert.True(two.Height > one.Height, "a second row adds no height")
                    let axis = MathConstants.AxisHeight * units
                    nearly(axis, (two.Ascent - two.Descent) / 2f, "the grid is not centred on the axis")
            )
            Test.Sync(
                "anEmptyCellStillHoldsItsPlaceInTheGrid",
                fun () ->
                    let sparse = laid(grid([ [ c 'x'; MA.Empty ]; [ MA.Empty; c 'y' ] ], []))
                    let cells = cellsOf sparse
                    Assert.Equal(2, cells.Rows, "rows")
                    Assert.Equal(2, cells.Cols, "columns")
                    Assert.True(cells.[0, 1].X > cells.[0, 0].X, "the empty cell is not in the second column")
            )
        ]
    )

let private blackboard =
    TestList(
        "Blackboard",
        [   Test.Sync(
                "aBlackboardCapitalIsDrawnFromTheSecondFace",
                fun () ->
                    let letter = glyphOf(laid(MA.Blackboard 'F'))
                    Assert.Equal(Face.Blackboard, letter.Glyph.Face, "face")
                    Assert.True(letter.Glyph.Advance > 0f<du>, "the glyph has no advance")
            )
            Test.CasesSync(
                "aLetterlikeSymbolIsTheSameLetterOfTheSameFace",
                [ 'C', 'ℂ'; 'H', 'ℍ'; 'N', 'ℕ'; 'P', 'ℙ'; 'Q', 'ℚ'; 'R', 'ℝ'; 'Z', 'ℤ' ]
                |> List.map (fun (letter, symbol) -> string symbol, (letter, symbol)),
                fun (letter, symbol) ->
                    let fromAlphabet = glyphOf(laid(MA.Blackboard letter))
                    let fromCharacter = glyphOf(laid(c symbol))
                    Assert.Equal(Face.Blackboard, fromCharacter.Glyph.Face, $"{symbol} comes from the math face")
                    Assert.Equal(fromAlphabet.Glyph.Id, fromCharacter.Glyph.Id, $"{symbol} is not a blackboard {letter}")
            )
            Test.Sync(
                "anOrdinaryCapitalIsStillItalic",
                fun () ->
                    let italic = glyphOf(laid(c 'N'))
                    Assert.Equal(Face.Math, italic.Glyph.Face, "face")
                    Assert.True(
                        italic.Glyph.Id <> (glyphOf(laid(MA.Blackboard 'N'))).Glyph.Id,
                        "N is drawn as a blackboard bold capital")
            )
        ]
    )

/// One of every MA case, so that the round trip below covers the whole language.
let private everyKind =
    [
        MA.Empty
        MA.String "ab"
        c 'x'
        MA.BoldVar 'v'
        MA.Char '⋅'
        MA.UprightD
        MA.ScriptSuper(c 'e', c '2', ValueNone)
        MA.ScriptSuper(c 'e', c '2', ValueSome(c '3'))
        MA.ScriptSub(c 'a', c '1')
        MA.Frac(c 'a', c 'b')
        MA.Function MathFunction.Sin
        MA.Char '+'
        MA.Paired(Bracket.Square, MA.String "0,1")
        MA.Sqrt(c 'x')
        MA.RootN(c '3', c 'x')
        MA.BigOp(BigOperator.Sum, ValueSome(c 'n'), ValueSome(c 'm'))
        MA.BigOp(BigOperator.Integral, ValueSome(c '0'), ValueNone)
        MA.Blackboard 'R'
        MA.Accented(Accent.Vec, c 'v')
        MA.Accented(Accent.WideHat, MA.String "ab")
        MA.Spanned(Spanning.Overbrace, c 'x')
        MA.Spanned(Spanning.Underbrace, MA.String "ab")
        MA.Text "for"
        MA.Space Space.Quad
        MA.Coloured(Color.Crimson, c 'x')
        MA.Overline(MA.String "ab")
        MA.Underline(MA.String "ab")
        MA.Stack(c '6', c 'x')
        MA.Binom(c '6', c 'x')
        grid([ [ c 'a'; c 'b' ]; [ c 'c'; MA.Empty ] ], [ Alignment.Right; Alignment.Left ])
        MA.Matrix(ImmA2D.fromJagged [ [ c 'a'; c 'b' ] ])
        MA.Cases(ImmA2D.fromJagged [ [ c 'a'; c 'b' ] ])
        row [ c 'a'; MA.Frac(c 'b', c 'c'); MA.Sqrt(c 'd') ]
    ]

let private roundTrip =
    TestList(
        "RoundTrip",
        [   Test.CasesSync(
                "layingOutKeepsTheFormulaItLaidOut",
                everyKind |> List.map (fun ma -> ma.ToString(), ma),
                fun ma ->
                    Assert.Equal(
                        ma.Flatten.ToString(),
                        (layout.Of ma).Pma.ToMA.ToString(),
                        "a laid-out atom gives back the atom it was laid out from")
            )
        ]
    )

let private flat(ma: MA) = ma.Flatten

/// Bounded, so that a walk which never ends fails the test rather than hanging it.
let private positions(ma: MA) =
    let walked = MACurs.Positions ma |> Seq.truncate 1000 |> List.ofSeq
    if walked.Length = 1000 then failwith "the walk did not end"
    walked

/// One for each atom a cursor can go inside, so every PlacedMACurs is reached, and one whose pen
/// reaches outside what it draws.
let private cursored = [
    row [ c 'a'; MA.Frac(MA.String "b+1", c 'c'); c 'd' ]
    MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i'))
    MA.ScriptSub(c 'x', c 'i')
    MA.Bracketed(Brackets.Matching Bracket.Normal, MA.String "x+1", BracketCompletion.Completed)
    MA.RootN(c '3', MA.String "x+1")
    MA.Sqrt(MA.String "2y")
    MA.Frac(MA.Empty, c 'c')
    row [ MA.Space Space.NegativeThin; c 'x' ]
]

let private cursors =
    TestList(
        "Cursor",
        [   Test.Sync(
                "steppingRightEndsRatherThanRunningOn",
                fun () ->
                    // Every position is walked to find the one nearest a click, so it has to end.
                    Assert.Equal(3, (positions(MA.String "ab" |> flat)).Length, "positions in ab")
                    Assert.Equal(6, (positions(MA.Frac(c 'b', c 'd') |> flat)).Length, "in a fraction")
            )
            Test.Sync(
                "aPlacedCursorGoesBackToTheCursorItWasPlacedFrom",
                fun () ->
                    for formula in cursored |> List.map flat do
                        for curs in positions formula do
                            Assert.Equal(curs, (layout.Of curs).ToMACurs, $"in {formula}")
            )
            Test.Sync(
                "aCursorIsDrawnOverAFormulaLaidOutTheSameWhereverItStands",
                fun () ->
                    for formula in cursored |> List.map flat do
                        let bare = edited formula
                        for curs in positions formula do
                            let placed = (layout.Of curs).Placed
                            nearly(bare.Width, placed.Width, $"width at {curs}")
                            nearly(bare.Ascent, placed.Ascent, $"ascent at {curs}")
                            nearly(bare.Descent, placed.Descent, $"descent at {curs}")
            )
            Test.Sync(
                "whatACursoredFormulaCoversHoldsBothTheFormulaAndTheCursor",
                fun () ->
                    for formula in cursored |> List.map flat do
                        let placed = edited formula
                        for curs in positions formula do
                            let cursored = layout.Of curs
                            let bounds = cursored.Bounds
                            let caret = cursored.Caret
                            let holds(from: float32<px>, until: float32<px>, low: float32<px>, high: float32<px>) =
                                Assert.True(
                                    from - 0.01f<px> <= low && high <= until + 0.01f<px>,
                                    $"{low} to {high} was not inside {from} to {until} at {curs}")
                            holds(bounds.X, bounds.X + bounds.Width, caret.X, caret.X + caret.Width)
                            holds(bounds.Y, bounds.Y + bounds.Thickness, caret.Y, caret.Y + caret.Thickness)
                            holds(bounds.X, bounds.X + bounds.Width, 0f<px>, placed.Width)
                            holds(
                                bounds.Y,
                                bounds.Y + bounds.Thickness,
                                -placed.Descent,
                                placed.Ascent)
            )
            Test.Sync(
                "theCursorMovesRightwardsAsItIsStepped",
                fun () ->
                    let formula = MA.String "abc" |> flat
                    let xs = [ for curs in positions formula do yield (layout.Of curs).Caret.X ]
                    for pair in List.pairwise xs do
                        let previous, next = pair
                        Assert.True(next > previous, $"the cursor did not move: {previous} then {next}")
            )
            Test.Sync(
                "theCursorInAnEmptySlotFillsTheBoxTheSlotShows",
                fun () ->
                    let slot = MA.Frac(MA.Empty, c 'c') |> flat
                    let placeholder =
                        match (edited slot).Pma with
                        | PlacedMA.Frac(numerator, _, _) -> numerator
                        | other -> failwith $"not a fraction: {other}"
                    let caret = (layout.Of (positions slot).[1]).Caret
                    nearly(placeholder.Width, caret.Width, "the cursor did not fill the box")
                    nearly(placeholder.Height, caret.Thickness, "the cursor did not fill the box")
            )
            Test.Sync(
                "whatACursoredFormulaCoversStandsStillAsTheCursorMoves",
                fun () ->
                    // A caller drawing from the bounds would otherwise shift the formula as it typed.
                    for formula in cursored |> List.map flat do
                        let bounds = [ for curs in positions formula do yield (layout.Of curs).Bounds ]
                        let first = List.head bounds
                        for b in bounds do
                            nearly(first.X, b.X, $"left in {formula}")
                            nearly(first.Y, b.Y, $"bottom in {formula}")
                            nearly(first.Width, b.Width, $"width in {formula}")
                            nearly(first.Thickness, b.Thickness, $"height in {formula}")
            )
            Test.Sync(
                "aFormulaBeingEditedStandsOnALineWhateverIsTypedOnIt",
                fun () ->
                    // A caller placing the formula by its ascent draws the cursor above it otherwise.
                    for ma in [ MA.Empty; MA.String "x"; MA.String "ag"; MA.String "xb" ] do
                        let placed = layout.Of(MACurs.AtEnd(ma.Flatten))
                        let caret = placed.Caret
                        Assert.True(
                            caret.Y + caret.Thickness <= placed.Placed.Ascent + 0.01f<px>,
                            $"the bar rose {caret.Y + caret.Thickness} over an ascent of {placed.Placed.Ascent}")
                        Assert.True(
                            caret.Y >= -placed.Placed.Descent - 0.01f<px>,
                            $"the bar fell to {caret.Y} under a descent of {placed.Placed.Descent}")
            )
            Test.Sync(
                "theCursorStandsTheSameHoweverMuchHasBeenTypedAtIt",
                fun () ->
                    // A bar that took its height from the atoms would jump as the first was typed.
                    let bar(ma: MA) = (layout.Of(MACurs.AtEnd ma)).Caret
                    let alone = (layout.Of(MACurs.AtStart MA.Empty)).Caret
                    for typed in [ MA.String "a"; MA.String "ab"; MA.String "ag" ] do
                        let among = bar (typed.Flatten)
                        nearly(alone.Width, among.Width, $"width at {typed}")
                        nearly(alone.Thickness, among.Thickness, $"height at {typed}")
                        nearly(alone.Y, among.Y, $"foot at {typed}")
            )
            Test.Sync(
                "aCursorIsSmallerWhereTheAtomsAroundItAre",
                fun () ->
                    // A caret takes its size from the atom it stands in, which a Placed now knows.
                    let formula = MA.ScriptSuper(c 'x', MA.String "ab", ValueNone) |> flat
                    let inScript = (layout.Of (positions formula).[3]).Caret
                    let beside = (layout.Of (positions formula).[0]).Caret
                    Assert.True(
                        inScript.Thickness < beside.Thickness,
                        $"a caret in a superscript was {inScript.Thickness}, beside it {beside.Thickness}")
            )
            Test.Sync(
                "aPointOnAPositionFindsThatPosition",
                fun () ->
                    for formula in cursored |> List.map flat do
                        for curs in positions formula do
                            let caret = (layout.Of curs).Caret
                            let x = caret.X + caret.Width / 2f
                            let y = caret.Y + caret.Thickness / 2f
                            // Carets overlap, as before x does with before x squared, so a click
                            // cannot always tell which was meant. It must land on one holding it.
                            let found = (PlacedCurs.Nearest(edited formula, x, y)).Caret
                            Assert.True(
                                found.X - 0.01f<px> <= x && x <= found.X + found.Width + 0.01f<px>
                                && found.Y - 0.01f<px> <= y && y <= found.Y + found.Thickness + 0.01f<px>,
                                $"clicking on {curs} found a cursor that was not under the point")
            )
            Test.Sync(
                "everyPointFindsAPositionTheCursorCanReach",
                fun () ->
                    // Stepping right is what a position is, so a click must not invent one besides.
                    for formula in cursored |> List.map flat do
                        let placed = edited formula
                        let reachable = positions formula
                        for step in 0 .. 20 do
                            let across = float32 step / 20f
                            for rise in 0 .. 10 do
                                let up = float32 rise / 10f
                                let x = placed.Width * across
                                let y = placed.Ascent * up - placed.Descent * (1f - up)
                                let found = (PlacedCurs.Nearest(placed, x, y)).ToMACurs
                                Assert.True(
                                    reachable |> List.contains found,
                                    $"a click at {x}, {y} on {formula} found {found}, which is unreachable")
            )
            Test.Sync(
                "aPointFarBelowFindsAPositionInTheDenominator",
                fun () ->
                    let formula = MA.Frac(c 'b', c 'd') |> flat
                    let placed = edited formula
                    let found = (PlacedCurs.Nearest(placed, placed.Width / 2f, -placed.Descent)).ToMACurs
                    match found with
                    | MACurs.FracDen _ -> ()
                    | other -> failwith $"a point under the bar found {other}"
            )
        ]
    )

let tests =
    TestFolder(
        "Layout",
        [   measurement
            structures
            repertoire
            bigOperators
            brackets
            colours
            marks
            words
            grown
            stacks
            tables
            blackboard
            cursors
            roundTrip ])
