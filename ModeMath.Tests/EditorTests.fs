module ModeMath.Tests.EditorTests

open System.Collections.Immutable
open SimpleTests
open ModeMath

let private layout = Layout 20f<px>
let private opened(ma: MA) = Editor(layout, ma)
let private arr(xs: MA list) = xs.ToImmutableArray()

/// Which key a character in a case stands for. The arrows are < and >, backspace and delete the
/// marks on their own keys, and the rest are the characters themselves.
let private key(character: char) =
    match character with
    | '(' -> MathKey.Open BracketKey.Round
    | '[' -> MathKey.Open BracketKey.Square
    | ')' -> MathKey.Close BracketKey.Round
    | ']' -> MathKey.Close BracketKey.Square
    | '|' -> MathKey.Bar
    | '/' -> MathKey.Fraction
    | '\u221A' -> MathKey.Sqrt
    | '\u221B' -> MathKey.Root
    | '^' -> MathKey.Superscript
    | '_' -> MathKey.Subscript
    | '<' -> MathKey.Move Direction.Left
    | '>' -> MathKey.Move Direction.Right
    | '\u232B' -> MathKey.Backspace
    | '\u2326' -> MathKey.Delete
    | _ -> MathKey.Character character

let private pressed(editor: Editor, character: char) =
    // The cursor put back at the start, which is opening the formula afresh rather than a key.
    if character = '\u2196' then opened editor.Formula
    else
        match editor.Press(key character) with
        | ValueSome pressed -> pressed
        | ValueNone -> failwith $"{character} had nothing to do"

let private typing(editor: Editor, c: char) = pressed(editor, c)
let private typed(editor: Editor, s: string) = s |> Seq.fold (fun e c -> pressed(e, c)) editor

/// The formula the keys leave behind, starting from nothing.
let private pressing(keys: string) =
    keys |> Seq.fold (fun editor key -> pressed(editor, key)) (opened MA.Empty)

/// A formula written out for a test to compare against, a bracket still waiting for its pair
/// marked with the ~ it is drawn faint by.
let rec private spell(ma: MA) =
    match ma with
    | MA.Row elements -> elements |> Seq.map spell |> String.concat ""
    // The marks the spelling uses are keys of their own, save these, which would read as spelling.
    | MA.Char('{' | '}' | '~' as c) -> failwith $"the tests cannot spell a literal {c}"
    | MA.Char c -> string c
    | MA.Function f -> $"fn{{{MathFunctions.name f}}}"
    | MA.Frac(numerator, denominator) -> $"frac{{{spell numerator}}}{{{spell denominator}}}"
    | MA.Sqrt x -> $"sqrt{{{spell x}}}"
    | MA.RootN(degree, radicand) -> $"root{{{spell degree}}}{{{spell radicand}}}"
    | MA.ScriptSuper(main, super, ValueNone) -> $"{carrying main}^{{{spell super}}}"
    | MA.ScriptSuper(main, super, ValueSome sub) ->
        $"{carrying main}^{{{spell super}}}_{{{spell sub}}}"
    | MA.ScriptSub(main, sub) -> $"{carrying main}_{{{spell sub}}}"
    | MA.Bracketed(brackets, inner, completion) ->
        let side(bracket: Bracket, opening: bool, completed: bool) =
            let drawn =
                match bracket, opening with
                | Bracket.Normal, true -> "("
                | Bracket.Normal, false -> ")"
                | Bracket.Square, true -> "["
                | Bracket.Square, false -> "]"
                | Bracket.Line, _ -> "|"
                | other, _ -> failwith $"the tests cannot spell {other}"
            if completed then drawn else $"~{drawn}~"
        side(brackets.Left, true, completion.LeftCompleted)
        + spell inner
        + side(brackets.Right, false, completion.RightCompleted)
    | other -> failwith $"the tests cannot spell {other}"

/// What a script goes on, braced where it is more than the one atom, so that {ab}^2 is not read
/// as a^2 beside b.
and private carrying(ma: MA) =
    match ma with
    | MA.Row elements when elements.Length = 1 -> carrying elements.[0]
    | MA.Row _ | MA.ScriptSuper _ | MA.ScriptSub _ -> $"{{{spell ma}}}"
    | _ -> spell ma

/// The formula the keys leave behind, written out.
let private after(keys: string) = spell (pressing keys).Formula

let private editing =
    TestList(
        "Editing",
        [   Test.CasesSync(
                "theKeysPressedBuildTheFormula",
                [
                    "typingBuildsTheFormulaTyped", ("abc", "abc")
                    "typingAFunctionNameMakesTheFunction", ("sin", "fn{sin}")
                    "aLongerNameIsTakenOverAShorterOneItStartsWith", ("sinh", "fn{sinh}")
                    "andOverOneAlreadyRecognised", ("sech", "fn{sech}")
                    "typingSqrtLeavesTheCursorInsideTheRadical", ("sqrtx", "sqrt{x}")
                    "typingDegreeGivesTheMark", ("90degree", "90°")
                    "aSquareRootLeavesTheCursorInsideIt", ("\u221Ax", "sqrt{x}")
                    "aRootLeavesTheCursorInItsDegree", ("\u221B3", "root{3}{}")
                    "andTheRadicandComesAfterIt", ("\u221B3>x", "root{3}{x}")
                    "backspaceTakesBackWhatWasTyped", ("abc\u232B", "ab")
                    "deleteTakesBackWhatIsAhead", ("abc<\u2326", "ab")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.Sync(
                // Recognising the shorter name first would strand the letters that follow it.
                "everyNameStartingWithOneRecognisedIsRecognisedItself",
                fun () ->
                    let spellings =
                        SpelledNames.table |> Seq.map (fun struct(spelling, _) -> spelling) |> Set.ofSeq
                    let extends(name: string) =
                        spellings
                        |> Set.exists (fun s -> s <> name && name.StartsWith(s, System.StringComparison.Ordinal))
                    for struct(name, f) in MathFunctions.named do
                        if extends name then
                            Assert.Equal(spell (MA.Function f), after name, $"{name} was left as its letters")
            )
            Test.CasesSync(
                "aFractionTakesUpTheTermBeforeIt",
                [
                    "withNoTermBeforeItTheCursorStartsInTheNumerator", ("/1", "frac{1}{}")
                    "aRunOfLettersIsOneTerm", ("ab/c", "frac{ab}{c}")
                    "aTermDoesNotReachAcrossWhatDividesOne", ("a+b/c", "a+frac{b}{c}")
                    "aScriptIsOneAtomSoTheWholeOfItIsTakenUp", ("x^2>/c", "frac{x^{2}}{c}")
                    // Brackets face their neighbours as an opening and a closing atom, but a pair is one.
                    "aBracketedGroupIsTakenUpWhole", ("(x+1)/c", "frac{(x+1)}{c}")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.CasesSync(
                "aScriptGoesOnTheAtomBeforeIt",
                [
                    "onTheOneAtomBeforeItRatherThanTheTerm", ("ab^2", "ab^{2}")
                    "aSuperscriptGoesOnWhatTheCursorStandsAfter", ("x^2", "x^{2}")
                    "aSubscriptJoinsTheSuperscriptTheAtomCarries", ("x^2>_i", "x^{2}_{i}")
                    "aSuperscriptJoinsTheSubscriptTheAtomCarries", ("x_i>^2", "x^{2}_{i}")
                    "aSecondSuperscriptGoesIntoTheOneAlreadyThere", ("x^2>^3", "x^{23}")
                    "aScriptWithNothingBeforeItIsSetOnAnEmptySlot", ("^2", "{}^{2}")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.CasesSync(
                "aBracketGoesWhereItIsTyped",
                [
                    "anOpeningBracketLeavesItsClosingOneToBeTyped", ("abc(d", "abc(d~)~")
                    "aClosingBracketClosesTheGroupTheCursorStandsIn", ("abc(d)", "abc(d)")
                    "andTheCursorThenStandsAfterIt", ("abc(d)e", "abc(d)e")
                    "anOpeningBracketWithNoGroupToOpenTakesWhatIsAfterItIn", ("a+b<<<(", "(a+b~)~")
                    "andTheCursorThenStandsAtTheStartOfIt", ("a+b<<<(x", "(xa+b~)~")
                    // The tentative bracket is drawn at the end, but it is typed where it belongs.
                    "aClosingBracketGoesWhereTheCursorIsInAGroupWaitingForOne", ("(a+b+c<<)", "(a+b)+c")
                    "andTheCursorThenStandsAfterTheGroup", ("(a+b+c<<)x", "(a+b)x+c")
                    // The tentative bracket is drawn at the start, but it is typed where it belongs.
                    "anOpeningBracketGoesWhereTheCursorIsInAGroupWaitingForOne", ("a+b+c)<<<<(", "a+(b+c)")
                    "andTheCursorThenStandsInsideTheGroup", ("a+b+c)<<<<(x", "a+(xb+c)")
                    "aClosingBracketNeedNotBeTheOneTheGroupWasOpenedWith", ("[0)", "[0)")
                    // The cursor stands in the denominator, and the bracket to close is outside it.
                    "aClosingBracketReachesOutOfASlotToTheGroupAroundIt", ("(1/2)", "(frac{1}{2})")
                    "aClosingBracketWithNoGroupToCloseTakesWhatIsBeforeItIn", ("abc)", "~(~abc)")
                    // Only a tentative bracket is still to be placed; a typed one has been.
                    "aClosingBracketLeavesOneAlreadyTypedWhereItStands", ("(ab)<<)", "(ab)")
                    "andTheCursorThenStandsAfterThatGroup", ("(ab)<<)x", "(ab)x")
                    "aClosingBracketNeverWritesOverOneAlreadyThere", ("(xy)<]", "(~[~xy])")
                    // A group waiting for a bracket cannot be split from a slot inside one of its atoms.
                    "anOpeningBracketOpensAGroupOfItsOwnFromInsideAnAtomOfOne",
                        ("1/2>x)<<<<(", "~(~frac{1}{(2~)~}x)")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.CasesSync(
                "aBracketTypedPastAGroupWaitingForOneIsTheOneItWaitedFor",
                [
                    "aClosingBracketPastAGroupWaitingForOne", ("(a+b+c>)", "(a+b+c)")
                    "theBracketTypedRatherThanTheOneDrawnTentative", ("(a+b+c>]", "(a+b+c]")
                    "anOpeningBracketBeforeAGroupWaitingForOne", ("a+b+c)\u2196(", "(a+b+c)")
                    "anythingPutPastAGroupWaitingForItsClosingBracketSettlesIt", ("(a+b+c>+", "(a+b+c)+")
                    "anythingPutBeforeAGroupWaitingForItsOpeningBracketSettlesIt",
                        ("a+b+c)\u2196+", "+(a+b+c)")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.CasesSync(
                "aBarOpensAGroupOrClosesTheOneItStandsIn",
                [
                    "theFirstBarOpensAGroup", ("|x", "|x~|~")
                    "theSecondClosesIt", ("|x|", "|x|")
                    // The group the cursor stands in is closed, so the bar belongs to the one around it.
                    "aBarClosesTheGroupWaitingRatherThanOneAlreadyClosed", ("|(xy)<|", "|(xy)|")
                    "aBarInsideAGroupAlreadyClosedOpensAnother", ("|x|<<|", "||x~|~|")
                ],
                fun (keys, expected) -> Assert.Equal(expected, after keys, keys)
            )
            Test.Sync(
                "aCharacterGoesInWhereTheCursorWasPutByAClick",
                fun () ->
                    let editor = pressing "ac"
                    let midway =
                        match editor.Placed.Pma with
                        | PlacedMA.Row children -> children.[1].X
                        | other -> failwith $"not a row: {other}"
                    Assert.Equal("abc", spell (typing(editor.Click(midway, 0f<px>), 'b')).Formula)
            )
            Test.Sync(
                "aCharacterTheFontCannotDrawIsNotTypedAtAll",
                fun () ->
                    // Move gives the key back the same way where there is nowhere to go.
                    let editor = pressing "ab"
                    Assert.Equal(
                        ValueNone,
                        editor.Press(MathKey.Character '\u2603'),
                        "a key with no glyph was taken in")
                    Assert.Equal("abc", after "abc", "an ordinary key was not")
            )
            Test.Sync(
                "aFormulaPutInAtTheCursorIsLeftBehindIt",
                fun () ->
                    let editor = (opened MA.Empty).Insert(MA.String "xy")
                    Assert.Equal("xyz", spell (typing(editor, 'z')).Formula)
            )
            Test.Sync(
                "aClickAtAPointThatIsNoNumberKeepsTheFormula",
                fun () ->
                    // A caller working the point out through a scale of zero must not lose the lot.
                    let editor = pressing "ab"
                    let nowhere = System.Single.NaN * 1f<px>
                    Assert.Equal("zab", spell (typing(editor.Click(nowhere, nowhere), 'z')).Formula)
                    let far = System.Single.PositiveInfinity * 1f<px>
                    Assert.Equal("zab", spell (typing(editor.Click(far, 0f<px>), 'z')).Formula)
            )
            Test.Sync(
                "aKeyWithNothingToDoIsPassedOn",
                fun () ->
                    let empty = opened MA.Empty
                    Assert.True(empty.Press(MathKey.Backspace).IsNone, "backspace on an empty formula")
                    Assert.True(empty.Press(MathKey.Delete).IsNone, "delete on an empty formula")
                    let atTheEnd = pressing "ab"
                    Assert.True(atTheEnd.Press(MathKey.Move Direction.Right).IsNone, "right at the end")
                    Assert.True(
                        (opened(MA.String "ab")).Press(MathKey.Move Direction.Left).IsNone,
                        "left at the start")
            )
        ]
    )

let private laying =
    TestList(
        "Laying out",
        [   Test.Sync(
                "movingTheCursorLaysNothingOut",
                fun () ->
                    // The formula does not change, so the tree it was laid out as is kept as it is.
                    let editor = pressing "abc"
                    let after = pressed(pressed(editor, '<'), '>')
                    Assert.True(
                        obj.ReferenceEquals(editor.Placed.Pma, after.Placed.Pma),
                        "moving the cursor laid the formula out again")
            )
            Test.Sync(
                "clickingLaysNothingOut",
                fun () ->
                    let editor = typed(opened MA.Empty, "abc")
                    let clicked = editor.Click(editor.Placed.Width, 0f<px>)
                    Assert.True(
                        obj.ReferenceEquals(editor.Placed.Pma, clicked.Placed.Pma),
                        "clicking laid the formula out again")
            )
            Test.Sync(
                "typingLaysTheFormulaOutAgain",
                fun () ->
                    let editor = typed(opened MA.Empty, "abc")
                    Assert.True(
                        not (obj.ReferenceEquals(editor.Placed.Pma, typing(editor, 'd').Placed.Pma)),
                        "a character was typed without the formula being laid out again")
            )
            Test.Sync(
                "aFormulaIsSetTheSameWithACursorInItAsWithout",
                fun () ->
                    // The cursor is flattened with the formula, so neither is spaced as a nested row.
                    // The plus ends the inner row, so unflattened it binds nothing and is set close.
                    let nested = MA.Row(arr [ MA.Row(arr [ MA.Char 'a'; MA.Char '+' ]); MA.Char 'b' ])
                    let bare = layout.Of nested
                    for curs in MACurs.Positions(nested.Flatten) do
                        let cursored = (layout.Of curs).Placed
                        Assert.True(
                            abs (bare.Width - cursored.Width) < 0.01f<px>,
                            $"{bare.Width} against {cursored.Width} at {curs}")
                    let unflattened = layout.Of(MACurs.AtStart nested)
                    Assert.True(
                        abs (bare.Width - unflattened.Placed.Width) < 0.01f<px>,
                        $"a cursor over an unflattened formula set it at {unflattened.Placed.Width}")
            )
            Test.Sync(
                "whatIsEditedStaysFlat",
                fun () ->
                    let filled = pressing "ab/cd"
                    Assert.Equal(filled.Formula.Flatten, filled.Formula, "the formula held a nested row")
            )
        ]
    )

let tests = TestFolder("Editor", [ editing; laying ])
