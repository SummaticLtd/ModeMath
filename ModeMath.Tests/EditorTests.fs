module ModeMath.Tests.EditorTests

open System.Collections.Immutable
open SimpleTests
open ModeMath

let private layout = Layout 20f<px>
let private opened(ma: MA) = Editor(layout, ma)
let private typed(editor: Editor, s: string) = s |> Seq.fold (fun (e: Editor) c -> e.Type c) editor
let private arr(xs: MA list) = xs.ToImmutableArray()

let private moved(direction: Direction, editor: Editor) =
    match editor.Move direction with
    | ValueSome moved -> moved
    | ValueNone -> failwith $"the cursor could not move {direction}"

let private editing =
    TestList(
        "Editing",
        [   Test.Sync(
                "typingBuildsTheFormulaTyped",
                fun () -> Assert.Equal(MA.String "abc", (typed(opened MA.Empty, "abc")).Formula)
            )
            Test.Sync(
                "typingAFunctionNameMakesTheFunction",
                fun () -> Assert.Equal(MA.Function MathFunction.Sin, (typed(opened MA.Empty, "sin")).Formula)
            )
            Test.Sync(
                "aCharacterGoesInWhereTheCursorWasPutByAClick",
                fun () ->
                    let editor = typed(opened MA.Empty, "ac")
                    let placed = editor.Placed
                    let midway =
                        match placed.Pma with
                        | PlacedMA.Row children -> children.[1].X
                        | other -> failwith $"not a row: {other}"
                    let clicked = editor.Click(midway, 0f<px>)
                    Assert.Equal(MA.String "abc", (clicked.Type 'b').Formula)
            )
            Test.Sync(
                "aFractionWithNoTermBeforeItLeavesTheCursorInItsNumerator",
                fun () ->
                    let editor = (opened MA.Empty).InsertFraction
                    Assert.Equal(MA.Frac(MA.Char '1', MA.Empty), (editor.Type '1').Formula)
            )
            Test.Sync(
                "aFractionTakesTheTermBeforeItUpIntoItsNumerator",
                fun () ->
                    let divided(before: string) =
                        (typed(typed(opened MA.Empty, before).InsertFraction, "c")).Formula
                    Assert.Equal(MA.Frac(MA.String "ab", MA.Char 'c'), divided "ab", "ab")
                    Assert.Equal(
                        MA.Row(arr [ MA.Char 'a'; MA.Char '+'; MA.Frac(MA.Char 'b', MA.Char 'c') ]),
                        divided "a+b",
                        "a term does not reach across what divides one")
                    let script = MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueNone)
                    Assert.Equal(
                        MA.Frac(script, MA.Char 'c'),
                        (typed((Editor.AtEnd(layout, script)).InsertFraction, "c")).Formula,
                        "a script is one atom, so the whole of it is taken up")
            )
            Test.Sync(
                "aScriptGoesOnTheOneAtomBeforeItRatherThanTheTerm",
                fun () ->
                    let editor = typed(opened MA.Empty, "ab").InsertSuperscript
                    Assert.Equal(
                        MA.Row(arr [ MA.Char 'a'; MA.ScriptSuper(MA.Char 'b', MA.Char '2', ValueNone) ]),
                        (typed(editor, "2")).Formula)
            )
            Test.Sync(
                "aFractionTakesUpABracketedGroupWhole",
                fun () ->
                    // Brackets face their neighbours as an opening and a closing atom, but a pair is one.
                    let bracketed = MA.RoundBracket(MA.String "x+1")
                    Assert.Equal(
                        MA.Frac(bracketed, MA.Char 'c'),
                        (typed((Editor.AtEnd(layout, bracketed)).InsertFraction, "c")).Formula)
            )
            Test.Sync(
                "anOpeningBracketLeavesItsClosingOneToBeTyped",
                fun () ->
                    let editor = (typed(opened MA.Empty, "abc")).InsertBracket(Brackets.Matching Bracket.Normal)
                    Assert.Equal(
                        MA.Row(
                            arr [
                                MA.String "abc"
                                MA.Bracketed(
                                    Brackets.Matching Bracket.Normal,
                                    MA.Char 'd',
                                    BracketCompletion.Left)
                            ]).Flatten,
                        (typed(editor, "d")).Formula)
            )
            Test.Sync(
                "aClosingBracketClosesTheGroupTheCursorStandsInAndStandsAfterIt",
                fun () ->
                    let editor = (typed(opened MA.Empty, "abc")).InsertBracket(Brackets.Matching Bracket.Normal)
                    let closed = (typed(editor, "d")).CloseBracket Bracket.Normal
                    Assert.Equal(
                        MA.Row(
                            arr [
                                MA.String "abc"
                                MA.RoundBracket(MA.Char 'd')
                            ]).Flatten,
                        closed.Formula,
                        "the group was not closed")
                    Assert.Equal(
                        MA.Row(
                            arr [
                                MA.String "abc"
                                MA.RoundBracket(MA.Char 'd')
                                MA.Char 'e'
                            ]).Flatten,
                        (closed.Type 'e').Formula,
                        "the cursor did not stand after the group")
            )
            Test.Sync(
                "anOpeningBracketWithNoGroupToOpenTakesWhatIsAfterItIn",
                fun () ->
                    let mutable start = typed(opened MA.Empty, "a+b")
                    for _ in 1 .. 3 do
                        start <- moved(Direction.Left, start)
                    let opening = start.InsertBracket(Brackets.Matching Bracket.Normal)
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Normal,
                            MA.String "a+b",
                            BracketCompletion.Left),
                        opening.Formula,
                        "the group did not take in what was after the cursor")
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Normal,
                            MA.String "xa+b",
                            BracketCompletion.Left),
                        (opening.Type 'x').Formula,
                        "the cursor did not stand at the start of the group")
            )
            Test.Sync(
                "anOpeningBracketGoesWhereTheCursorIsInAGroupWaitingForOne",
                fun () ->
                    // The tentative bracket is drawn at the start, but it is typed where it belongs.
                    let stray = (typed(opened MA.Empty, "a+b+c")).CloseBracket Bracket.Normal
                    let mutable before = stray
                    for _ in 1 .. 4 do
                        before <- moved(Direction.Left, before)
                    let opening = before.InsertBracket(Brackets.Matching Bracket.Normal)
                    Assert.Equal(
                        MA.Row(
                            arr [ MA.Char 'a'; MA.Char '+'; MA.RoundBracket(MA.String "b+c") ]).Flatten,
                        opening.Formula,
                        "the group did not take its opening bracket at the cursor")
                    Assert.Equal(
                        MA.Row(
                            arr [ MA.Char 'a'; MA.Char '+'; MA.RoundBracket(MA.String "xb+c") ]).Flatten,
                        (opening.Type 'x').Formula,
                        "the cursor did not stand inside the group it opened")
            )
            Test.Sync(
                "anOpeningBracketOpensAGroupOfItsOwnFromInsideAnAtomOfOne",
                fun () ->
                    // The group is waiting for a bracket, but a slot inside one of its atoms cannot
                    // be split from it, so a group is opened in that slot instead.
                    let divided = typed((typed(opened MA.Empty, "1")).InsertFraction, "2")
                    let group = (typed(moved(Direction.Right, divided), "x")).CloseBracket Bracket.Normal
                    let mutable inside = group
                    for _ in 1 .. 4 do
                        inside <- moved(Direction.Left, inside)
                    let opened = inside.InsertBracket(Brackets.Matching Bracket.Normal)
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Normal,
                            MA.Row2(
                                MA.Frac(
                                    MA.Char '1',
                                    MA.Bracketed(
                                        Brackets.Matching Bracket.Normal,
                                        MA.Char '2',
                                        BracketCompletion.Left)),
                                MA.Char 'x'),
                            BracketCompletion.Right),
                        opened.Formula)
            )
            Test.Sync(
                "aClosingBracketGoesWhereTheCursorIsInAGroupWaitingForOne",
                fun () ->
                    // The tentative bracket is drawn at the end, but it is typed where it belongs.
                    let waiting =
                        typed((opened MA.Empty).InsertBracket(Brackets.Matching Bracket.Normal), "a+b+c")
                    let mutable before = waiting
                    for _ in 1 .. 2 do
                        before <- moved(Direction.Left, before)
                    let closed = before.CloseBracket Bracket.Normal
                    Assert.Equal(
                        MA.Row(
                            arr [ MA.RoundBracket(MA.String "a+b"); MA.Char '+'; MA.Char 'c' ]).Flatten,
                        closed.Formula,
                        "the group did not take its closing bracket at the cursor")
                    Assert.Equal(
                        MA.Row(
                            arr [
                                MA.RoundBracket(MA.String "a+b")
                                MA.Char 'x'
                                MA.Char '+'
                                MA.Char 'c'
                            ]).Flatten,
                        (closed.Type 'x').Formula,
                        "the cursor did not stand after the group")
            )
            Test.Sync(
                "aClosingBracketLeavesOneAlreadyTypedWhereItStands",
                fun () ->
                    // Only a tentative bracket is still to be placed; a typed one has been.
                    let group = MA.RoundBracket(MA.String "ab")
                    let mutable inside = opened group
                    for _ in 1 .. 2 do
                        inside <- moved(Direction.Right, inside)
                    let closed = inside.CloseBracket Bracket.Normal
                    Assert.Equal(group, closed.Formula, "the closing bracket moved to the cursor")
                    Assert.Equal(
                        MA.Row2(group, MA.Char 'x'),
                        (closed.Type 'x').Formula,
                        "the cursor did not stand after the group")
            )
            Test.Sync(
                "aClosingBracketNeverWritesOverOneAlreadyThere",
                fun () ->
                    let round = MA.RoundBracket(MA.String "xy")
                    let mutable inside = opened round
                    for _ in 1 .. 3 do
                        inside <- moved(Direction.Right, inside)
                    Assert.Equal(
                        MA.RoundBracket(
                            MA.Bracketed(
                                Brackets.Matching Bracket.Square,
                                MA.String "xy",
                                BracketCompletion.Right)),
                        (inside.CloseBracket Bracket.Square).Formula,
                        "the closing bracket already there was written over")
                    // The group the cursor stands in is closed, so the bar belongs to the one around it.
                    let bars = MA.Bracketed(Brackets.Matching Bracket.Line, round, BracketCompletion.Left)
                    let mutable barred = opened bars
                    for _ in 1 .. 3 do
                        barred <- moved(Direction.Right, barred)
                    Assert.Equal(
                        MA.Bracketed(Brackets.Matching Bracket.Line, round, BracketCompletion.Completed),
                        (barred.InsertBar Bracket.Line).Formula,
                        "the bar closed the group it stood in rather than the one waiting")
            )
            Test.Sync(
                "aBracketTypedPastAGroupWaitingForOneIsTheOneItWaitedFor",
                fun () ->
                    let group = MA.String "a+b+c"
                    let waiting =
                        moved(
                            Direction.Right,
                            typed((opened MA.Empty).InsertBracket(Brackets.Matching Bracket.Normal), "a+b+c"))
                    Assert.Equal(
                        MA.RoundBracket group,
                        (waiting.CloseBracket Bracket.Normal).Formula,
                        "a closing bracket past a group waiting for one")
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets(Bracket.Normal, Bracket.Square),
                            group,
                            BracketCompletion.Completed),
                        (waiting.CloseBracket Bracket.Square).Formula,
                        "the bracket typed rather than the one drawn tentative")
                    let stray = opened((typed(opened MA.Empty, "a+b+c")).CloseBracket(Bracket.Normal).Formula)
                    Assert.Equal(
                        MA.RoundBracket group,
                        (stray.InsertBracket(Brackets.Matching Bracket.Normal)).Formula,
                        "an opening bracket before a group waiting for one")
            )
            Test.Sync(
                "anythingPutPastATentativeBracketSettlesIt",
                fun () ->
                    let group = MA.String "a+b+c"
                    let waiting =
                        moved(
                            Direction.Right,
                            typed((opened MA.Empty).InsertBracket(Brackets.Matching Bracket.Normal), "a+b+c"))
                    Assert.Equal(
                        MA.Row2(MA.RoundBracket group, MA.Char '+'),
                        (waiting.Type '+').Formula,
                        "a group waiting for its closing bracket")
                    let stray = opened((typed(opened MA.Empty, "a+b+c")).CloseBracket(Bracket.Normal).Formula)
                    Assert.Equal(
                        MA.Row2(MA.Char '+', MA.RoundBracket group),
                        (stray.Type '+').Formula,
                        "a group waiting for its opening bracket")
            )
            Test.Sync(
                "aClosingBracketNeedNotBeTheOneTheGroupWasOpenedWith",
                fun () ->
                    let editor = (opened MA.Empty).InsertBracket(Brackets.Matching Bracket.Square)
                    let closed = (typed(editor, "0")).CloseBracket Bracket.Normal
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets(Bracket.Square, Bracket.Normal),
                            MA.Char '0',
                            BracketCompletion.Completed),
                        closed.Formula)
            )
            Test.Sync(
                "aClosingBracketReachesOutOfASlotToTheGroupAroundIt",
                fun () ->
                    // The cursor stands in the denominator, and the bracket to close is outside it.
                    let editor = (opened MA.Empty).InsertBracket(Brackets.Matching Bracket.Normal)
                    let divided = typed((typed(editor, "1")).InsertFraction, "2")
                    Assert.Equal(
                        MA.RoundBracket(MA.Frac(MA.Char '1', MA.Char '2')),
                        (divided.CloseBracket Bracket.Normal).Formula)
            )
            Test.Sync(
                "aClosingBracketWithNoGroupToCloseTakesWhatIsBeforeItIn",
                fun () ->
                    let closed = (typed(opened MA.Empty, "abc")).CloseBracket Bracket.Normal
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Normal,
                            MA.String "abc",
                            BracketCompletion.Right),
                        closed.Formula)
            )
            Test.Sync(
                "aBarClosesTheGroupItStandsInRatherThanOpeningAnother",
                fun () ->
                    let opening = (opened MA.Empty).InsertBar Bracket.Line
                    let filled = typed(opening, "x")
                    Assert.Equal(
                        MA.Bracketed(Brackets.Matching Bracket.Line, MA.Char 'x', BracketCompletion.Left),
                        filled.Formula,
                        "the first bar did not open a group")
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Line,
                            MA.Char 'x',
                            BracketCompletion.Completed),
                        (filled.InsertBar Bracket.Line).Formula,
                        "the second bar did not close it")
            )
            Test.Sync(
                "aBarInsideAGroupAlreadyClosedOpensAnotherRatherThanClosingItTwice",
                fun () ->
                    let bars = MA.Bracketed(Brackets.Matching Bracket.Line, MA.Char 'x', BracketCompletion.Completed)
                    let inside = moved(Direction.Right, opened bars)
                    let barred = (inside.InsertBar Bracket.Line).Formula
                    Assert.Equal(
                        MA.Bracketed(
                            Brackets.Matching Bracket.Line,
                            MA.Bracketed(
                                Brackets.Matching Bracket.Line,
                                MA.Char 'x',
                                BracketCompletion.Left),
                            BracketCompletion.Completed),
                        barred)
            )
            Test.Sync(
                "aSquareRootLeavesTheCursorInsideIt",
                fun () ->
                    let editor = (opened MA.Empty).InsertSqrt
                    Assert.Equal(MA.Sqrt(MA.Char 'x'), (editor.Type 'x').Formula)
            )
            Test.Sync(
                "aSuperscriptGoesOnWhatTheCursorStandsAfter",
                fun () ->
                    let editor = (typed(opened MA.Empty, "x")).InsertSuperscript
                    Assert.Equal(
                        MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueNone),
                        (editor.Type '2').Formula)
            )
            Test.Sync(
                "aSubscriptJoinsTheSuperscriptTheAtomAlreadyCarries",
                fun () ->
                    let squared = Editor.AtEnd(layout, MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueNone))
                    Assert.Equal(
                        MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueSome(MA.Char 'i')),
                        (squared.InsertSubscript.Type 'i').Formula)
            )
            Test.Sync(
                "aSuperscriptJoinsTheSubscriptTheAtomAlreadyCarries",
                fun () ->
                    let indexed = Editor.AtEnd(layout, MA.ScriptSub(MA.Char 'x', MA.Char 'i'))
                    Assert.Equal(
                        MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueSome(MA.Char 'i')),
                        (indexed.InsertSuperscript.Type '2').Formula)
            )
            Test.Sync(
                "aSecondSuperscriptGoesIntoTheOneAlreadyThere",
                fun () ->
                    let squared = Editor.AtEnd(layout, MA.ScriptSuper(MA.Char 'x', MA.Char '2', ValueNone))
                    Assert.Equal(
                        MA.ScriptSuper(MA.Char 'x', MA.String "23", ValueNone),
                        (squared.InsertSuperscript.Type '3').Formula)
            )
            Test.Sync(
                "aScriptWithNothingBeforeItIsSetOnAnEmptySlot",
                fun () ->
                    let editor = (opened MA.Empty).InsertSuperscript
                    Assert.Equal(
                        MA.ScriptSuper(MA.Empty, MA.Char '2', ValueNone),
                        (editor.Type '2').Formula)
            )
            Test.Sync(
                "aFormulaPutInAtTheCursorIsLeftBehindIt",
                fun () ->
                    let editor = (opened MA.Empty).Insert(MA.String "xy")
                    Assert.Equal(MA.String "xyz", (editor.Type 'z').Formula)
            )
            Test.Sync(
                "backspaceTakesBackWhatWasTyped",
                fun () ->
                    match (typed(opened MA.Empty, "abc")).BackSpace with
                    | ValueSome editor -> Assert.Equal(MA.String "ab", editor.Formula)
                    | ValueNone -> Assert.Fail "backspace found nothing to take back"
            )
            Test.Sync(
                "aClickAtAPointThatIsNoNumberKeepsTheFormula",
                fun () ->
                    // A caller working the point out through a scale of zero must not lose the lot.
                    let editor = typed(opened MA.Empty, "ab")
                    let nowhere = System.Single.NaN * 1f<px>
                    Assert.Equal(MA.String "zab", ((editor.Click(nowhere, nowhere)).Type 'z').Formula)
                    let far = System.Single.PositiveInfinity * 1f<px>
                    Assert.Equal(MA.String "zab", ((editor.Click(far, 0f<px>)).Type 'z').Formula)
            )
            Test.Sync(
                "aKeyWithNothingToDoIsPassedOn",
                fun () ->
                    let empty = opened MA.Empty
                    Assert.True(empty.BackSpace.IsNone, "backspace on an empty formula")
                    Assert.True(empty.Delete.IsNone, "delete on an empty formula")
                    let atTheEnd = typed(opened MA.Empty, "ab")
                    Assert.True((atTheEnd.Move Direction.Right).IsNone, "right at the end")
                    Assert.True((opened(MA.String "ab")).Move(Direction.Left).IsNone, "left at the start")
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
                    let editor = typed(opened MA.Empty, "abc")
                    let after = moved(Direction.Right, moved(Direction.Left, editor))
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
                        not (obj.ReferenceEquals(editor.Placed.Pma, (editor.Type 'd').Placed.Pma)),
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
                    let editor = (typed(opened MA.Empty, "ab")).InsertFraction
                    let filled = typed(editor, "cd")
                    Assert.Equal(filled.Formula.Flatten, filled.Formula, "the formula held a nested row")
            )
        ]
    )

let tests = TestFolder("Editor", [ editing; laying ])
