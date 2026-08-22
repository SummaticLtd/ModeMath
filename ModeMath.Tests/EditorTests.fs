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
