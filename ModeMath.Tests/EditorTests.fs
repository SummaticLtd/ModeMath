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
                "aFractionLeavesTheCursorInItsNumerator",
                fun () ->
                    let editor = (opened MA.Empty).InsertFraction
                    Assert.Equal(MA.Frac(MA.Char '1', MA.Empty), (editor.Type '1').Formula)
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
