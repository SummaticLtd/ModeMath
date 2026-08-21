module ModeMath.Tests.CursorTests

open System.Collections.Immutable
open SimpleTests
open ModeMath

let private typed (s: string) =
    s |> Seq.fold (fun (m: MICurs) c -> m.AddAlphanumeric c) MICurs.CursorOrEmpty

let private backspaced (m: MICurs) =
    match m.BackSpace with
    | Choice1Of2 m -> m
    | Choice2Of2 ma -> failwith $"cursor fell off the left, leaving {ma}"

let private deleted (m: MICurs) =
    match m.Delete with
    | Choice1Of2 m -> m
    | Choice2Of2 ma -> failwith $"cursor fell off the right, leaving {ma}"

let private moved(direction: Direction, m: MICurs) =
    match m.Move direction with
    | ValueSome m -> m
    | ValueNone -> failwith $"could not move {direction} from {m}"

let private arr (xs: MA list) = xs.ToImmutableArray()

let private n = MA.Char 'n'
let private d = MA.Char 'd'
let private frac = MA.Frac(n, d)

let private walkRight(ma: MA) = MICurs.Positions ma |> List.ofSeq

let private walkLeft(ma: MA) =
    let rec loop(acc: MICurs list, m: MICurs) =
        match m.Left with
        | ValueSome next -> loop(next :: acc, next)
        | ValueNone -> List.rev acc
    let start = MICurs.AtEnd ma
    start :: loop([], start)

let private sum = MA.BigOp(BigOperator.Sum, ValueSome(MA.String "n=1"), ValueSome(MA.Char 'm'))

let private sampleFormulas =
    [
        MA.Char 'a'
        MA.String "abc"
        frac
        MA.Sqrt(MA.String "xy")
        MA.ScriptSuper(MA.Char 'e', MA.Char '2', ValueNone)
        MA.ScriptSuper(MA.Char 'e', MA.Char '2', ValueSome(MA.Char '3'))
        MA.Row(arr [ MA.Char 'a'; frac; MA.Char 'b' ])
        MA.RootN(MA.Char '3', MA.Char 'x')
        MA.Bracketed(Brackets.Matching Bracket.Normal, MA.String "ab", BracketCompletion.Completed)
        sum
        MA.Row(arr [ MA.Char 'a'; sum; MA.Char 'b' ])
    ]

let private editing =
    TestList(
        "Editing",
        [   Test.Sync(
                "typingProducesRowOfCharacters",
                fun () -> Assert.Equal(MA.String "xyz", (typed "xyz").ToMA)
            )
            Test.Sync(
                "backspaceRemovesLastCharacter",
                fun () -> Assert.Equal(typed "xy", backspaced (typed "xyz"))
            )
            Test.Sync(
                "backspaceOfSingleCharacterEmptiesFormula",
                fun () -> Assert.Equal(MICurs.CursorOrEmpty, backspaced (typed "x"))
            )
            Test.Sync(
                "backspaceOnEmptyFormulaFallsOffLeft",
                fun () ->
                    match MICurs.CursorOrEmpty.BackSpace with
                    | Choice2Of2 ma -> Assert.Equal(MA.Empty, ma)
                    | Choice1Of2 m -> Assert.Fail($"expected Choice2, got {m}")
            )
            Test.CasesSync(
                "typingThenBackspaceIsIdentity",
                [ "x", "x"; "xy", "xy"; "q1", "q1" ],
                fun s -> Assert.Equal(typed s, backspaced ((typed s).AddAlphanumeric 'w'))
            )
            Test.Sync(
                "trailingFunctionNameBecomesFunction",
                fun () -> Assert.Equal(MA.Function MathFunction.Sin, (typed "sin").ToMA)
            )
            Test.Sync(
                "longestFunctionNameWins",
                fun () -> Assert.Equal(MA.Function MathFunction.Asin, (typed "asin").ToMA)
            )
            Test.Sync(
                "functionNameRecognisedOnlyAtEndOfLetterRun",
                fun () ->
                    Assert.Equal(MA.Row2(MA.Char '2', MA.Function MathFunction.Cos), (typed "2cos").ToMA)
            )
            Test.Sync(
                "backspaceAtStartOfDenominatorDissolvesFraction",
                fun () ->
                    let cursored = MICurs.FracDen(n, MICurs.AtStart d)
                    Assert.Equal(frac, cursored.ToMA)
                    Assert.Equal(MICurs.Between(n, d), backspaced cursored)
            )
            Test.Sync(
                "backspaceAtStartOfSuperscriptKeepsSubscriptOnTheBase",
                fun () ->
                    let x = MA.Char 'x'
                    let two = MA.Char '2'
                    let three = MA.Char '3'
                    let cursored = MICurs.ScriptSuper(x, MICurs.AtStart two, ValueSome three)
                    Assert.Equal(MA.ScriptSuper(x, two, ValueSome three), cursored.ToMA)
                    Assert.Equal(MICurs.Between(MA.ScriptSub(x, three), two), backspaced cursored)
            )
            Test.Sync(
                "backspaceAtStartOfSubscriptKeepsSuperscriptOnTheBase",
                fun () ->
                    let x = MA.Char 'x'
                    let two = MA.Char '2'
                    let three = MA.Char '3'
                    let cursored = MICurs.ScriptSub(x, ValueSome two, MICurs.AtStart three)
                    Assert.Equal(MA.ScriptSuper(x, two, ValueSome three), cursored.ToMA)
                    Assert.Equal(MICurs.Between(MA.ScriptSuper(x, two, ValueNone), three), backspaced cursored)
            )
            Test.Sync(
                "aLargeOperatorOffersNoPositionInsideItsLimits",
                fun () ->
                    Assert.Equal(
                        (walkRight(MA.Char 'a')).Length,
                        (walkRight sum).Length,
                        "a large operator is stepped over like a single character")
            )
            Test.Sync(
                "backspaceRemovesAWholeLargeOperator",
                fun () ->
                    let cursored = MICurs.AtEnd(MA.Row2(MA.Char 'x', sum))
                    Assert.Equal(MA.Char 'x', (backspaced cursored).ToMA, "limits and all")
            )
            Test.Sync(
                "deleteRemovesAWholeLargeOperator",
                fun () ->
                    let cursored = MICurs.AtStart(MA.Row2(sum, MA.Char 'x'))
                    Assert.Equal(MA.Char 'x', (deleted cursored).ToMA, "limits and all")
            )
            Test.Sync(
                "backspaceInsideSquareRootDeletesWithinIt",
                fun () ->
                    let cursored = MICurs.Sqrt(MICurs.AtEnd(MA.String "xy"))
                    Assert.Equal(MA.Sqrt(MA.Char 'x'), (backspaced cursored).ToMA)
            )
            Test.Sync(
                "addMICursSubstitutesAtCursorAndLeavesCursorInside",
                fun () ->
                    let result = (typed "x").AddMICurs(MICurs.Sqrt MICurs.CursorOrEmpty)
                    Assert.Equal(MA.Row2(MA.Char 'x', MA.Sqrt MA.Empty), result.ToMA)
                    Assert.Equal(MA.Row2(MA.Char 'x', MA.Sqrt(MA.Char 'y')), (result.AddAlphanumeric 'y').ToMA)
            )
            Test.Sync(
                "deleteRemovesNextCharacter",
                fun () ->
                    Assert.Equal(MICurs.AtStart(MA.String "yz"), deleted (MICurs.AtStart(MA.String "xyz")))
            )
            Test.Sync(
                "deleteAtEndFallsOffRight",
                fun () ->
                    match (MICurs.AtEnd(MA.Char 'x')).Delete with
                    | Choice2Of2 ma -> Assert.Equal(MA.Char 'x', ma)
                    | Choice1Of2 m -> Assert.Fail($"expected Choice2, got {m}")
            )
            Test.Sync(
                "deleteAtEndOfNumeratorDissolvesFraction",
                fun () ->
                    Assert.Equal(MICurs.Between(n, d), deleted (MICurs.FracNum(MICurs.AtEnd n, d)))
            )
            Test.Sync(
                "deleteRemovesTheWholeFraction",
                fun () -> Assert.Equal(MICurs.CursorOrEmpty, deleted (MICurs.AtStart frac))
            )
            Test.Sync(
                "backspaceRemovesTheWholeFraction",
                fun () -> Assert.Equal(MICurs.CursorOrEmpty, backspaced (MICurs.AtEnd frac))
            )
            Test.Sync(
                "backspaceRemovesAFractionLeavingWhatPrecedesIt",
                fun () ->
                    let x = MA.Char 'x'
                    let cursored = MICurs.AtEnd(MA.Row(arr [ x; frac ]))
                    Assert.Equal(MICurs.AtEnd x, backspaced cursored)
            )
        ]
    )

let private navigation =
    TestList(
        "Navigation",
        [   Test.Sync(
                "rightAtEndOfFormulaStops",
                fun () -> Assert.Equal(ValueNone, (MICurs.AtEnd(MA.String "ab")).Right)
            )
            Test.Sync(
                "leftAtStartOfFormulaStops",
                fun () -> Assert.Equal(ValueNone, (MICurs.AtStart(MA.String "ab")).Left)
            )
            Test.Sync(
                "rightStepsOverACharacter",
                fun () ->
                    let start = MICurs.AtStart(MA.String "ab")
                    let expected = MICurs.Row(arr [ MA.Char 'a' ], MICurs.CursorOrEmpty, arr [ MA.Char 'b' ])
                    Assert.Equal(expected, moved(Direction.Right, start))
            )
            Test.Sync(
                "rightEntersFractionNumerator",
                fun () ->
                    Assert.Equal(
                        MICurs.FracNum(MICurs.AtStart n, d),
                        moved(Direction.Right, MICurs.AtStart frac))
            )
            Test.Sync(
                "rightFromNumeratorEndEntersDenominator",
                fun () ->
                    Assert.Equal(
                        MICurs.FracDen(n, MICurs.AtStart d),
                        moved(Direction.Right, MICurs.FracNum(MICurs.AtEnd n, d)))
            )
            Test.Sync(
                "rightFromDenominatorEndExitsFraction",
                fun () ->
                    Assert.Equal(
                        MICurs.AtEnd frac,
                        moved(Direction.Right, MICurs.FracDen(n, MICurs.AtEnd d)))
            )
            Test.Sync(
                "leftEntersFractionDenominatorFromTheRight",
                fun () ->
                    Assert.Equal(
                        MICurs.FracDen(n, MICurs.AtEnd d),
                        moved(Direction.Left, MICurs.AtEnd frac))
            )
            Test.Sync(
                "upFromDenominatorGoesToNumerator",
                fun () ->
                    Assert.Equal(
                        MICurs.FracNum(MICurs.AtStart n, d),
                        moved(Direction.Up, MICurs.FracDen(n, MICurs.AtStart d)))
            )
            Test.Sync(
                "downFromNumeratorGoesToDenominator",
                fun () ->
                    Assert.Equal(
                        MICurs.FracDen(n, MICurs.AtStart d),
                        moved(Direction.Down, MICurs.FracNum(MICurs.AtStart n, d)))
            )
            Test.Sync(
                "upFromBaseGoesToSuperscript",
                fun () ->
                    let e = MA.Char 'e'
                    let two = MA.Char '2'
                    Assert.Equal(
                        MICurs.ScriptSuper(e, MICurs.AtStart two, ValueNone),
                        moved(Direction.Up, MICurs.ScriptMainSuper(MICurs.AtEnd e, two, ValueNone)))
            )
            Test.Sync(
                "upAndDownStopWhenThereIsNothingStacked",
                fun () ->
                    Assert.Equal(ValueNone, (typed "ab").Up)
                    Assert.Equal(ValueNone, (typed "ab").Down)
            )
            Test.CasesSync(
                "navigationPreservesTheFormula",
                sampleFormulas |> List.map (fun ma -> ma.ToString(), ma),
                fun ma ->
                    for position in walkRight ma do
                        Assert.Equal(ma, position.ToMA, "walking right changed the formula")
                    for position in walkLeft ma do
                        Assert.Equal(ma, position.ToMA, "walking left changed the formula")
            )
            Test.CasesSync(
                "walkingRightThenLeftVisitsTheSamePositions",
                sampleFormulas |> List.map (fun ma -> ma.ToString(), ma),
                fun ma -> Assert.Equal(walkRight ma, walkLeft ma |> List.rev)
            )
            Test.CasesSync(
                "everyRightStepIsUndoneByALeftStep",
                sampleFormulas |> List.map (fun ma -> ma.ToString(), ma),
                fun ma ->
                    let positions = walkRight ma
                    for previous, next in List.pairwise positions do
                        Assert.Equal(ValueSome previous, next.Left, $"left from {next}")
            )
        ]
    )

let tests = TestFolder("ModeMath", [ editing; navigation ])
