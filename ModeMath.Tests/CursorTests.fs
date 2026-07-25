module ModeMath.Tests.CursorTests

open System.Collections.Immutable
open Xunit
open ModeMath

let private typed (s: string) =
    s |> Seq.fold (fun (m: MICurs) c -> m.AddAlphanumeric c) MICurs.CursorOrEmpty

let private backspace (m: MICurs) =
    match m.BackSpace with
    | Choice1Of2 m -> m
    | Choice2Of2 ma -> failwithf "cursor fell off the left, leaving %O" ma

let private arr (xs: MA list) = xs.ToImmutableArray()

[<Fact>]
let ``typing produces a row of characters`` () =
    Assert.Equal(MA.String "xyz", (typed "xyz").ToMA)

[<Fact>]
let ``backspace removes the last character`` () =
    Assert.Equal<MICurs>(typed "xy", backspace (typed "xyz"))

[<Fact>]
let ``backspace of a single character empties the formula`` () =
    Assert.Equal<MICurs>(MICurs.CursorOrEmpty, backspace (typed "x"))

[<Fact>]
let ``backspace on an empty formula reports the cursor fell off the left`` () =
    match MICurs.CursorOrEmpty.BackSpace with
    | Choice2Of2 ma -> Assert.Equal(MA.Empty, ma)
    | Choice1Of2 m -> failwithf "expected Choice2, got %O" m

[<Fact>]
let ``typing a character then deleting it is the identity`` () =
    for s in [ "x"; "xy"; "q1" ] do
        Assert.Equal<MICurs>(typed s, backspace ((typed s).AddAlphanumeric 'w'))

[<Fact>]
let ``a trailing function name becomes a function`` () =
    Assert.Equal(MA.Function MathFunction.Sin, (typed "sin").ToMA)

[<Fact>]
let ``the longest function name wins`` () =
    Assert.Equal(MA.Function MathFunction.Asin, (typed "asin").ToMA)

[<Fact>]
let ``a function name is only recognised at the end of a letter run`` () =
    Assert.Equal(MA.Row2(MA.Char '2', MA.Function MathFunction.Cos), (typed "2cos").ToMA)

[<Fact>]
let ``MakeRow collapses an empty split`` () =
    Assert.Equal<MICurs>(
        MICurs.CursorOrEmpty,
        MICurs.MakeRow(ImmutableArray.Empty, MICurs.CursorOrEmpty, ImmutableArray.Empty))

[<Fact>]
let ``MakeRow merges a nested row so a row cursor has one representation`` () =
    let inner = MICurs.Row(arr [ MA.Char 'b' ], MICurs.CursorOrEmpty, arr [ MA.Char 'c' ])
    let merged = MICurs.MakeRow(arr [ MA.Char 'a' ], inner, arr [ MA.Char 'd' ])
    let expected =
        MICurs.Row(
            arr [ MA.Char 'a'; MA.Char 'b' ],
            MICurs.CursorOrEmpty,
            arr [ MA.Char 'c'; MA.Char 'd' ])
    Assert.Equal<MICurs>(expected, merged)

[<Fact>]
let ``AtEnd AtStart and Between preserve the formula`` () =
    let formulas =
        [ MA.Empty
          MA.Char 'a'
          MA.String "abc"
          MA.Frac(MA.Char 'n', MA.Char 'd')
          MA.Sqrt(MA.String "xy")
          MA.ScriptSuper(MA.Char 'e', MA.Char '2', ValueNone) ]
    for ma in formulas do
        Assert.Equal(ma, (MICurs.AtEnd ma).ToMA)
        Assert.Equal(ma, (MICurs.AtStart ma).ToMA)
    Assert.Equal(
        MA.Row2(MA.Char 'a', MA.Char 'b'),
        (MICurs.Between(MA.Char 'a', MA.Char 'b')).ToMA)

[<Fact>]
let ``backspace at the start of a denominator dissolves the fraction`` () =
    let cursored = MICurs.FracDen(MA.Char 'n', MICurs.AtStart(MA.Char 'd'))
    Assert.Equal(MA.Frac(MA.Char 'n', MA.Char 'd'), cursored.ToMA)
    let after = backspace cursored
    Assert.Equal<MICurs>(MICurs.Between(MA.Char 'n', MA.Char 'd'), after)
    Assert.Equal(MA.Row2(MA.Char 'n', MA.Char 'd'), after.ToMA)

[<Fact>]
let ``backspace inside a square root deletes within it`` () =
    let cursored = MICurs.Sqrt(MICurs.AtEnd(MA.String "xy"))
    Assert.Equal(MA.Sqrt(MA.String "xy"), cursored.ToMA)
    Assert.Equal(MA.Sqrt(MA.Char 'x'), (backspace cursored).ToMA)

[<Fact>]
let ``AddMICurs substitutes at the cursor and leaves the cursor inside`` () =
    let result = (typed "x").AddMICurs(MICurs.Sqrt MICurs.CursorOrEmpty)
    Assert.Equal(MA.Row2(MA.Char 'x', MA.Sqrt MA.Empty), result.ToMA)
    Assert.Equal(MA.Row2(MA.Char 'x', MA.Sqrt(MA.Char 'y')), (result.AddAlphanumeric 'y').ToMA)
