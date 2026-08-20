namespace ModeMath

open System
open System.Collections.Immutable
open FSUtils

module internal FunctionNames =
    let table =
        [|
            "sin", MathFunction.Sin
            "cos", MathFunction.Cos
            "tan", MathFunction.Tan
            "asin", MathFunction.Asin
            "acos", MathFunction.Acos
            "atan", MathFunction.Atan
            "sinh", MathFunction.Sinh
            "cosh", MathFunction.Cosh
            "tanh", MathFunction.Tanh
            "log", MathFunction.Log
            "ln", MathFunction.Ln
            "sec", MathFunction.Sec
            "csc", MathFunction.Csc
            "cot", MathFunction.Cot
            "erf", MathFunction.Erf
            "min", MathFunction.Min
            "max", MathFunction.Max
        |]
        |> Array.sortByDescending (fun (name, _) -> name.Length)

/// Math formula Input with a cursor
[<RequireQualifiedAccess>]
type MICurs =
    /// A cursor (e.g. in a row), or empty position (e.g. empty superscript).
    | CursorOrEmpty
    | Row of before: ImmutableArray<MA> * MICurs * after: ImmutableArray<MA>
    /// In the main part of a scripts containing a superscript
    | ScriptMainSuper of main: MICurs * super: MA * sub: MA voption
    /// In the main part of a scripts containing a subscript only
    | ScriptMainSub of main: MICurs * sub: MA
    | ScriptSuper of main: MA * super: MICurs * sub: MA voption
    | ScriptSub of main: MA * super: MA voption * sub: MICurs
    | FracNum of n: MICurs * d: MA
    | FracDen of n: MA * d: MICurs
    | Bracketed of Bracket * MICurs * completion: BracketCompletion
    | RootNDegree of n: MICurs * x: MA
    | RootNMain of n: MA * x: MICurs
    | Sqrt of x: MICurs

    /// Builds a Row in canonical form, collapsing an empty split and merging a nested Row.
    static member MakeRow(before: ImmutableArray<MA>, inner: MICurs, after: ImmutableArray<MA>) =
        match inner with
        | Row(b, i, a) -> MICurs.MakeRow(before.AddRange b, i, a.AddRange after)
        | _ when before.IsEmpty && after.IsEmpty -> inner
        | _ -> Row(before, inner, after)

    /// Cursor at the right-hand end of an MA.
    static member AtEnd(ma: MA) = MICurs.MakeRow(ma.Elements, CursorOrEmpty, ImmutableArray.Empty)

    /// Cursor at the left-hand end of an MA.
    static member AtStart(ma: MA) = MICurs.MakeRow(ImmutableArray.Empty, CursorOrEmpty, ma.Elements)

    /// Cursor between two MAs.
    static member Between(before: MA, after: MA) =
        MICurs.MakeRow(before.Elements, CursorOrEmpty, after.Elements)

    /// The formula with the cursor removed.
    member t.ToMA: MA =
        match t with
        | CursorOrEmpty -> MA.Empty
        | Row(before, inner, after) ->
            let b = ImmutableArray.CreateBuilder<MA>()
            b.AddRange before
            let innerMa = inner.ToMA
            if not innerMa.IsEmpty then b.AddRange innerMa.Elements
            b.AddRange after
            b.ToImmutable() |> MA.OfElements
        | ScriptMainSuper(main, super, sub) -> MA.ScriptSuper(main.ToMA, super, sub)
        | ScriptMainSub(main, sub) -> MA.ScriptSub(main.ToMA, sub)
        | ScriptSuper(main, super, sub) -> MA.ScriptSuper(main, super.ToMA, sub)
        | ScriptSub(main, super, sub) ->
            match super with
            | ValueSome super -> MA.ScriptSuper(main, super, ValueSome sub.ToMA)
            | ValueNone -> MA.ScriptSub(main, sub.ToMA)
        | FracNum(n, d) -> MA.Frac(n.ToMA, d)
        | FracDen(n, d) -> MA.Frac(n, d.ToMA)
        | Bracketed(b, inner, bc) -> MA.Bracketed(b, inner.ToMA, bc)
        | RootNDegree(n, x) -> MA.RootN(n.ToMA, x)
        | RootNMain(n, x) -> MA.RootN(n, x.ToMA)
        | Sqrt x -> MA.Sqrt x.ToMA

    /// Gives the MICurs resulting from pressing backspace from the right of an MA. ValueNone if the original MA is Empty
    static member private BackspaceFromRight(ma: MA): MICurs voption =
        match ma with
        | MA.Row l ->
            if l.IsEmpty then ValueNone
            else
                let last = l.[l.Length - 1]
                let prev = l.RemoveAt(l.Length - 1)
                match MICurs.BackspaceFromRight last with
                | ValueSome last -> MICurs.MakeRow(prev, last, ImmutableArray.Empty) |> ValueSome
                | ValueNone -> MICurs.BackspaceFromRight(MA.Row prev)
        | MA.Bracketed(b, inner, bc) ->
            match bc with
            | BracketCompletion.Left | BracketCompletion.Completed ->
                Bracketed(b, MICurs.AtEnd inner, BracketCompletion.Left) |> ValueSome
            | BracketCompletion.Right -> MICurs.AtEnd inner |> ValueSome
        | MA.Char _ | MA.BoldVar _ | MA.Cdot | MA.UprightD | MA.Function _ | MA.Operator _
        | MA.ScriptSuper _ | MA.ScriptSub _ | MA.Frac _ | MA.RootN _ | MA.Sqrt _ | MA.BigOp _ ->
            ValueSome CursorOrEmpty

    /// Returns Choice2 of MA if the cursor is on the left; otherwise returns Choice1 of the altered MICurs
    member t.BackSpace: Choice<MICurs, MA> =
        match t with
        | CursorOrEmpty -> Choice2Of2 MA.Empty
        | Row(before, inner, after) ->
            match inner.BackSpace with
            | Choice1Of2 inner -> MICurs.MakeRow(before, inner, after) |> Choice1Of2
            | Choice2Of2 innerMa ->
                let rest = innerMa.Elements.AddRange after
                match MICurs.BackspaceFromRight(MA.Row before) with
                | ValueSome before -> MICurs.MakeRow(ImmutableArray.Empty, before, rest) |> Choice1Of2
                | ValueNone -> MA.OfElements rest |> Choice2Of2
        | ScriptMainSuper(main, super, sub) ->
            match main.BackSpace with
            | Choice1Of2 main -> ScriptMainSuper(main, super, sub) |> Choice1Of2
            | Choice2Of2 main -> MA.ScriptSuper(main, super, sub) |> Choice2Of2
        | ScriptMainSub(main, sub) ->
            match main.BackSpace with
            | Choice1Of2 main -> ScriptMainSub(main, sub) |> Choice1Of2
            | Choice2Of2 main -> MA.ScriptSub(main, sub) |> Choice2Of2
        | ScriptSuper(main, super, sub) ->
            match super.BackSpace with
            | Choice1Of2 super -> ScriptSuper(main, super, sub) |> Choice1Of2
            | Choice2Of2 super ->
                let baseAtom =
                    match sub with
                    | ValueSome sub -> MA.ScriptSub(main, sub)
                    | ValueNone -> main
                MICurs.Between(baseAtom, super) |> Choice1Of2
        | ScriptSub(main, super, sub) ->
            match sub.BackSpace with
            | Choice1Of2 sub -> ScriptSub(main, super, sub) |> Choice1Of2
            | Choice2Of2 sub ->
                let baseAtom =
                    match super with
                    | ValueSome super -> MA.ScriptSuper(main, super, ValueNone)
                    | ValueNone -> main
                MICurs.Between(baseAtom, sub) |> Choice1Of2
        | FracNum(n, d) ->
            match n.BackSpace with
            | Choice1Of2 n -> FracNum(n, d) |> Choice1Of2
            | Choice2Of2 n -> MICurs.AtStart(MA.Row2(n, d)) |> Choice1Of2
        | FracDen(n, d) ->
            match d.BackSpace with
            | Choice1Of2 d -> FracDen(n, d) |> Choice1Of2
            | Choice2Of2 d -> MICurs.Between(n, d) |> Choice1Of2
        | Bracketed(b, inner, completion) ->
            match inner.BackSpace with
            | Choice1Of2 inner -> Bracketed(b, inner, completion) |> Choice1Of2
            | Choice2Of2 inner -> MICurs.AtStart inner |> Choice1Of2
        | RootNDegree(n, x) ->
            match n.BackSpace with
            | Choice1Of2 n -> RootNDegree(n, x) |> Choice1Of2
            | Choice2Of2 n -> MICurs.AtStart(MA.Row2(n, x)) |> Choice1Of2
        | RootNMain(n, x) ->
            match x.BackSpace with
            | Choice1Of2 x -> RootNMain(n, x) |> Choice1Of2
            | Choice2Of2 x -> MICurs.Between(n, x) |> Choice1Of2
        | Sqrt x ->
            match x.BackSpace with
            | Choice1Of2 x -> Sqrt x |> Choice1Of2
            | Choice2Of2 x -> MICurs.AtStart x |> Choice1Of2

    /// Gives the MICurs resulting from pressing delete from the left of an MA. ValueNone if the original MA is Empty
    static member private DeleteFromLeft(ma: MA): MICurs voption =
        match ma with
        | MA.Row l ->
            if l.IsEmpty then ValueNone
            else
                let first = l.[0]
                let rest = l.RemoveAt 0
                match MICurs.DeleteFromLeft first with
                | ValueSome first -> MICurs.MakeRow(ImmutableArray.Empty, first, rest) |> ValueSome
                | ValueNone -> MICurs.DeleteFromLeft(MA.Row rest)
        | MA.Bracketed(b, inner, bc) ->
            match bc with
            | BracketCompletion.Right | BracketCompletion.Completed ->
                Bracketed(b, MICurs.AtStart inner, BracketCompletion.Right) |> ValueSome
            | BracketCompletion.Left -> MICurs.AtStart inner |> ValueSome
        | MA.Char _ | MA.BoldVar _ | MA.Cdot | MA.UprightD | MA.Function _ | MA.Operator _
        | MA.ScriptSuper _ | MA.ScriptSub _ | MA.Frac _ | MA.RootN _ | MA.Sqrt _ | MA.BigOp _ ->
            ValueSome CursorOrEmpty

    /// Returns Choice2 of MA if the cursor is on the right; otherwise returns Choice1 of the altered MICurs
    member t.Delete: Choice<MICurs, MA> =
        match t with
        | CursorOrEmpty -> Choice2Of2 MA.Empty
        | Row(before, inner, after) ->
            match inner.Delete with
            | Choice1Of2 inner -> MICurs.MakeRow(before, inner, after) |> Choice1Of2
            | Choice2Of2 innerMa ->
                let rest = before.AddRange innerMa.Elements
                match MICurs.DeleteFromLeft(MA.Row after) with
                | ValueSome after -> MICurs.MakeRow(rest, after, ImmutableArray.Empty) |> Choice1Of2
                | ValueNone -> MA.OfElements rest |> Choice2Of2
        | ScriptMainSuper(main, super, sub) ->
            match main.Delete with
            | Choice1Of2 main -> ScriptMainSuper(main, super, sub) |> Choice1Of2
            | Choice2Of2 main ->
                let baseAtom =
                    match sub with
                    | ValueSome sub -> MA.ScriptSub(main, sub)
                    | ValueNone -> main
                MICurs.Between(baseAtom, super) |> Choice1Of2
        | ScriptMainSub(main, sub) ->
            match main.Delete with
            | Choice1Of2 main -> ScriptMainSub(main, sub) |> Choice1Of2
            | Choice2Of2 main -> MICurs.Between(main, sub) |> Choice1Of2
        | ScriptSuper(main, super, sub) ->
            match super.Delete with
            | Choice1Of2 super -> ScriptSuper(main, super, sub) |> Choice1Of2
            | Choice2Of2 super -> MA.ScriptSuper(main, super, sub) |> Choice2Of2
        | ScriptSub(main, super, sub) ->
            match sub.Delete with
            | Choice1Of2 sub -> ScriptSub(main, super, sub) |> Choice1Of2
            | Choice2Of2 sub ->
                match super with
                | ValueSome super -> MA.ScriptSuper(main, super, ValueSome sub) |> Choice2Of2
                | ValueNone -> MA.ScriptSub(main, sub) |> Choice2Of2
        | FracNum(n, d) ->
            match n.Delete with
            | Choice1Of2 n -> FracNum(n, d) |> Choice1Of2
            | Choice2Of2 n -> MICurs.Between(n, d) |> Choice1Of2
        | FracDen(n, d) ->
            match d.Delete with
            | Choice1Of2 d -> FracDen(n, d) |> Choice1Of2
            | Choice2Of2 d -> MICurs.AtEnd(MA.Row2(n, d)) |> Choice1Of2
        | Bracketed(b, inner, completion) ->
            match inner.Delete with
            | Choice1Of2 inner -> Bracketed(b, inner, completion) |> Choice1Of2
            | Choice2Of2 inner -> MICurs.AtEnd inner |> Choice1Of2
        | RootNDegree(n, x) ->
            match n.Delete with
            | Choice1Of2 n -> RootNDegree(n, x) |> Choice1Of2
            | Choice2Of2 n -> MICurs.Between(n, x) |> Choice1Of2
        | RootNMain(n, x) ->
            match x.Delete with
            | Choice1Of2 x -> RootNMain(n, x) |> Choice1Of2
            | Choice2Of2 x -> MICurs.AtEnd(MA.Row2(n, x)) |> Choice1Of2
        | Sqrt x ->
            match x.Delete with
            | Choice1Of2 x -> Sqrt x |> Choice1Of2
            | Choice2Of2 x -> MICurs.AtEnd x |> Choice1Of2

    /// Cursor at the start of an MA's first editable part. ValueNone if it has none.
    static member private EnterFromLeft(ma: MA): MICurs voption =
        match ma with
        | MA.Row _ | MA.Char _ | MA.BoldVar _ | MA.Cdot | MA.UprightD | MA.Function _ | MA.Operator _
        | MA.BigOp _ ->
            ValueNone
        | MA.ScriptSuper(main, super, sub) -> ScriptMainSuper(MICurs.AtStart main, super, sub) |> ValueSome
        | MA.ScriptSub(main, sub) -> ScriptMainSub(MICurs.AtStart main, sub) |> ValueSome
        | MA.Frac(n, d) -> FracNum(MICurs.AtStart n, d) |> ValueSome
        | MA.Bracketed(b, inner, bc) -> Bracketed(b, MICurs.AtStart inner, bc) |> ValueSome
        | MA.RootN(n, x) -> RootNDegree(MICurs.AtStart n, x) |> ValueSome
        | MA.Sqrt x -> Sqrt(MICurs.AtStart x) |> ValueSome

    /// Cursor at the end of an MA's last editable part. ValueNone if it has none.
    static member private EnterFromRight(ma: MA): MICurs voption =
        match ma with
        | MA.Row _ | MA.Char _ | MA.BoldVar _ | MA.Cdot | MA.UprightD | MA.Function _ | MA.Operator _
        | MA.BigOp _ ->
            ValueNone
        | MA.ScriptSuper(main, super, sub) ->
            match sub with
            | ValueSome sub -> ScriptSub(main, ValueSome super, MICurs.AtEnd sub) |> ValueSome
            | ValueNone -> ScriptSuper(main, MICurs.AtEnd super, ValueNone) |> ValueSome
        | MA.ScriptSub(main, sub) -> ScriptSub(main, ValueNone, MICurs.AtEnd sub) |> ValueSome
        | MA.Frac(n, d) -> FracDen(n, MICurs.AtEnd d) |> ValueSome
        | MA.Bracketed(b, inner, bc) -> Bracketed(b, MICurs.AtEnd inner, bc) |> ValueSome
        | MA.RootN(n, x) -> RootNMain(n, MICurs.AtEnd x) |> ValueSome
        | MA.Sqrt x -> Sqrt(MICurs.AtEnd x) |> ValueSome

    /// Moves the cursor one place right without leaving this MICurs. ValueNone if it is already at the right-hand end.
    member private t.MoveRightWithin: MICurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            match inner.MoveRightWithin with
            | ValueSome inner -> MICurs.MakeRow(before, inner, after) |> ValueSome
            | ValueNone ->
                match inner with
                | CursorOrEmpty ->
                    if after.IsEmpty then ValueNone
                    else
                        let next = after.[0]
                        let rest = after.RemoveAt 0
                        match MICurs.EnterFromLeft next with
                        | ValueSome entered -> MICurs.MakeRow(before, entered, rest) |> ValueSome
                        | ValueNone -> MICurs.MakeRow(before.Add next, CursorOrEmpty, rest) |> ValueSome
                | _ -> MICurs.MakeRow(before.AddRange inner.ToMA.Elements, CursorOrEmpty, after) |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            match main.MoveRightWithin with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptSuper(main.ToMA, MICurs.AtStart super, sub) |> ValueSome
        | ScriptMainSub(main, sub) ->
            match main.MoveRightWithin with
            | ValueSome main -> ScriptMainSub(main, sub) |> ValueSome
            | ValueNone -> ScriptSub(main.ToMA, ValueNone, MICurs.AtStart sub) |> ValueSome
        | ScriptSuper(main, super, sub) ->
            match super.MoveRightWithin with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone ->
                sub |> ValueOption.map (fun sub -> ScriptSub(main, ValueSome super.ToMA, MICurs.AtStart sub))
        | ScriptSub(main, super, sub) ->
            sub.MoveRightWithin |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) ->
            match n.MoveRightWithin with
            | ValueSome n -> FracNum(n, d) |> ValueSome
            | ValueNone -> FracDen(n.ToMA, MICurs.AtStart d) |> ValueSome
        | FracDen(n, d) -> d.MoveRightWithin |> ValueOption.map (fun d -> FracDen(n, d))
        | Bracketed(b, inner, bc) ->
            inner.MoveRightWithin |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) ->
            match n.MoveRightWithin with
            | ValueSome n -> RootNDegree(n, x) |> ValueSome
            | ValueNone -> RootNMain(n.ToMA, MICurs.AtStart x) |> ValueSome
        | RootNMain(n, x) -> x.MoveRightWithin |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> x.MoveRightWithin |> ValueOption.map (fun x -> Sqrt x)

    /// Moves the cursor one place left without leaving this MICurs. ValueNone if it is already at the left-hand end.
    member private t.MoveLeftWithin: MICurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            match inner.MoveLeftWithin with
            | ValueSome inner -> MICurs.MakeRow(before, inner, after) |> ValueSome
            | ValueNone ->
                match inner with
                | CursorOrEmpty ->
                    if before.IsEmpty then ValueNone
                    else
                        let previous = before.[before.Length - 1]
                        let rest = before.RemoveAt(before.Length - 1)
                        match MICurs.EnterFromRight previous with
                        | ValueSome entered -> MICurs.MakeRow(rest, entered, after) |> ValueSome
                        | ValueNone -> MICurs.MakeRow(rest, CursorOrEmpty, after.Insert(0, previous)) |> ValueSome
                | _ -> MICurs.MakeRow(before, CursorOrEmpty, inner.ToMA.Elements.AddRange after) |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            main.MoveLeftWithin |> ValueOption.map (fun main -> ScriptMainSuper(main, super, sub))
        | ScriptMainSub(main, sub) ->
            main.MoveLeftWithin |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            match super.MoveLeftWithin with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptMainSuper(MICurs.AtEnd main, super.ToMA, sub) |> ValueSome
        | ScriptSub(main, super, sub) ->
            match sub.MoveLeftWithin with
            | ValueSome sub -> ScriptSub(main, super, sub) |> ValueSome
            | ValueNone ->
                match super with
                | ValueSome super -> ScriptSuper(main, MICurs.AtEnd super, ValueSome sub.ToMA) |> ValueSome
                | ValueNone -> ScriptMainSub(MICurs.AtEnd main, sub.ToMA) |> ValueSome
        | FracNum(n, d) -> n.MoveLeftWithin |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) ->
            match d.MoveLeftWithin with
            | ValueSome d -> FracDen(n, d) |> ValueSome
            | ValueNone -> FracNum(MICurs.AtEnd n, d.ToMA) |> ValueSome
        | Bracketed(b, inner, bc) ->
            inner.MoveLeftWithin |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) -> n.MoveLeftWithin |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) ->
            match x.MoveLeftWithin with
            | ValueSome x -> RootNMain(n, x) |> ValueSome
            | ValueNone -> RootNDegree(MICurs.AtEnd n, x.ToMA) |> ValueSome
        | Sqrt x -> x.MoveLeftWithin |> ValueOption.map (fun x -> Sqrt x)

    /// Moves the cursor one place right, leaving the outermost atom if needed. ValueNone at the end of the formula.
    member t.Right: MICurs voption =
        match t.MoveRightWithin with
        | ValueSome moved -> ValueSome moved
        | ValueNone ->
            match t with
            | CursorOrEmpty -> ValueNone
            | Row(_, CursorOrEmpty, after) when after.IsEmpty -> ValueNone
            | _ -> MICurs.AtEnd t.ToMA |> ValueSome

    /// Moves the cursor one place left, leaving the outermost atom if needed. ValueNone at the start of the formula.
    member t.Left: MICurs voption =
        match t.MoveLeftWithin with
        | ValueSome moved -> ValueSome moved
        | ValueNone ->
            match t with
            | CursorOrEmpty -> ValueNone
            | Row(before, CursorOrEmpty, _) when before.IsEmpty -> ValueNone
            | _ -> MICurs.AtStart t.ToMA |> ValueSome

    /// Moves the cursor to the start of the block above, e.g. the numerator from the denominator. ValueNone if there is none.
    member t.Up: MICurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            inner.Up |> ValueOption.map (fun inner -> MICurs.MakeRow(before, inner, after))
        | ScriptMainSuper(main, super, sub) ->
            match main.Up with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptSuper(main.ToMA, MICurs.AtStart super, sub) |> ValueSome
        | ScriptMainSub(main, sub) -> main.Up |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            super.Up |> ValueOption.map (fun super -> ScriptSuper(main, super, sub))
        | ScriptSub(main, super, sub) ->
            match sub.Up with
            | ValueSome sub -> ScriptSub(main, super, sub) |> ValueSome
            | ValueNone ->
                match super with
                | ValueSome super -> ScriptMainSuper(MICurs.AtStart main, super, ValueSome sub.ToMA) |> ValueSome
                | ValueNone -> ScriptMainSub(MICurs.AtStart main, sub.ToMA) |> ValueSome
        | FracNum(n, d) -> n.Up |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) ->
            match d.Up with
            | ValueSome d -> FracDen(n, d) |> ValueSome
            | ValueNone -> FracNum(MICurs.AtStart n, d.ToMA) |> ValueSome
        | Bracketed(b, inner, bc) -> inner.Up |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) -> n.Up |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) ->
            match x.Up with
            | ValueSome x -> RootNMain(n, x) |> ValueSome
            | ValueNone -> RootNDegree(MICurs.AtStart n, x.ToMA) |> ValueSome
        | Sqrt x -> x.Up |> ValueOption.map (fun x -> Sqrt x)

    /// Moves the cursor to the start of the block below, e.g. the denominator from the numerator. ValueNone if there is none.
    member t.Down: MICurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            inner.Down |> ValueOption.map (fun inner -> MICurs.MakeRow(before, inner, after))
        | ScriptMainSuper(main, super, sub) ->
            match main.Down with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone ->
                sub |> ValueOption.map (fun sub -> ScriptSub(main.ToMA, ValueSome super, MICurs.AtStart sub))
        | ScriptMainSub(main, sub) ->
            match main.Down with
            | ValueSome main -> ScriptMainSub(main, sub) |> ValueSome
            | ValueNone -> ScriptSub(main.ToMA, ValueNone, MICurs.AtStart sub) |> ValueSome
        | ScriptSuper(main, super, sub) ->
            match super.Down with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptMainSuper(MICurs.AtStart main, super.ToMA, sub) |> ValueSome
        | ScriptSub(main, super, sub) ->
            sub.Down |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) ->
            match n.Down with
            | ValueSome n -> FracNum(n, d) |> ValueSome
            | ValueNone -> FracDen(n.ToMA, MICurs.AtStart d) |> ValueSome
        | FracDen(n, d) -> d.Down |> ValueOption.map (fun d -> FracDen(n, d))
        | Bracketed(b, inner, bc) -> inner.Down |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) ->
            match n.Down with
            | ValueSome n -> RootNDegree(n, x) |> ValueSome
            | ValueNone -> RootNMain(n.ToMA, MICurs.AtStart x) |> ValueSome
        | RootNMain(n, x) -> x.Down |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> x.Down |> ValueOption.map (fun x -> Sqrt x)

    member t.Move(direction: Direction): MICurs voption =
        match direction with
        | Direction.Left -> t.Left
        | Direction.Right -> t.Right
        | Direction.Up -> t.Up
        | Direction.Down -> t.Down
        | _ -> ValueNone

    /// Adds an MICurs naively
    member t.AddMICurs(addition: MICurs): MICurs =
        match t with
        | CursorOrEmpty -> addition
        | Row(before, inner, after) -> MICurs.MakeRow(before, inner.AddMICurs addition, after)
        | ScriptMainSuper(main, super, sub) -> ScriptMainSuper(main.AddMICurs addition, super, sub)
        | ScriptMainSub(main, sub) -> ScriptMainSub(main.AddMICurs addition, sub)
        | ScriptSuper(main, super, sub) -> ScriptSuper(main, super.AddMICurs addition, sub)
        | ScriptSub(main, super, sub) -> ScriptSub(main, super, sub.AddMICurs addition)
        | FracNum(n, d) -> FracNum(n.AddMICurs addition, d)
        | FracDen(n, d) -> FracDen(n, d.AddMICurs addition)
        | Bracketed(b, inner, bc) -> Bracketed(b, inner.AddMICurs addition, bc)
        | RootNDegree(n, x) -> RootNDegree(n.AddMICurs addition, x)
        | RootNMain(n, x) -> RootNMain(n, x.AddMICurs addition)
        | Sqrt x -> Sqrt(x.AddMICurs addition)

    /// Replaces a trailing run of letters spelling a function name with that function.
    static member private RecogniseFunction(elements: ImmutableArray<MA>) =
        let isLetter(i: int) =
            match elements.[i] with
            | MA.Char c -> Char.IsLetter c
            | _ -> false
        let mutable start = elements.Length
        while start > 0 && isLetter (start - 1) do
            start <- start - 1
        let letters =
            String(Array.init (elements.Length - start) (fun i ->
                match elements.[start + i] with
                | MA.Char c -> c
                | _ -> ' '))
        let matched =
            FunctionNames.table
            |> Array.tryFind (fun (name, _) -> letters.EndsWith(name, StringComparison.Ordinal))
        match matched with
        | Some(name, fn) -> (elements |> ImmArray.truncate (elements.Length - name.Length)).Add(MA.Function fn)
        | None -> elements

    /// Adds a character at the cursor, replacing a completed function name with that function.
    member t.AddAlphanumeric(c: char): MICurs =
        match t with
        | CursorOrEmpty -> MICurs.AtEnd(MA.Char c)
        | Row(before, CursorOrEmpty, after) ->
            MICurs.MakeRow(MICurs.RecogniseFunction(before.Add(MA.Char c)), CursorOrEmpty, after)
        | Row(before, inner, after) -> MICurs.MakeRow(before, inner.AddAlphanumeric c, after)
        | ScriptMainSuper(main, super, sub) -> ScriptMainSuper(main.AddAlphanumeric c, super, sub)
        | ScriptMainSub(main, sub) -> ScriptMainSub(main.AddAlphanumeric c, sub)
        | ScriptSuper(main, super, sub) -> ScriptSuper(main, super.AddAlphanumeric c, sub)
        | ScriptSub(main, super, sub) -> ScriptSub(main, super, sub.AddAlphanumeric c)
        | FracNum(n, d) -> FracNum(n.AddAlphanumeric c, d)
        | FracDen(n, d) -> FracDen(n, d.AddAlphanumeric c)
        | Bracketed(b, inner, bc) -> Bracketed(b, inner.AddAlphanumeric c, bc)
        | RootNDegree(n, x) -> RootNDegree(n.AddAlphanumeric c, x)
        | RootNMain(n, x) -> RootNMain(n, x.AddAlphanumeric c)
        | Sqrt x -> Sqrt(x.AddAlphanumeric c)

    override t.ToString() =
        let props(name: string, xs: obj seq) =
            name + "(" + (xs |> Seq.map string |> String.concat ", ") + ")"
        let row(l: ImmutableArray<MA>) = l |> Seq.map string |> String.concat " "
        match t with
        | CursorOrEmpty -> "|"
        | Row(before, inner, after) -> props("Row", [ box (row before); box inner; box (row after) ])
        | ScriptMainSuper(main, super, sub) -> props("ScriptMainSuper", [ box main; box super; box sub ])
        | ScriptMainSub(main, sub) -> props("ScriptMainSub", [ box main; box sub ])
        | ScriptSuper(main, super, sub) -> props("ScriptSuper", [ box main; box super; box sub ])
        | ScriptSub(main, super, sub) -> props("ScriptSub", [ box main; box super; box sub ])
        | FracNum(n, d) -> props("FracNum", [ box n; box d ])
        | FracDen(n, d) -> props("FracDen", [ box n; box d ])
        | Bracketed(b, inner, bc) -> props("Bracketed", [ box b; box inner; box bc ])
        | RootNDegree(n, x) -> props("RootNDegree", [ box n; box x ])
        | RootNMain(n, x) -> props("RootNMain", [ box n; box x ])
        | Sqrt x -> props("Sqrt", [ box x ])
