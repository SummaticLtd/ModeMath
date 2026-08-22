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
type internal MACurs =
    /// A cursor (e.g. in a row), or empty position (e.g. empty superscript).
    | CursorOrEmpty
    | Row of before: ImmutableArray<MA> * MACurs * after: ImmutableArray<MA>
    /// In the main part of a scripts containing a superscript
    | ScriptMainSuper of main: MACurs * super: MA * sub: MA voption
    /// In the main part of a scripts containing a subscript only
    | ScriptMainSub of main: MACurs * sub: MA
    | ScriptSuper of main: MA * super: MACurs * sub: MA voption
    | ScriptSub of main: MA * super: MA voption * sub: MACurs
    | FracNum of n: MACurs * d: MA
    | FracDen of n: MA * d: MACurs
    | Bracketed of Brackets * MACurs * completion: BracketCompletion
    | RootNDegree of n: MACurs * x: MA
    | RootNMain of n: MA * x: MACurs
    | Sqrt of x: MACurs

    /// Builds a Row in canonical form, collapsing an empty split and merging a nested Row.
    static member MakeRow(before: ImmutableArray<MA>, inner: MACurs, after: ImmutableArray<MA>) =
        match inner with
        | Row(b, i, a) -> MACurs.MakeRow(before.AddRange b, i, a.AddRange after)
        | _ when before.IsEmpty && after.IsEmpty -> inner
        | _ -> Row(before, inner, after)

    /// Cursor at the right-hand end of an MA.
    static member AtEnd(ma: MA) = MACurs.MakeRow(ma.Elements, CursorOrEmpty, ImmutableArray.Empty)

    /// Cursor at the left-hand end of an MA.
    static member AtStart(ma: MA) = MACurs.MakeRow(ImmutableArray.Empty, CursorOrEmpty, ma.Elements)

    /// Cursor between two MAs.
    static member Between(before: MA, after: MA) =
        MACurs.MakeRow(before.Elements, CursorOrEmpty, after.Elements)

    /// Every cursor position in a formula, from its left-hand end rightwards.
    static member Positions(ma: MA) =
        seq {
            let mutable current = ValueSome(MACurs.AtStart ma)
            while current.IsSome do
                yield current.Value
                current <- current.Value.Right
        }

    member t.Flatten: MACurs =
        let flat(ma: MA) = ma.Flatten
        match t with
        | CursorOrEmpty -> t
        | Row(before, inner, after) ->
            MACurs.MakeRow(MA.FlattenElements before, inner.Flatten, MA.FlattenElements after)
        | ScriptMainSuper(main, super, sub) ->
            ScriptMainSuper(main.Flatten, flat super, sub |> ValueOption.map flat)
        | ScriptMainSub(main, sub) -> ScriptMainSub(main.Flatten, flat sub)
        | ScriptSuper(main, super, sub) ->
            ScriptSuper(flat main, super.Flatten, sub |> ValueOption.map flat)
        | ScriptSub(main, super, sub) ->
            ScriptSub(flat main, super |> ValueOption.map flat, sub.Flatten)
        | FracNum(n, d) -> FracNum(n.Flatten, flat d)
        | FracDen(n, d) -> FracDen(flat n, d.Flatten)
        | Bracketed(b, inner, bc) -> Bracketed(b, inner.Flatten, bc)
        | RootNDegree(n, x) -> RootNDegree(n.Flatten, flat x)
        | RootNMain(n, x) -> RootNMain(flat n, x.Flatten)
        | Sqrt x -> Sqrt x.Flatten

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

    /// Gives the MACurs resulting from pressing backspace from the right of an MA. ValueNone if the original MA is Empty
    static member private BackspaceFromRight(ma: MA): MACurs voption =
        match ma with
        | MA.Row l ->
            if l.IsEmpty then ValueNone
            else
                let last = l.[l.Length - 1]
                let prev = l.RemoveAt(l.Length - 1)
                match MACurs.BackspaceFromRight last with
                | ValueSome last -> MACurs.MakeRow(prev, last, ImmutableArray.Empty) |> ValueSome
                | ValueNone -> MACurs.BackspaceFromRight(MA.Row prev)
        | MA.Bracketed(b, inner, bc) ->
            match bc with
            | BracketCompletion.Left | BracketCompletion.Completed ->
                Bracketed(b, MACurs.AtEnd inner, BracketCompletion.Left) |> ValueSome
            | BracketCompletion.Right -> MACurs.AtEnd inner |> ValueSome
        | MA.Char _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD | MA.Function _
        | MA.ScriptSuper _ | MA.ScriptSub _ | MA.Frac _ | MA.RootN _ | MA.Sqrt _
        | MA.BigOp _ | MA.Accented _ | MA.Spanned _ | MA.Overline _ | MA.Underline _ | MA.Stack _
        | MA.Table _ | MA.Text _ | MA.Space _ | MA.Coloured _ ->
            ValueSome CursorOrEmpty

    /// Returns Choice2 of MA if the cursor is on the left; otherwise returns Choice1 of the altered MACurs
    member t.BackSpace: Choice<MACurs, MA> =
        match t with
        | CursorOrEmpty -> Choice2Of2 MA.Empty
        | Row(before, inner, after) ->
            match inner.BackSpace with
            | Choice1Of2 inner -> MACurs.MakeRow(before, inner, after) |> Choice1Of2
            | Choice2Of2 innerMa ->
                let rest = innerMa.Elements.AddRange after
                match MACurs.BackspaceFromRight(MA.Row before) with
                | ValueSome before -> MACurs.MakeRow(ImmutableArray.Empty, before, rest) |> Choice1Of2
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
                MACurs.Between(baseAtom, super) |> Choice1Of2
        | ScriptSub(main, super, sub) ->
            match sub.BackSpace with
            | Choice1Of2 sub -> ScriptSub(main, super, sub) |> Choice1Of2
            | Choice2Of2 sub ->
                let baseAtom =
                    match super with
                    | ValueSome super -> MA.ScriptSuper(main, super, ValueNone)
                    | ValueNone -> main
                MACurs.Between(baseAtom, sub) |> Choice1Of2
        | FracNum(n, d) ->
            match n.BackSpace with
            | Choice1Of2 n -> FracNum(n, d) |> Choice1Of2
            | Choice2Of2 n -> MACurs.AtStart(MA.Row2(n, d)) |> Choice1Of2
        | FracDen(n, d) ->
            match d.BackSpace with
            | Choice1Of2 d -> FracDen(n, d) |> Choice1Of2
            | Choice2Of2 d -> MACurs.Between(n, d) |> Choice1Of2
        | Bracketed(b, inner, completion) ->
            match inner.BackSpace with
            | Choice1Of2 inner -> Bracketed(b, inner, completion) |> Choice1Of2
            | Choice2Of2 inner -> MACurs.AtStart inner |> Choice1Of2
        | RootNDegree(n, x) ->
            match n.BackSpace with
            | Choice1Of2 n -> RootNDegree(n, x) |> Choice1Of2
            | Choice2Of2 n -> MACurs.AtStart(MA.Row2(n, x)) |> Choice1Of2
        | RootNMain(n, x) ->
            match x.BackSpace with
            | Choice1Of2 x -> RootNMain(n, x) |> Choice1Of2
            | Choice2Of2 x -> MACurs.Between(n, x) |> Choice1Of2
        | Sqrt x ->
            match x.BackSpace with
            | Choice1Of2 x -> Sqrt x |> Choice1Of2
            | Choice2Of2 x -> MACurs.AtStart x |> Choice1Of2

    /// Gives the MACurs resulting from pressing delete from the left of an MA. ValueNone if the original MA is Empty
    static member private DeleteFromLeft(ma: MA): MACurs voption =
        match ma with
        | MA.Row l ->
            if l.IsEmpty then ValueNone
            else
                let first = l.[0]
                let rest = l.RemoveAt 0
                match MACurs.DeleteFromLeft first with
                | ValueSome first -> MACurs.MakeRow(ImmutableArray.Empty, first, rest) |> ValueSome
                | ValueNone -> MACurs.DeleteFromLeft(MA.Row rest)
        | MA.Bracketed(b, inner, bc) ->
            match bc with
            | BracketCompletion.Right | BracketCompletion.Completed ->
                Bracketed(b, MACurs.AtStart inner, BracketCompletion.Right) |> ValueSome
            | BracketCompletion.Left -> MACurs.AtStart inner |> ValueSome
        | MA.Char _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD | MA.Function _
        | MA.ScriptSuper _ | MA.ScriptSub _ | MA.Frac _ | MA.RootN _ | MA.Sqrt _
        | MA.BigOp _ | MA.Accented _ | MA.Spanned _ | MA.Overline _ | MA.Underline _ | MA.Stack _
        | MA.Table _ | MA.Text _ | MA.Space _ | MA.Coloured _ ->
            ValueSome CursorOrEmpty

    /// Returns Choice2 of MA if the cursor is on the right; otherwise returns Choice1 of the altered MACurs
    member t.Delete: Choice<MACurs, MA> =
        match t with
        | CursorOrEmpty -> Choice2Of2 MA.Empty
        | Row(before, inner, after) ->
            match inner.Delete with
            | Choice1Of2 inner -> MACurs.MakeRow(before, inner, after) |> Choice1Of2
            | Choice2Of2 innerMa ->
                let rest = before.AddRange innerMa.Elements
                match MACurs.DeleteFromLeft(MA.Row after) with
                | ValueSome after -> MACurs.MakeRow(rest, after, ImmutableArray.Empty) |> Choice1Of2
                | ValueNone -> MA.OfElements rest |> Choice2Of2
        | ScriptMainSuper(main, super, sub) ->
            match main.Delete with
            | Choice1Of2 main -> ScriptMainSuper(main, super, sub) |> Choice1Of2
            | Choice2Of2 main ->
                let baseAtom =
                    match sub with
                    | ValueSome sub -> MA.ScriptSub(main, sub)
                    | ValueNone -> main
                MACurs.Between(baseAtom, super) |> Choice1Of2
        | ScriptMainSub(main, sub) ->
            match main.Delete with
            | Choice1Of2 main -> ScriptMainSub(main, sub) |> Choice1Of2
            | Choice2Of2 main -> MACurs.Between(main, sub) |> Choice1Of2
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
            | Choice2Of2 n -> MACurs.Between(n, d) |> Choice1Of2
        | FracDen(n, d) ->
            match d.Delete with
            | Choice1Of2 d -> FracDen(n, d) |> Choice1Of2
            | Choice2Of2 d -> MACurs.AtEnd(MA.Row2(n, d)) |> Choice1Of2
        | Bracketed(b, inner, completion) ->
            match inner.Delete with
            | Choice1Of2 inner -> Bracketed(b, inner, completion) |> Choice1Of2
            | Choice2Of2 inner -> MACurs.AtEnd inner |> Choice1Of2
        | RootNDegree(n, x) ->
            match n.Delete with
            | Choice1Of2 n -> RootNDegree(n, x) |> Choice1Of2
            | Choice2Of2 n -> MACurs.Between(n, x) |> Choice1Of2
        | RootNMain(n, x) ->
            match x.Delete with
            | Choice1Of2 x -> RootNMain(n, x) |> Choice1Of2
            | Choice2Of2 x -> MACurs.AtEnd(MA.Row2(n, x)) |> Choice1Of2
        | Sqrt x ->
            match x.Delete with
            | Choice1Of2 x -> Sqrt x |> Choice1Of2
            | Choice2Of2 x -> MACurs.AtEnd x |> Choice1Of2

    /// Cursor at the start of an MA's first editable part. ValueNone if it has none.
    static member private EnterFromLeft(ma: MA): MACurs voption =
        match ma with
        | MA.Row _ | MA.Char _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD
        | MA.Function _ | MA.BigOp _ | MA.Accented _ | MA.Spanned _ | MA.Overline _
        | MA.Underline _ | MA.Stack _ | MA.Table _ | MA.Text _ | MA.Space _ | MA.Coloured _ ->
            ValueNone
        | MA.ScriptSuper(main, super, sub) -> ScriptMainSuper(MACurs.AtStart main, super, sub) |> ValueSome
        | MA.ScriptSub(main, sub) -> ScriptMainSub(MACurs.AtStart main, sub) |> ValueSome
        | MA.Frac(n, d) -> FracNum(MACurs.AtStart n, d) |> ValueSome
        | MA.Bracketed(b, inner, bc) -> Bracketed(b, MACurs.AtStart inner, bc) |> ValueSome
        | MA.RootN(n, x) -> RootNDegree(MACurs.AtStart n, x) |> ValueSome
        | MA.Sqrt x -> Sqrt(MACurs.AtStart x) |> ValueSome

    /// Cursor at the end of an MA's last editable part. ValueNone if it has none.
    static member private EnterFromRight(ma: MA): MACurs voption =
        match ma with
        | MA.Row _ | MA.Char _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD
        | MA.Function _ | MA.BigOp _ | MA.Accented _ | MA.Spanned _ | MA.Overline _
        | MA.Underline _ | MA.Stack _ | MA.Table _ | MA.Text _ | MA.Space _ | MA.Coloured _ ->
            ValueNone
        | MA.ScriptSuper(main, super, sub) ->
            match sub with
            | ValueSome sub -> ScriptSub(main, ValueSome super, MACurs.AtEnd sub) |> ValueSome
            | ValueNone -> ScriptSuper(main, MACurs.AtEnd super, ValueNone) |> ValueSome
        | MA.ScriptSub(main, sub) -> ScriptSub(main, ValueNone, MACurs.AtEnd sub) |> ValueSome
        | MA.Frac(n, d) -> FracDen(n, MACurs.AtEnd d) |> ValueSome
        | MA.Bracketed(b, inner, bc) -> Bracketed(b, MACurs.AtEnd inner, bc) |> ValueSome
        | MA.RootN(n, x) -> RootNMain(n, MACurs.AtEnd x) |> ValueSome
        | MA.Sqrt x -> Sqrt(MACurs.AtEnd x) |> ValueSome

    /// The end of the slot this stands in, where an atom filling it on its own keeps no row to
    /// step out into. ValueNone where the cursor is already there.
    member private t.OutRight: MACurs voption =
        let atEnd = MACurs.AtEnd t.ToMA
        if atEnd = t then ValueNone else ValueSome atEnd

    /// The start of the slot this stands in, which the same goes for.
    member private t.OutLeft: MACurs voption =
        let atStart = MACurs.AtStart t.ToMA
        if atStart = t then ValueNone else ValueSome atStart

    /// Moves the cursor one place right without leaving this MACurs. ValueNone if it is already at the right-hand end.
    member private t.MoveRightWithin: MACurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            match inner.MoveRightWithin with
            | ValueSome inner -> MACurs.MakeRow(before, inner, after) |> ValueSome
            | ValueNone ->
                match inner with
                | CursorOrEmpty ->
                    if after.IsEmpty then ValueNone
                    else
                        let next = after.[0]
                        let rest = after.RemoveAt 0
                        match MACurs.EnterFromLeft next with
                        | ValueSome entered -> MACurs.MakeRow(before, entered, rest) |> ValueSome
                        | ValueNone -> MACurs.MakeRow(before.Add next, CursorOrEmpty, rest) |> ValueSome
                | _ -> MACurs.MakeRow(before.AddRange inner.ToMA.Elements, CursorOrEmpty, after) |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            match main.Rightwards with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptSuper(main.ToMA, MACurs.AtStart super, sub) |> ValueSome
        | ScriptMainSub(main, sub) ->
            match main.Rightwards with
            | ValueSome main -> ScriptMainSub(main, sub) |> ValueSome
            | ValueNone -> ScriptSub(main.ToMA, ValueNone, MACurs.AtStart sub) |> ValueSome
        | ScriptSuper(main, super, sub) ->
            match super.Rightwards with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone ->
                sub |> ValueOption.map (fun sub -> ScriptSub(main, ValueSome super.ToMA, MACurs.AtStart sub))
        | ScriptSub(main, super, sub) ->
            sub.Rightwards |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) ->
            match n.Rightwards with
            | ValueSome n -> FracNum(n, d) |> ValueSome
            | ValueNone -> FracDen(n.ToMA, MACurs.AtStart d) |> ValueSome
        | FracDen(n, d) -> d.Rightwards |> ValueOption.map (fun d -> FracDen(n, d))
        | Bracketed(b, inner, bc) ->
            inner.Rightwards |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) ->
            match n.Rightwards with
            | ValueSome n -> RootNDegree(n, x) |> ValueSome
            | ValueNone -> RootNMain(n.ToMA, MACurs.AtStart x) |> ValueSome
        | RootNMain(n, x) -> x.Rightwards |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> x.Rightwards |> ValueOption.map (fun x -> Sqrt x)

    /// Rightwards inside a slot, which is one place along and then the end of the slot itself.
    member private t.Rightwards =
        t.MoveRightWithin |> ValueOption.orElseWith (fun () -> t.OutRight)

    /// Moves the cursor one place left without leaving this MACurs. ValueNone if it is already at the left-hand end.
    member private t.MoveLeftWithin: MACurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            match inner.MoveLeftWithin with
            | ValueSome inner -> MACurs.MakeRow(before, inner, after) |> ValueSome
            | ValueNone ->
                match inner with
                | CursorOrEmpty ->
                    if before.IsEmpty then ValueNone
                    else
                        let previous = before.[before.Length - 1]
                        let rest = before.RemoveAt(before.Length - 1)
                        match MACurs.EnterFromRight previous with
                        | ValueSome entered -> MACurs.MakeRow(rest, entered, after) |> ValueSome
                        | ValueNone -> MACurs.MakeRow(rest, CursorOrEmpty, after.Insert(0, previous)) |> ValueSome
                | _ -> MACurs.MakeRow(before, CursorOrEmpty, inner.ToMA.Elements.AddRange after) |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            main.Leftwards |> ValueOption.map (fun main -> ScriptMainSuper(main, super, sub))
        | ScriptMainSub(main, sub) ->
            main.Leftwards |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            match super.Leftwards with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptMainSuper(MACurs.AtEnd main, super.ToMA, sub) |> ValueSome
        | ScriptSub(main, super, sub) ->
            match sub.Leftwards with
            | ValueSome sub -> ScriptSub(main, super, sub) |> ValueSome
            | ValueNone ->
                match super with
                | ValueSome super -> ScriptSuper(main, MACurs.AtEnd super, ValueSome sub.ToMA) |> ValueSome
                | ValueNone -> ScriptMainSub(MACurs.AtEnd main, sub.ToMA) |> ValueSome
        | FracNum(n, d) -> n.Leftwards |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) ->
            match d.Leftwards with
            | ValueSome d -> FracDen(n, d) |> ValueSome
            | ValueNone -> FracNum(MACurs.AtEnd n, d.ToMA) |> ValueSome
        | Bracketed(b, inner, bc) ->
            inner.Leftwards |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) -> n.Leftwards |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) ->
            match x.Leftwards with
            | ValueSome x -> RootNMain(n, x) |> ValueSome
            | ValueNone -> RootNDegree(MACurs.AtEnd n, x.ToMA) |> ValueSome
        | Sqrt x -> x.Leftwards |> ValueOption.map (fun x -> Sqrt x)

    /// Leftwards inside a slot, which is one place back and then the start of the slot itself.
    member private t.Leftwards =
        t.MoveLeftWithin |> ValueOption.orElseWith (fun () -> t.OutLeft)

    /// Moves the cursor one place right, leaving the outermost atom if needed. ValueNone at the end of the formula.
    member t.Right: MACurs voption =
        match t.MoveRightWithin with
        | ValueSome moved -> ValueSome moved
        | ValueNone ->
            match t with
            | CursorOrEmpty -> ValueNone
            | Row(_, CursorOrEmpty, after) when after.IsEmpty -> ValueNone
            | _ -> MACurs.AtEnd t.ToMA |> ValueSome

    /// Moves the cursor one place left, leaving the outermost atom if needed. ValueNone at the start of the formula.
    member t.Left: MACurs voption =
        match t.MoveLeftWithin with
        | ValueSome moved -> ValueSome moved
        | ValueNone ->
            match t with
            | CursorOrEmpty -> ValueNone
            | Row(before, CursorOrEmpty, _) when before.IsEmpty -> ValueNone
            | _ -> MACurs.AtStart t.ToMA |> ValueSome

    /// Moves the cursor to the start of the block above, e.g. the numerator from the denominator. ValueNone if there is none.
    member t.Up: MACurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            inner.Up |> ValueOption.map (fun inner -> MACurs.MakeRow(before, inner, after))
        | ScriptMainSuper(main, super, sub) ->
            match main.Up with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptSuper(main.ToMA, MACurs.AtStart super, sub) |> ValueSome
        | ScriptMainSub(main, sub) -> main.Up |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            super.Up |> ValueOption.map (fun super -> ScriptSuper(main, super, sub))
        | ScriptSub(main, super, sub) ->
            match sub.Up with
            | ValueSome sub -> ScriptSub(main, super, sub) |> ValueSome
            | ValueNone ->
                match super with
                | ValueSome super -> ScriptMainSuper(MACurs.AtStart main, super, ValueSome sub.ToMA) |> ValueSome
                | ValueNone -> ScriptMainSub(MACurs.AtStart main, sub.ToMA) |> ValueSome
        | FracNum(n, d) -> n.Up |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) ->
            match d.Up with
            | ValueSome d -> FracDen(n, d) |> ValueSome
            | ValueNone -> FracNum(MACurs.AtStart n, d.ToMA) |> ValueSome
        | Bracketed(b, inner, bc) -> inner.Up |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) -> n.Up |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) ->
            match x.Up with
            | ValueSome x -> RootNMain(n, x) |> ValueSome
            | ValueNone -> RootNDegree(MACurs.AtStart n, x.ToMA) |> ValueSome
        | Sqrt x -> x.Up |> ValueOption.map (fun x -> Sqrt x)

    /// Moves the cursor to the start of the block below, e.g. the denominator from the numerator. ValueNone if there is none.
    member t.Down: MACurs voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            inner.Down |> ValueOption.map (fun inner -> MACurs.MakeRow(before, inner, after))
        | ScriptMainSuper(main, super, sub) ->
            match main.Down with
            | ValueSome main -> ScriptMainSuper(main, super, sub) |> ValueSome
            | ValueNone ->
                sub |> ValueOption.map (fun sub -> ScriptSub(main.ToMA, ValueSome super, MACurs.AtStart sub))
        | ScriptMainSub(main, sub) ->
            match main.Down with
            | ValueSome main -> ScriptMainSub(main, sub) |> ValueSome
            | ValueNone -> ScriptSub(main.ToMA, ValueNone, MACurs.AtStart sub) |> ValueSome
        | ScriptSuper(main, super, sub) ->
            match super.Down with
            | ValueSome super -> ScriptSuper(main, super, sub) |> ValueSome
            | ValueNone -> ScriptMainSuper(MACurs.AtStart main, super.ToMA, sub) |> ValueSome
        | ScriptSub(main, super, sub) ->
            sub.Down |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) ->
            match n.Down with
            | ValueSome n -> FracNum(n, d) |> ValueSome
            | ValueNone -> FracDen(n.ToMA, MACurs.AtStart d) |> ValueSome
        | FracDen(n, d) -> d.Down |> ValueOption.map (fun d -> FracDen(n, d))
        | Bracketed(b, inner, bc) -> inner.Down |> ValueOption.map (fun inner -> Bracketed(b, inner, bc))
        | RootNDegree(n, x) ->
            match n.Down with
            | ValueSome n -> RootNDegree(n, x) |> ValueSome
            | ValueNone -> RootNMain(n.ToMA, MACurs.AtStart x) |> ValueSome
        | RootNMain(n, x) -> x.Down |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> x.Down |> ValueOption.map (fun x -> Sqrt x)

    member t.Move(direction: Direction): MACurs voption =
        match direction with
        | Direction.Left -> t.Left
        | Direction.Right -> t.Right
        | Direction.Up -> t.Up
        | Direction.Down -> t.Down
        | _ -> ValueNone

    /// Replaces the atoms before the cursor with one built from as many of them as takes asks for.
    /// It is given an empty slot where it asks for none, or where the cursor has none before it.
    member t.ReplaceBefore(takes: ImmutableArray<MA> -> int, build: MA -> MACurs): MACurs =
        let again(curs: MACurs) = curs.ReplaceBefore(takes, build)
        match t with
        | CursorOrEmpty -> build MA.Empty
        | Row(before, inner, after) ->
            if not inner.IsCursorOrEmpty then MACurs.MakeRow(before, again inner, after)
            else
                let kept = before.Length - takes before
                MACurs.MakeRow(
                    before |> ImmArray.truncate kept,
                    build(MA.OfElements(before.RemoveRange(0, kept))),
                    after)
        | ScriptMainSuper(main, super, sub) -> ScriptMainSuper(again main, super, sub)
        | ScriptMainSub(main, sub) -> ScriptMainSub(again main, sub)
        | ScriptSuper(main, super, sub) -> ScriptSuper(main, again super, sub)
        | ScriptSub(main, super, sub) -> ScriptSub(main, super, again sub)
        | FracNum(n, d) -> FracNum(again n, d)
        | FracDen(n, d) -> FracDen(n, again d)
        | Bracketed(b, inner, bc) -> Bracketed(b, again inner, bc)
        | RootNDegree(n, x) -> RootNDegree(again n, x)
        | RootNMain(n, x) -> RootNMain(n, again x)
        | Sqrt x -> Sqrt(again x)

    /// The pair of the innermost bracketed atom the cursor stands in, where its closing bracket
    /// is still to be typed. ValueNone where the cursor stands in no such atom.
    member t.Unclosed: Brackets voption =
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(_, inner, _) -> inner.Unclosed
        | Bracketed(b, inner, completion) ->
            match inner.Unclosed with
            | ValueSome found -> ValueSome found
            | ValueNone -> if completion.RightCompleted then ValueNone else ValueSome b
        | ScriptMainSuper(main, _, _) | ScriptMainSub(main, _) -> main.Unclosed
        | ScriptSuper(_, super, _) -> super.Unclosed
        | ScriptSub(_, _, sub) -> sub.Unclosed
        | FracNum(n, _) -> n.Unclosed
        | FracDen(_, d) -> d.Unclosed
        | RootNDegree(n, _) -> n.Unclosed
        | RootNMain(_, x) -> x.Unclosed
        | Sqrt x -> x.Unclosed

    /// Replaces the atoms after the cursor with one built from all of them, and stands in it.
    member t.ReplaceAfter(build: MA -> MACurs): MACurs =
        let again(curs: MACurs) = curs.ReplaceAfter build
        match t with
        | CursorOrEmpty -> build MA.Empty
        | Row(before, inner, after) ->
            if not inner.IsCursorOrEmpty then MACurs.MakeRow(before, again inner, after)
            else MACurs.MakeRow(before, build(MA.OfElements after), ImmutableArray.Empty)
        | ScriptMainSuper(main, super, sub) -> ScriptMainSuper(again main, super, sub)
        | ScriptMainSub(main, sub) -> ScriptMainSub(again main, sub)
        | ScriptSuper(main, super, sub) -> ScriptSuper(main, again super, sub)
        | ScriptSub(main, super, sub) -> ScriptSub(main, super, again sub)
        | FracNum(n, d) -> FracNum(again n, d)
        | FracDen(n, d) -> FracDen(n, again d)
        | Bracketed(b, inner, bc) -> Bracketed(b, again inner, bc)
        | RootNDegree(n, x) -> RootNDegree(again n, x)
        | RootNMain(n, x) -> RootNMain(n, again x)
        | Sqrt x -> Sqrt(again x)

    /// The atoms before and after the cursor in the slot it stands among.
    /// ValueNone where the cursor stands inside an atom rather than among them.
    member t.Split: struct (ImmutableArray<MA> * ImmutableArray<MA>) voption =
        match t with
        | CursorOrEmpty -> ValueSome(struct (ImmutableArray.Empty, ImmutableArray.Empty))
        | Row(before, CursorOrEmpty, after) -> ValueSome(struct (before, after))
        | Row _ | ScriptMainSuper _ | ScriptMainSub _ | ScriptSuper _ | ScriptSub _ | FracNum _
        | FracDen _ | Bracketed _ | RootNDegree _ | RootNMain _ | Sqrt _ -> ValueNone

    /// Opens the innermost bracketed atom the cursor stands among with the bracket given, leaving
    /// what was before the cursor outside it. ValueNone where no such atom waits for one.
    member t.OpenBracket(left: Bracket): MACurs voption =
        let opened(inner: MACurs) = inner.OpenBracket left
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            opened inner |> ValueOption.map (fun inner -> MACurs.MakeRow(before, inner, after))
        | Bracketed(b, inner, completion) ->
            match opened inner with
            | ValueSome inner -> Bracketed(b, inner, completion) |> ValueSome
            | ValueNone ->
                match (if completion.LeftCompleted then ValueNone else inner.Split) with
                | ValueNone -> ValueNone
                | ValueSome(struct (before, after)) ->
                    MACurs.MakeRow(
                        before,
                        Bracketed(
                            Brackets(left, b.Right),
                            MACurs.MakeRow(ImmutableArray.Empty, CursorOrEmpty, after),
                            BracketCompletion.Completed),
                        ImmutableArray.Empty)
                    |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            opened main |> ValueOption.map (fun main -> ScriptMainSuper(main, super, sub))
        | ScriptMainSub(main, sub) -> opened main |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            opened super |> ValueOption.map (fun super -> ScriptSuper(main, super, sub))
        | ScriptSub(main, super, sub) ->
            opened sub |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) -> opened n |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) -> opened d |> ValueOption.map (fun d -> FracDen(n, d))
        | RootNDegree(n, x) -> opened n |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) -> opened x |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> opened x |> ValueOption.map (fun x -> Sqrt x)

    /// Closes the innermost bracketed atom the cursor stands in with the bracket given, leaving the
    /// cursor after it. ValueNone where the cursor stands in no bracketed atom at all.
    member t.CloseBracket(right: Bracket): MACurs voption =
        let closed(inner: MACurs) = inner.CloseBracket right
        match t with
        | CursorOrEmpty -> ValueNone
        | Row(before, inner, after) ->
            closed inner |> ValueOption.map (fun inner -> MACurs.MakeRow(before, inner, after))
        | Bracketed(b, inner, completion) ->
            match closed inner with
            | ValueSome inner -> Bracketed(b, inner, completion) |> ValueSome
            | ValueNone ->
                let closing(inner: MA) = MA.Bracketed(Brackets(b.Left, right), inner, BracketCompletion.Completed)
                match (if completion.RightCompleted then ValueNone else inner.Split) with
                | ValueNone -> closing(inner.ToMA) |> MACurs.AtEnd |> ValueSome
                | ValueSome(struct (before, after)) ->
                    MACurs.MakeRow(
                        ImmutableArray.Create(closing(MA.OfElements before)),
                        CursorOrEmpty,
                        after)
                    |> ValueSome
        | ScriptMainSuper(main, super, sub) ->
            closed main |> ValueOption.map (fun main -> ScriptMainSuper(main, super, sub))
        | ScriptMainSub(main, sub) -> closed main |> ValueOption.map (fun main -> ScriptMainSub(main, sub))
        | ScriptSuper(main, super, sub) ->
            closed super |> ValueOption.map (fun super -> ScriptSuper(main, super, sub))
        | ScriptSub(main, super, sub) ->
            closed sub |> ValueOption.map (fun sub -> ScriptSub(main, super, sub))
        | FracNum(n, d) -> closed n |> ValueOption.map (fun n -> FracNum(n, d))
        | FracDen(n, d) -> closed d |> ValueOption.map (fun d -> FracDen(n, d))
        | RootNDegree(n, x) -> closed n |> ValueOption.map (fun n -> RootNDegree(n, x))
        | RootNMain(n, x) -> closed x |> ValueOption.map (fun x -> RootNMain(n, x))
        | Sqrt x -> closed x |> ValueOption.map (fun x -> Sqrt x)

    /// Adds an MACurs naively
    member t.AddMACurs(addition: MACurs): MACurs =
        match t with
        | CursorOrEmpty -> addition
        | Row(before, inner, after) -> MACurs.MakeRow(before, inner.AddMACurs addition, after)
        | ScriptMainSuper(main, super, sub) -> ScriptMainSuper(main.AddMACurs addition, super, sub)
        | ScriptMainSub(main, sub) -> ScriptMainSub(main.AddMACurs addition, sub)
        | ScriptSuper(main, super, sub) -> ScriptSuper(main, super.AddMACurs addition, sub)
        | ScriptSub(main, super, sub) -> ScriptSub(main, super, sub.AddMACurs addition)
        | FracNum(n, d) -> FracNum(n.AddMACurs addition, d)
        | FracDen(n, d) -> FracDen(n, d.AddMACurs addition)
        | Bracketed(b, inner, bc) -> Bracketed(b, inner.AddMACurs addition, bc)
        | RootNDegree(n, x) -> RootNDegree(n.AddMACurs addition, x)
        | RootNMain(n, x) -> RootNMain(n, x.AddMACurs addition)
        | Sqrt x -> Sqrt(x.AddMACurs addition)

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
        let spelled(name: string) =
            FunctionNames.table |> Array.tryPick (fun (spelling, fn) -> if spelling = name then Some fn else None)
        /// The name the function standing before the letters was spelled with, if one stands there.
        let before =
            if start = 0 then None
            else
                match elements.[start - 1] with
                | MA.Function fn ->
                    FunctionNames.table
                    |> Array.tryPick (fun (spelling, found) -> if found = fn then Some spelling else None)
                | _ -> None
        let matched =
            FunctionNames.table
            |> Array.tryFind (fun (name, _) -> letters.EndsWith(name, StringComparison.Ordinal))
        match matched with
        | Some(name, fn) -> (elements |> ImmArray.truncate (elements.Length - name.Length)).Add(MA.Function fn)
        | None ->
            // A name no run of letters spells may be one a function before them goes on to spell.
            match before |> Option.bind (fun name -> spelled(name + letters)) with
            | Some fn -> (elements |> ImmArray.truncate (start - 1)).Add(MA.Function fn)
            | None -> elements

    /// Adds a character at the cursor, replacing a completed function name with that function.
    member t.AddAlphanumeric(c: char): MACurs =
        match t with
        | CursorOrEmpty -> MACurs.AtEnd(MA.Char c)
        | Row(before, CursorOrEmpty, after) ->
            MACurs.MakeRow(MACurs.RecogniseFunction(before.Add(MA.Char c)), CursorOrEmpty, after)
        | Row(before, inner, after) -> MACurs.MakeRow(before, inner.AddAlphanumeric c, after)
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
        let props(name: string, xs: objnull seq) =
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
