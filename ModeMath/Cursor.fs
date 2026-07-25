namespace ModeMath

open System
open System.Collections.Immutable

module internal FunctionNames =
    /// Longest first, so that "asin" wins over "sin".
    let table =
        [| "sin", MathFunction.Sin
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
           "max", MathFunction.Max |]
        |> Array.sortByDescending (fun (name, _) -> name.Length)

/// A formula with a cursor in it: one child is a MICurs and the rest are plain MA,
/// so the path from the root to the cursor is encoded in the value itself.
[<RequireQualifiedAccess>]
type MICurs =
    /// A cursor (e.g. in a row), or empty position (e.g. empty superscript).
    | CursorOrEmpty
    /// A row split at the cursor. Use MakeRow rather than this case directly.
    | Row of before: ImmutableArray<MA> * MICurs * after: ImmutableArray<MA>
    /// In the main part of a script containing a superscript
    | ScriptMainSuper of main: MICurs * super: MA * sub: MA voption
    /// In the main part of a script containing a subscript only
    | ScriptMainSub of main: MICurs * sub: MA
    | ScriptSuper of main: MA * super: MICurs * sub: MA voption
    | ScriptSub of main: MA * super: MA voption * sub: MICurs
    | FracNum of n: MICurs * d: MA
    | FracDen of n: MA * d: MICurs
    | Bracketed of Bracket * MICurs * completion: BracketCompletion
    | RootNDegree of n: MICurs * x: MA
    | RootNMain of n: MA * x: MICurs
    | Sqrt of x: MICurs

    /// Builds a Row, collapsing an empty split and merging a nested Row so that a
    /// cursor in a row is always represented by exactly one Row case.
    static member MakeRow(before: ImmutableArray<MA>, inner: MICurs, after: ImmutableArray<MA>) =
        match inner with
        | Row(b, i, a) -> MICurs.MakeRow(before.AddRange b, i, a.AddRange after)
        | _ when before.IsEmpty && after.IsEmpty -> inner
        | _ -> Row(before, inner, after)

    /// Cursor at the right-hand end of an atom.
    static member AtEnd(ma: MA) = MICurs.MakeRow(ma.Elements, CursorOrEmpty, ImmutableArray.Empty)

    /// Cursor at the left-hand end of an atom.
    static member AtStart(ma: MA) = MICurs.MakeRow(ImmutableArray.Empty, CursorOrEmpty, ma.Elements)

    /// Cursor between two atoms.
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

    /// Cursor state after deleting leftwards from immediately right of an atom.
    /// ValueNone when the atom holds nothing to delete.
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
        | MA.Char _ | MA.BoldVar _ | MA.Cdot | MA.UprightD | MA.Function _ | MA.Operator _ ->
            ValueSome CursorOrEmpty
        | MA.ScriptSuper(main, super, sub) -> ScriptSuper(main, MICurs.AtEnd super, sub) |> ValueSome
        | MA.ScriptSub(main, sub) -> ScriptSub(main, ValueNone, MICurs.AtEnd sub) |> ValueSome
        | MA.Frac(n, d) -> FracNum(MICurs.AtEnd n, d) |> ValueSome
        | MA.Bracketed(b, inner, bc) ->
            match bc with
            | BracketCompletion.Left | BracketCompletion.Completed ->
                Bracketed(b, MICurs.AtEnd inner, BracketCompletion.Left) |> ValueSome
            | BracketCompletion.Right -> MICurs.AtEnd inner |> ValueSome
        | MA.RootN(n, x) -> RootNMain(n, MICurs.AtEnd x) |> ValueSome
        | MA.Sqrt x -> Sqrt(MICurs.AtEnd x) |> ValueSome

    /// Choice1 is the new cursor state. Choice2 means the cursor was already at the far left
    /// of this atom, and carries its plain content for the caller to place.
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
                // TODO: asymmetric with ScriptSuper above, which keeps the other script on the
                // base atom. Ported as-is from SummaticApp; check against the intended behaviour.
                match super with
                | ValueSome super -> MICurs.Between(main, MA.ScriptSuper(sub, super, ValueNone)) |> Choice1Of2
                | ValueNone -> MICurs.Between(main, sub) |> Choice1Of2
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

    /// Substitutes another cursored formula in at the cursor.
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
        | Bracketed(b, inner, completion) -> Bracketed(b, inner.AddMICurs addition, completion)
        | RootNDegree(n, x) -> RootNDegree(n.AddMICurs addition, x)
        | RootNMain(n, x) -> RootNMain(n, x.AddMICurs addition)
        | Sqrt x -> Sqrt(x.AddMICurs addition)

    /// Replaces a trailing run of letters spelling a function name with that function.
    static member private RecogniseFunction(elements: ImmutableArray<MA>) =
        let isLetter i =
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
        match FunctionNames.table |> Array.tryFind (fun (name, _) -> letters.EndsWith(name, StringComparison.Ordinal)) with
        | Some(name, fn) -> (elements |> ImmArray.take (elements.Length - name.Length)).Add(MA.Function fn)
        | None -> elements

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
        | Bracketed(b, inner, completion) -> Bracketed(b, inner.AddAlphanumeric c, completion)
        | RootNDegree(n, x) -> RootNDegree(n.AddAlphanumeric c, x)
        | RootNMain(n, x) -> RootNMain(n, x.AddAlphanumeric c)
        | Sqrt x -> Sqrt(x.AddAlphanumeric c)

    override t.ToString() =
        let props name (xs: obj seq) =
            name + "(" + (xs |> Seq.map string |> String.concat ", ") + ")"
        let row (l: ImmutableArray<MA>) = l |> Seq.map string |> String.concat " "
        match t with
        | CursorOrEmpty -> "|"
        | Row(before, inner, after) -> props "Row" [ box (row before); box inner; box (row after) ]
        | ScriptMainSuper(main, super, sub) -> props "ScriptMainSuper" [ box main; box super; box sub ]
        | ScriptMainSub(main, sub) -> props "ScriptMainSub" [ box main; box sub ]
        | ScriptSuper(main, super, sub) -> props "ScriptSuper" [ box main; box super; box sub ]
        | ScriptSub(main, super, sub) -> props "ScriptSub" [ box main; box super; box sub ]
        | FracNum(n, d) -> props "FracNum" [ box n; box d ]
        | FracDen(n, d) -> props "FracDen" [ box n; box d ]
        | Bracketed(b, inner, completion) -> props "Bracketed" [ box b; box inner; box completion ]
        | RootNDegree(n, x) -> props "RootNDegree" [ box n; box x ]
        | RootNMain(n, x) -> props "RootNMain" [ box n; box x ]
        | Sqrt x -> props "Sqrt" [ box x ]
