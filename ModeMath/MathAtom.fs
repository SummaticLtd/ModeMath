namespace ModeMath

open System.Collections.Immutable

module internal ImmArray =
    let map<'T, 'U> (f: 'T -> 'U) (a: ImmutableArray<'T>) =
        let b = ImmutableArray.CreateBuilder<'U> a.Length
        for x in a do b.Add(f x)
        b.MoveToImmutable()

    let forall<'T> (f: 'T -> bool) (a: ImmutableArray<'T>) =
        let mutable ok = true
        let mutable i = 0
        while ok && i < a.Length do
            ok <- f a.[i]
            i <- i + 1
        ok

    let take<'T> (n: int) (a: ImmutableArray<'T>) =
        let b = ImmutableArray.CreateBuilder<'T> n
        for i in 0 .. n - 1 do b.Add a.[i]
        b.MoveToImmutable()

    /// The largest f x over the array, or minimum, whichever is higher.
    let inline maxWithSafe<'T, 'U when 'U: comparison>
        (a: ImmutableArray<'T>, minimum: 'U, [<InlineIfLambda>] f: 'T -> 'U) =
        let mutable m = minimum
        for x in a do
            let fx = f x
            if fx > m then m <- fx
        m

type MathFunction =
    | Sin = 0
    | Cos = 1
    | Tan = 2
    | Asin = 3
    | Acos = 4
    | Atan = 5
    | Sinh = 6
    | Cosh = 7
    | Tanh = 8
    | Exp = 9
    | Log = 10
    | Ln = 11
    | Fact = 12
    | Sec = 13
    | Csc = 14
    | Cot = 15
    | Min = 16
    | Max = 17
    | Erf = 18
    | Indicator = 19
    | Identity = 20
    | Real = 21
    | Imaginary = 22
    | Gamma = 23
    | Sign = 24

type Operator =
    | Times = 0
    | Plus = 1
    | Minus = 2
    | Divide = 3
    | Equals = 4

type Bracket =
    | Normal = 0
    | Line = 1

[<Struct; RequireQualifiedAccess>]
type BracketCompletion =
    | Completed
    /// Left is done; right is tentative (grey)
    | Left
    /// Right is done; left is tentative (grey)
    | Right
    member t.LeftCompleted = match t with Completed | Left -> true | Right -> false
    member t.RightCompleted = match t with Completed | Right -> true | Left -> false

/// Math Atom: visual structure of a formula
[<RequireQualifiedAccess>]
type MA =
    | Row of ImmutableArray<MA>
    | Char of char
    | BoldVar of char
    | Cdot
    /// Upright d, for derivatives
    | UprightD
    /// A script containing a superscript, which may also have a subscript.
    | ScriptSuper of main: MA * super: MA * sub: MA voption
    /// A script without a superscript, which only has a subscript.
    | ScriptSub of main: MA * sub: MA
    | Frac of numerator: MA * denominator: MA
    | Function of MathFunction
    | Operator of Operator
    | Bracketed of Bracket * MA * BracketCompletion
    | RootN of n: MA * x: MA
    | Sqrt of x: MA

    static member Empty = Row ImmutableArray<MA>.Empty
    static member Row2(a: MA, b: MA) = Row(ImmutableArray.Create(a, b))
    static member Row3(a: MA, b: MA, c: MA) = Row(ImmutableArray.Create(a, b, c))
    static member String(s: string) = Row(s.ToImmutableArray() |> ImmArray.map Char)
    static member RoundBracket(x: MA) = Bracketed(Bracket.Normal, x, BracketCompletion.Completed)

    member t.IsEmpty =
        match t with
        | Row l -> l |> ImmArray.forall (fun ma -> ma.IsEmpty)
        | _ -> false

    member t.Elements =
        match t with
        | Row l -> l
        | _ -> ImmutableArray.Create t

    static member OfElements(elements: ImmutableArray<MA>) =
        if elements.Length = 1 then elements.[0] else Row elements

    /// Given a list of MAs, which may contain Rows, returns a flattened list which does not contain rows at the top level.
    static member FlattenElements(atoms: ImmutableArray<MA>) =
        let b = ImmutableArray.CreateBuilder<MA>()
        for ma in atoms do
            match ma.Flatten with
            | Row r -> for inner in r do b.Add inner
            | flattened -> b.Add flattened
        b.ToImmutable()

    /// Flattens rows
    member t.Flatten: MA =
        match t with
        | Row l -> MA.FlattenElements l |> MA.OfElements
        | Char _ | BoldVar _ | Cdot | UprightD | Function _ | Operator _ -> t
        | ScriptSuper(main, super, sub) ->
            ScriptSuper(main.Flatten, super.Flatten, sub |> ValueOption.map (fun s -> s.Flatten))
        | ScriptSub(main, sub) -> ScriptSub(main.Flatten, sub.Flatten)
        | Frac(n, d) -> Frac(n.Flatten, d.Flatten)
        | Bracketed(b, ma, bc) -> Bracketed(b, ma.Flatten, bc)
        | RootN(n, x) -> RootN(n.Flatten, x.Flatten)
        | Sqrt x -> Sqrt x.Flatten

    override t.ToString() =
        let props(name: string, xs: obj seq) =
            name + "(" + (xs |> Seq.map string |> String.concat ", ") + ")"
        match t with
        | Row l -> props("Row", l |> Seq.map box)
        | Char c -> string c
        | BoldVar c -> props("BoldVar", [ box c ])
        | Cdot -> "Cdot"
        | UprightD -> "UprightD"
        | ScriptSuper(main, super, sub) -> props("ScriptSuper", [ main; super; sub ])
        | ScriptSub(main, sub) -> props("ScriptSub", [ main; sub ])
        | Frac(n, d) -> props("Frac", [ n; d ])
        | Function f -> props("Fn", [ box f ])
        | Operator o -> props("Op", [ box o ])
        | Bracketed(b, ma, bc) -> props("Bracketed", [ box b; box ma; box bc ])
        | RootN(n, x) -> props("RootN", [ n; x ])
        | Sqrt x -> props("Sqrt", [ x ])

type Direction =
    | Up = 0
    | Down = 1
    | Left = 2
    | Right = 3
