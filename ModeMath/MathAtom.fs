namespace ModeMath

open System.Collections.Immutable
open System.Drawing
open FSUtils

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

/// Operators large enough to carry limits, which sit above and below them in display style.
type BigOperator =
    | Sum = 0
    | Product = 1
    | Coproduct = 2
    | Integral = 3
    | ContourIntegral = 4
    | Union = 5
    | Intersection = 6
    /// Set in upright letters rather than drawn from a glyph.
    | Limit = 7

/// A mark set over an atom, which does not change what the atom is.
type Accent =
    | Hat = 0
    | Tilde = 1
    | Bar = 2
    | Vec = 3
    | Dot = 4
    | DoubleDot = 5
    | Check = 6
    | Acute = 7
    | Grave = 8
    | Breve = 9
    /// The circumflex of \widehat, which takes a size that covers its base.
    | WideHat = 10
    /// The tilde of \widetilde, which takes a size that covers its base.
    | WideTilde = 11

/// A mark grown to span what it is set over or under.
type Spanning =
    | Overbrace = 0
    | Underbrace = 1
    | Overrightarrow = 2

/// A gap of a fixed width, named for the TeX command that gives it.
type Space =
    /// 3/18 of an em, as \, gives.
    | Thin = 0
    /// 4/18 of an em, as \: gives.
    | Medium = 1
    /// 5/18 of an em, as \; gives.
    | Thick = 2
    /// Back 3/18 of an em, as \! gives.
    | NegativeThin = 3
    /// A full em, as \quad gives.
    | Quad = 4
    /// Two ems, as \qquad gives.
    | QQuad = 5

/// Where a table's cells sit in the column they share.
type Alignment =
    | Centre = 0
    | Left = 1
    | Right = 2

/// A bracket shape, side-agnostic: Normal draws ( on the left and ) on the right.
type Bracket =
    | Normal = 0
    | Line = 1
    | Square = 2
    | Curly = 3
    | Angle = 4
    /// Nothing at all, as \left. leaves a side of a formula open.
    | None = 5

/// The pair a formula is bracketed with, which need not match: [0, 1) is a square left and a round right.
[<Struct>]
type Brackets(left: Bracket, right: Bracket) =
    member _.Left = left
    member _.Right = right
    /// The pair entry produces, both sides the same shape.
    static member Matching(bracket: Bracket) = Brackets(bracket, bracket)

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
    /// A blackboard bold capital, which the second face draws.
    | Blackboard of char
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
    | Bracketed of Brackets * MA * BracketCompletion
    | RootN of n: MA * x: MA
    | Sqrt of x: MA
    /// A large operator with its limits, which are scripts in every style but display.
    | BigOp of op: BigOperator * lower: MA voption * upper: MA voption
    | Accented of accent: Accent * x: MA
    | Overline of x: MA
    | Underline of x: MA
    /// A fraction with no rule between its parts, which brackets turn into a binomial coefficient.
    | Stack of top: MA * bottom: MA
    /// A grid of cells, whose columns take the alignments in turn, repeating. None centres them all.
    | Table of cells: ImmA2D<MA> * alignments: ImmutableArray<Alignment>
    | Spanned of mark: Spanning * x: MA
    /// A formula in a colour. TODO: Argb<byte> once .NET 11 lands; Color is 24 bytes and a pointer.
    | Coloured of colour: Color * x: MA
    /// Words set upright among the mathematics, as \text does.
    | Text of string
    | Space of Space

    static member Empty = Row ImmutableArray<MA>.Empty
    static member Row2(a: MA, b: MA) = Row(ImmutableArray.Create(a, b))
    static member Row3(a: MA, b: MA, c: MA) = Row(ImmutableArray.Create(a, b, c))
    static member String(s: string) = Row(s.ToImmutableArray() |> ImmArray.map Char)
    /// A matching pair around x, as entering a bracket produces.
    static member Paired(bracket: Bracket, x: MA) =
        Bracketed(Brackets.Matching bracket, x, BracketCompletion.Completed)

    static member RoundBracket(x: MA) = MA.Paired(Bracket.Normal, x)

    /// A binomial coefficient: a stack in round brackets, as \binom sets one.
    static member Binom(top: MA, bottom: MA) = MA.RoundBracket(Stack(top, bottom))

    /// A grid with every column centred, as a matrix is set.
    static member Matrix(cells: ImmA2D<MA>) = Table(cells, ImmutableArray<Alignment>.Empty)

    /// A left-aligned grid behind an opening brace, as a definition by cases is set.
    static member Cases(cells: ImmA2D<MA>) =
        Bracketed(
            Brackets(Bracket.Curly, Bracket.None),
            Table(cells, ImmutableArray.Create Alignment.Left),
            BracketCompletion.Completed)

    /// Where the cells of a column sit, columns past the end of the alignments taking them again.
    static member AlignmentOf(alignments: ImmutableArray<Alignment>, column: int) =
        if alignments.IsEmpty then Alignment.Centre else alignments.[column % alignments.Length]

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
        | Char _ | BoldVar _ | Blackboard _ | Cdot | UprightD | Function _ | Operator _
        | Text _ | Space _ -> t
        | ScriptSuper(main, super, sub) ->
            ScriptSuper(main.Flatten, super.Flatten, sub |> ValueOption.map (fun s -> s.Flatten))
        | ScriptSub(main, sub) -> ScriptSub(main.Flatten, sub.Flatten)
        | Frac(n, d) -> Frac(n.Flatten, d.Flatten)
        | Bracketed(b, ma, bc) -> Bracketed(b, ma.Flatten, bc)
        | RootN(n, x) -> RootN(n.Flatten, x.Flatten)
        | Sqrt x -> Sqrt x.Flatten
        | BigOp(op, lower, upper) ->
            BigOp(
                op,
                lower |> ValueOption.map (fun l -> l.Flatten),
                upper |> ValueOption.map (fun u -> u.Flatten))
        | Accented(accent, x) -> Accented(accent, x.Flatten)
        | Spanned(mark, x) -> Spanned(mark, x.Flatten)
        | Coloured(colour, x) -> Coloured(colour, x.Flatten)
        | Overline x -> Overline x.Flatten
        | Underline x -> Underline x.Flatten
        | Stack(top, bottom) -> Stack(top.Flatten, bottom.Flatten)
        | Table(cells, alignments) -> Table(cells |> ImmA2D.map (fun cell -> cell.Flatten), alignments)


    override t.ToString() =
        let props(name: string, xs: obj seq) =
            name + "(" + (xs |> Seq.map string |> String.concat ", ") + ")"
        match t with
        | Row l -> props("Row", l |> Seq.map box)
        | Char c -> string c
        | BoldVar c -> props("BoldVar", [ box c ])
        | Blackboard c -> props("Blackboard", [ box c ])
        | Text text -> props("Text", [ box text ])
        | Space space -> props("Space", [ box space ])
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
        | BigOp(op, lower, upper) -> props("BigOp", [ box op; box lower; box upper ])
        | Accented(accent, x) -> props("Accented", [ box accent; box x ])
        | Spanned(mark, x) -> props("Spanned", [ box mark; box x ])
        | Coloured(colour, x) -> props("Coloured", [ box colour; box x ])
        | Overline x -> props("Overline", [ x ])
        | Underline x -> props("Underline", [ x ])
        | Stack(top, bottom) -> props("Stack", [ top; bottom ])
        | Table(cells, alignments) ->
            let row(r: int) = box (props("Row", seq { for c in 0 .. cells.Cols - 1 -> box cells.[r, c] }))
            props(
                "Table",
                Seq.append (seq { for r in 0 .. cells.Rows - 1 -> row r }) (alignments |> Seq.map box))

type Direction =
    | Up = 0
    | Down = 1
    | Left = 2
    | Right = 3
