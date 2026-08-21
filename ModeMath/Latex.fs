namespace ModeMath

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Drawing
open FSUtils

/// Why a LaTeX string could not be read, and where in it the reader stopped.
[<Struct>]
type LatexError(message: string, position: int) =
    member _.Message = message
    member _.Position = position
    override _.ToString() = $"{message}, at character {position}"

module internal Latexing =

    /// A control word or symbol stripped of its backslash, or a character LaTeX reads itself.
    [<RequireQualifiedAccess>]
    type Token =
        | Command of string
        | Char of char
        | Open
        | Close
        | Super
        | Sub
        | Cell
        | Break

    exception Rejected of string * int

    let fail(message: string, position: int) : 'a = raise(Rejected(message, position))

    /// A letter a control word may be spelled with, which θ is not though .NET counts it one.
    let private spells(c: char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    /// The tokens a string is made of, paired with where each begins.
    let lex(latex: string) =
        let tokens = ImmutableArray.CreateBuilder<struct (Token * int)>()
        let mutable i = 0
        while i < latex.Length do
            let start = i
            let take(token: Token) = tokens.Add(struct (token, start))
            let c = latex.[i]
            i <- i + 1
            match c with
            | '\\' ->
                if i < latex.Length && latex.[i] = '\\' then
                    i <- i + 1
                    take Token.Break
                elif i < latex.Length && spells latex.[i] then
                    let from = i
                    while i < latex.Length && spells latex.[i] do
                        i <- i + 1
                    take(Token.Command(latex.Substring(from, i - from)))
                elif i < latex.Length then
                    let symbol = latex.[i]
                    i <- i + 1
                    take(Token.Command(string symbol))
                else fail("a backslash ends the formula", start)
            | '{' -> take Token.Open
            | '}' -> take Token.Close
            | '^' -> take Token.Super
            | '_' -> take Token.Sub
            | '&' -> take Token.Cell
            | '$' -> fail("a formula is read in math mode already", start)
            | _ -> if not(Char.IsWhiteSpace c) then take(Token.Char c)
        tokens.ToImmutable()

    let private greek =
        [
            "alpha", 'α'; "beta", 'β'; "gamma", 'γ'; "Gamma", 'Γ'
            "delta", 'δ'; "Delta", 'Δ'; "epsilon", 'ϵ'; "varepsilon", 'ε'
            "zeta", 'ζ'; "eta", 'η'; "theta", 'θ'; "vartheta", 'ϑ'; "Theta", 'Θ'
            "iota", 'ι'; "kappa", 'κ'; "lambda", 'λ'; "Lambda", 'Λ'
            "mu", 'μ'; "nu", 'ν'; "xi", 'ξ'; "Xi", 'Ξ'; "pi", 'π'; "Pi", 'Π'
            "rho", 'ρ'; "varrho", 'ϱ'; "sigma", 'σ'; "Sigma", 'Σ'
            "tau", 'τ'; "upsilon", 'υ'; "Upsilon", 'Υ'; "phi", 'ϕ'; "varphi", 'φ'; "Phi", 'Φ'
            "chi", 'χ'; "psi", 'ψ'; "Psi", 'Ψ'; "omega", 'ω'; "Omega", 'Ω'
        ]

    let private marks =
        [
            "infty", '∞'; "partial", '∂'; "emptyset", '∅'
            "therefore", '∴'; "because", '∵'; "mid", '|'; "prime", '′'
            "cdots", '⋯'; "ldots", '…'; "dots", '…'; "vdots", '⋮'; "ddots", '⋱'
            "times", '×'; "div", '÷'; "cdot", '⋅'; "pm", '±'; "mp", '∓'; "ast", '∗'
            "cap", '∩'; "cup", '∪'; "wedge", '∧'; "land", '∧'; "vee", '∨'; "lor", '∨'
            "setminus", '∖'; "oplus", '⊕'; "otimes", '⊗'
            "leq", '≤'; "le", '≤'; "geq", '≥'; "ge", '≥'; "neq", '≠'; "ne", '≠'
            "approx", '≈'; "equiv", '≡'; "sim", '∼'; "cong", '≅'; "propto", '∝'
            "in", '∈'; "notin", '∉'; "subset", '⊂'; "subseteq", '⊆'
            "to", '→'; "rightarrow", '→'; "Rightarrow", '⇒'; "Leftrightarrow", '⇔'; "iff", '⟺'
            "neg", '¬'; "lnot", '¬'; "langle", '⟨'; "rangle", '⟩'
            "leftarrow", '←'; "gets", '←'; "Leftarrow", '⇐'; "leftrightarrow", '↔'; "mapsto", '↦'
            "Longleftrightarrow", '⟺'; "supset", '⊃'; "supseteq", '⊇'; "perp", '⊥'; "parallel", '∥'
            "angle", '∠'; "ell", 'ℓ'; "nabla", '∇'; "forall", '∀'; "exists", '∃'
            "dagger", '†'; "degree", '°'; "backslash", '\\'
        ]

    let private functions =
        [
            "sin", MathFunction.Sin; "cos", MathFunction.Cos; "tan", MathFunction.Tan
            "arcsin", MathFunction.Asin; "arccos", MathFunction.Acos; "arctan", MathFunction.Atan
            "sinh", MathFunction.Sinh; "cosh", MathFunction.Cosh; "tanh", MathFunction.Tanh
            "exp", MathFunction.Exp; "log", MathFunction.Log; "ln", MathFunction.Ln
            "sec", MathFunction.Sec; "csc", MathFunction.Csc; "cot", MathFunction.Cot
            "erf", MathFunction.Erf; "min", MathFunction.Min; "max", MathFunction.Max
            "Re", MathFunction.Real; "Im", MathFunction.Imaginary
        ]

    let private bigOps =
        [
            "sum", BigOperator.Sum; "prod", BigOperator.Product; "coprod", BigOperator.Coproduct
            "int", BigOperator.Integral; "oint", BigOperator.ContourIntegral
            "bigcup", BigOperator.Union; "bigcap", BigOperator.Intersection
            "lim", BigOperator.Limit
        ]

    let private accents =
        [
            "hat", Accent.Hat; "tilde", Accent.Tilde; "bar", Accent.Bar; "vec", Accent.Vec
            "dot", Accent.Dot; "ddot", Accent.DoubleDot; "check", Accent.Check
            "acute", Accent.Acute; "grave", Accent.Grave; "breve", Accent.Breve
            "widehat", Accent.WideHat; "widetilde", Accent.WideTilde
        ]

    let private spanning =
        [
            "overbrace", Spanning.Overbrace
            "underbrace", Spanning.Underbrace
            "overrightarrow", Spanning.Overrightarrow
        ]

    let private spaces =
        [
            ",", Space.Thin; ":", Space.Medium; ";", Space.Thick; "!", Space.NegativeThin
            " ", Space.Medium; "enspace", Space.Medium
            "quad", Space.Quad; "qquad", Space.QQuad
        ]

    /// What a command stands for, so that one table holds them all.
    [<RequireQualifiedAccess>]
    type Standing =
        | Symbol of char
        | Function of MathFunction
        | BigOp of BigOperator
        | Space of Space
        | Accent of Accent
        | Spanning of Spanning

    let commands =
        [
            for name, c in greek @ marks do
                yield KeyValuePair(name, Standing.Symbol c)
            for name, f in functions do
                yield KeyValuePair(name, Standing.Function f)
            for name, op in bigOps do
                yield KeyValuePair(name, Standing.BigOp op)
            for name, accent in accents do
                yield KeyValuePair(name, Standing.Accent accent)
            for name, mark in spanning do
                yield KeyValuePair(name, Standing.Spanning mark)
            for name, space in spaces do
                yield KeyValuePair(name, Standing.Space space)
        ]
        |> ImmutableDictionary.CreateRange

    /// A formula with its letters set in another alphabet, and whatever that alphabet lacks left alone.
    let rec private set(build: char -> MA voption, ma: MA) =
        match ma with
        | MA.Row elements -> MA.Row(elements |> ImmArray.map (fun element -> set(build, element)))
        | MA.Char c -> build c |> ValueOption.defaultValue ma
        | MA.ScriptSuper(main, super, sub) -> MA.ScriptSuper(set(build, main), super, sub)
        | MA.ScriptSub(main, sub) -> MA.ScriptSub(set(build, main), sub)
        | _ -> ma

    let bold(ma: MA) =
        set((fun c -> if spells c then ValueSome(MA.BoldVar c) else ValueNone), ma)

    let blackboard(ma: MA) =
        set((fun c -> if c >= 'A' && c <= 'Z' then ValueSome(MA.Blackboard c) else ValueNone), ma)

    /// The shape a delimiter draws, which is the same on either side of a pair.
    let delimiter(token: Token) =
        match token with
        | Token.Char('(' | ')') -> ValueSome Bracket.Normal
        | Token.Char('[' | ']') -> ValueSome Bracket.Square
        | Token.Char '|' -> ValueSome Bracket.Line
        | Token.Char '.' -> ValueSome Bracket.None
        | Token.Command("{" | "}" | "lbrace" | "rbrace") -> ValueSome Bracket.Curly
        | Token.Command("langle" | "rangle") -> ValueSome Bracket.Angle
        | Token.Command("lvert" | "rvert" | "vert" | "mid") -> ValueSome Bracket.Line
        | _ -> ValueNone

    /// The brackets a matrix environment is set in. ValueNone where it is set in none.
    let matrixBrackets(name: string) =
        match name with
        | "matrix" -> ValueSome ValueNone
        | "pmatrix" -> ValueSome(ValueSome Bracket.Normal)
        | "bmatrix" -> ValueSome(ValueSome Bracket.Square)
        | "Bmatrix" -> ValueSome(ValueSome Bracket.Curly)
        | "vmatrix" -> ValueSome(ValueSome Bracket.Line)
        | _ -> ValueNone

    let alignment(spelling: char) =
        match spelling with
        | 'l' -> ValueSome Alignment.Left
        | 'c' -> ValueSome Alignment.Centre
        | 'r' -> ValueSome Alignment.Right
        | _ -> ValueNone

    type Reader(tokens: ImmutableArray<struct (Token * int)>, source: string) =
        let mutable at = 0

        let here() =
            if at < tokens.Length then
                let struct (_, position) = tokens.[at]
                position
            else source.Length

        let peek() =
            if at < tokens.Length then
                let struct (token, _) = tokens.[at]
                ValueSome token
            else ValueNone

        let advance() = at <- at + 1

        /// Where a stray closing token stands, once the whole string should have been read.
        member _.Unread = if at < tokens.Length then ValueSome(here()) else ValueNone

        member private _.Ends =
            match peek() with
            | ValueNone
            | ValueSome(Token.Close | Token.Cell | Token.Break)
            | ValueSome(Token.Command("right" | "end")) -> true
            | _ -> false

        /// The atoms up to the end of the string or the token that closes what they stand in.
        member t.Formula() =
            let elements = ImmutableArray.CreateBuilder<MA>()
            while not t.Ends do
                elements.Add(t.Atom())
            MA.OfElements(elements.ToImmutable())

        member private t.Atom() = t.Scripted(t.Base())

        member private t.Base() : MA =
            let position = here()
            match peek() with
            | ValueSome Token.Open -> t.Group()
            | ValueSome(Token.Char c) ->
                advance()
                MA.Char c
            | ValueSome(Token.Command name) ->
                advance()
                t.Named(name, position)
            | ValueSome(Token.Super | Token.Sub) -> fail("a script with nothing before it", position)
            | ValueSome Token.Close -> fail("a closing brace with no opening one", position)
            | ValueSome Token.Cell -> fail("an & outside a table", position)
            | ValueSome Token.Break -> fail("a row break outside a table", position)
            | ValueNone -> fail("the formula ends where an atom was expected", position)

        /// The scripts standing after an atom, which a large operator takes as its limits instead.
        member private t.Scripted(bass: MA) =
            let mutable super = ValueNone
            let mutable sub = ValueNone
            let mutable scripted = true
            while scripted do
                let position = here()
                match peek() with
                | ValueSome Token.Super ->
                    advance()
                    if super.IsSome then fail("two superscripts on one atom", position)
                    super <- ValueSome(t.Argument())
                | ValueSome Token.Sub ->
                    advance()
                    if sub.IsSome then fail("two subscripts on one atom", position)
                    sub <- ValueSome(t.Argument())
                | _ -> scripted <- false
            match bass with
            | MA.BigOp(op, ValueNone, ValueNone) -> MA.BigOp(op, sub, super)
            | _ ->
                match super, sub with
                | ValueNone, ValueNone -> bass
                | ValueSome super, _ -> MA.ScriptSuper(bass, super, sub)
                | ValueNone, ValueSome sub -> MA.ScriptSub(bass, sub)

        /// What a command takes: a group, or the one atom standing where a group would.
        member private t.Argument() =
            match peek() with
            | ValueSome Token.Open -> t.Group()
            | _ -> t.Base()

        member private t.Group() =
            let position = here()
            advance()
            let inner = t.Formula()
            match peek() with
            | ValueSome Token.Close ->
                advance()
                inner
            | _ -> fail("a group is never closed", position)

        /// The characters between braces, taken from the string rather than read as atoms.
        member private t.Words(position: int) =
            match peek() with
            | ValueSome Token.Open ->
                let opening = here()
                advance()
                let mutable depth = 1
                let mutable closing = opening
                while depth > 0 do
                    match peek() with
                    | ValueNone -> fail("a word is never closed", position)
                    | ValueSome Token.Open ->
                        depth <- depth + 1
                        advance()
                    | ValueSome Token.Close ->
                        depth <- depth - 1
                        closing <- here()
                        advance()
                    | _ -> advance()
                source.Substring(opening + 1, closing - opening - 1)
            | _ -> fail("a word in braces was expected", position)

        member private t.Named(name: string, position: int) : MA =
            match name with
            | "frac" | "dfrac" | "tfrac" -> MA.Frac(t.Argument(), t.Argument())
            | "binom" -> MA.Binom(t.Argument(), t.Argument())
            | "sqrt" ->
                match t.Degree() with
                | ValueSome degree -> MA.RootN(degree, t.Argument())
                | ValueNone -> MA.Sqrt(t.Argument())
            | "overline" -> MA.Overline(t.Argument())
            | "underline" -> MA.Underline(t.Argument())
            | "left" -> t.Bracketed(position)
            | "right" -> fail("a right delimiter with no left one", position)
            | "begin" -> t.Environment(position)
            | "end" -> fail("an environment ends where none began", position)
            | "text" | "textrm" -> MA.Text(t.Words position)
            | "mathrm" ->
                let words = t.Words position
                if words = "d" then MA.UprightD else MA.Text words
            | "mathbf" | "boldsymbol" -> bold(t.Argument())
            | "mathbb" -> blackboard(t.Argument())
            | "color" | "textcolor" ->
                let colour = t.Colour position
                MA.Coloured(colour, t.Argument())
            | "{" | "}" | "%" | "#" | "&" | "_" | "$" -> MA.Char name.[0]
            | _ ->
                match commands |> ImmutableDictionary.tryFind name with
                | ValueSome(Standing.Symbol c) -> MA.Char c
                | ValueSome(Standing.Function f) -> MA.Function f
                | ValueSome(Standing.BigOp op) -> MA.BigOp(op, ValueNone, ValueNone)
                | ValueSome(Standing.Space space) -> MA.Space space
                | ValueSome(Standing.Accent accent) -> MA.Accented(accent, t.Argument())
                | ValueSome(Standing.Spanning mark) -> MA.Spanned(mark, t.Argument())
                | ValueNone -> fail($"{name} is no command this reads", position)

        /// The degree a root is taken to, which square brackets hold rather than braces.
        member private t.Degree() =
            match peek() with
            | ValueSome(Token.Char '[') ->
                let position = here()
                advance()
                let elements = ImmutableArray.CreateBuilder<MA>()
                let mutable closed = false
                while not closed do
                    match peek() with
                    | ValueSome(Token.Char ']') ->
                        advance()
                        closed <- true
                    | ValueNone -> fail("a root's degree is never closed", position)
                    | _ -> elements.Add(t.Atom())
                ValueSome(MA.OfElements(elements.ToImmutable()))
            | _ -> ValueNone

        member private t.Colour(position: int) =
            let name = t.Words position
            if name.StartsWith('#') then
                match Int32.TryParse(name.AsSpan 1, Globalization.NumberStyles.HexNumber, null) with
                | true, value when name.Length = 7 ->
                    Color.FromArgb(255, (value >>> 16) &&& 255, (value >>> 8) &&& 255, value &&& 255)
                | _ -> fail($"{name} is no colour", position)
            else
                let named = Color.FromName name
                if named.IsKnownColor then named else fail($"{name} is no colour", position)

        member private t.Bracketed(position: int) =
            let left = t.Delimiter position
            let inner = t.Formula()
            match peek() with
            | ValueSome(Token.Command "right") ->
                advance()
                let right = t.Delimiter(here())
                MA.Bracketed(Brackets(left, right), inner, BracketCompletion.Completed)
            | _ -> fail("a left delimiter with no right one", position)

        member private t.Delimiter(position: int) =
            match peek() |> ValueOption.bind delimiter with
            | ValueSome bracket ->
                advance()
                bracket
            | ValueNone -> fail("a delimiter was expected", position)

        member private t.Environment(position: int) : MA =
            let name = t.Words position
            match name with
            | "cases" -> MA.Cases(t.Cells(name, position))
            | "eqnarray" | "eqnarray*" ->
                MA.Table(
                    t.Cells(name, position),
                    ImmutableArray.Create(Alignment.Right, Alignment.Centre, Alignment.Left))
            | "align" | "align*" ->
                MA.Table(t.Cells(name, position), ImmutableArray.Create(Alignment.Right, Alignment.Left))
            | "array" ->
                let alignments =
                    t.Words position
                    |> Seq.map (fun spelling ->
                        match alignment spelling with
                        | ValueSome alignment -> alignment
                        | ValueNone -> fail($"{spelling} is no column alignment", position))
                MA.Table(t.Cells(name, position), alignments.ToImmutableArray())
            | _ ->
                match matrixBrackets name with
                | ValueSome brackets ->
                    let table = MA.Matrix(t.Cells(name, position))
                    match brackets with
                    | ValueSome bracket -> MA.Paired(bracket, table)
                    | ValueNone -> table
                | ValueNone -> fail($"{name} is no environment this reads", position)

        /// The cells up to the end of the environment, short rows filled out to the widest.
        member private t.Cells(name: string, position: int) =
            let rows = ResizeArray<ResizeArray<MA>>()
            let mutable row = ResizeArray<MA>()
            let mutable ended = false
            while not ended do
                row.Add(t.Formula())
                match peek() with
                | ValueSome Token.Cell -> advance()
                | ValueSome Token.Break ->
                    advance()
                    rows.Add row
                    row <- ResizeArray()
                | ValueSome(Token.Command "end") ->
                    advance()
                    let closing = t.Words position
                    if closing <> name then fail($"{name} is closed by {closing}", position)
                    // A row break may end the last row rather than start an empty one.
                    if rows.Count = 0 || row.Count > 1 || not row.[0].IsEmpty then rows.Add row
                    ended <- true
                | _ -> fail($"{name} is never closed", position)
            let columns = rows |> Seq.map (fun row -> row.Count) |> Seq.max
            rows
            |> Seq.map (fun row -> Seq.append row (Seq.replicate (columns - row.Count) MA.Empty))
            |> ImmA2D.fromJagged

[<AbstractClass; Sealed>]
type Latex =
    /// The formula a math-mode LaTeX string spells, or why it could not be read.
    static member Read(latex: string) : Result<MA, LatexError> =
        try
            let reader = Latexing.Reader(Latexing.lex latex, latex)
            let formula = reader.Formula().Flatten
            match reader.Unread with
            | ValueNone -> Ok formula
            | ValueSome position -> Error(LatexError("the formula ends before the string does", position))
        with Latexing.Rejected(message, position) -> Error(LatexError(message, position))
