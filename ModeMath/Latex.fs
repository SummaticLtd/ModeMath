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
    override _.ToString() = $"{message}, at character {position.ToString()}"

/// The colours \color and \textcolor name, which a caller may give its own.
[<Sealed>]
type Palette(colours: ImmutableDictionary<string, Color>) =
    /// LaTeX names a colour without regard to case, so darkGray and darkgray are the one name.
    let named =
        ImmutableDictionary.CreateRange(
            StringComparer.OrdinalIgnoreCase,
            colours |> Seq.map (fun c -> KeyValuePair(c.Key, Color.FromArgb(c.Value.ToArgb()))))

    /// The nineteen colours xcolor names, in the shades .NET gives them.
    static member val Default =
        [
            "red", Color.Red; "green", Color.Green; "blue", Color.Blue
            "cyan", Color.Cyan; "magenta", Color.Magenta; "yellow", Color.Yellow
            "black", Color.Black; "white", Color.White
            "gray", Color.Gray; "darkgray", Color.DarkGray; "lightgray", Color.LightGray
            "brown", Color.Brown; "lime", Color.Lime; "olive", Color.Olive
            "orange", Color.Orange; "pink", Color.Pink; "purple", Color.Purple
            "teal", Color.Teal; "violet", Color.Violet
        ]
        |> Seq.map (fun (name, colour) -> KeyValuePair(name, colour))
        |> ImmutableDictionary.CreateRange
        |> Palette

    /// The colour a name stands for. ValueNone where this palette does not name one.
    member _.Named(name: string) : Color voption = named |> ImmutableDictionary.tryFind name

    /// The same palette naming one more colour, which takes the place of any name already given.
    member _.With(name: string, colour: Color) = Palette(named.SetItem(name, colour))

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

    /// Where Unicode's italic alphabets begin, against the letters a formula sets in italic anyway.
    let private italicised = [ 0x1D434, 'A', 26; 0x1D44E, 'a', 26; 0x1D6E2, 'Α', 25; 0x1D6FC, 'α', 25 ]

    /// The shapes Unicode keeps out of those alphabets, the rule a spreadsheet draws a bar with,
    /// and the apostrophe a derivative is written with.
    let private apart = [ 0x210E, 'h'; 0x1D6F3, 'Θ'; 0x2502, '|'; 0x27, '′' ]

    /// The character a character stands for, where it is not one a formula holds as it is.
    let standsFor(codepoint: int) =
        match apart |> List.tryFind (fun (shape, _) -> shape = codepoint) with
        | Some(_, letter) -> ValueSome letter
        | None ->
            italicised
            |> List.tryPick (fun (first, letter, count) ->
                if codepoint >= first && codepoint < first + count then
                    Some(char (int letter + codepoint - first))
                else None)
            |> ValueOption.ofOption

    /// The characters LaTeX gives a meaning of its own, which a formula holding one escapes.
    let private kept = Set.ofSeq "{}%#&_$^"

    /// A mark that gives ink of its own, which a zero-width space does not.
    let private inked(c: char) = Char.GetUnicodeCategory c <> Globalization.UnicodeCategory.Format

    /// The tokens a string is made of, paired with where each begins.
    let lex(latex: string) =
        let tokens = ImmutableArray.CreateBuilder<struct(Token * int)>()
        let mutable i = 0
        while i < latex.Length do
            let start = i
            let take(token: Token) = tokens.Add(struct(token, start))
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
            | _ ->
                if Char.IsHighSurrogate c && i < latex.Length && Char.IsLowSurrogate latex.[i] then
                    let codepoint = Char.ConvertToUtf32(c, latex.[i])
                    i <- i + 1
                    match standsFor codepoint with
                    | ValueSome letter -> take(Token.Char letter)
                    | ValueNone ->
                        let hex = codepoint.ToString "X4"
                        fail($"U+{hex} is no character this draws", start)
                elif Char.IsWhiteSpace c || not(inked c) then ()
                else
                    match standsFor(int c) with
                    | ValueSome letter -> take(Token.Char letter)
                    // What the font cannot draw is refused here, so laying a formula out cannot fail.
                    | ValueNone when (Glyphs.variable c).IsNone ->
                        fail($"{c.ToString()} is no character this draws", start)
                    | ValueNone -> take(Token.Char c)
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
            "therefore", '∴'; "because", '∵'; "mid", '∣'; "prime", '′'
            "cdots", '⋯'; "ldots", '…'; "dots", '…'; "vdots", '⋮'; "ddots", '⋱'
            "times", '×'; "div", '÷'; "cdot", '⋅'; "pm", '±'; "mp", '∓'; "ast", '∗'
            "cap", '∩'; "cup", '∪'; "wedge", '∧'; "land", '∧'; "vee", '∨'; "lor", '∨'
            "setminus", '∖'; "oplus", '⊕'; "otimes", '⊗'
            "leq", '≤'; "le", '≤'; "geq", '≥'; "ge", '≥'; "neq", '≠'; "ne", '≠'
            "approx", '≈'; "equiv", '≡'; "sim", '∼'; "cong", '≅'; "propto", '∝'
            "in", '∈'; "notin", '∉'; "subset", '⊂'; "subseteq", '⊆'
            "to", '→'; "rightarrow", '→'; "Rightarrow", '⇒'; "Leftrightarrow", '⇔'; "iff", '⟺'
            "neg", '¬'; "lnot", '¬'; "langle", '⟨'; "rangle", '⟩'
            "lfloor", '⌊'; "rfloor", '⌋'; "lceil", '⌈'; "rceil", '⌉'; "diameter", '⌀'
            "leftarrow", '←'; "gets", '←'; "Leftarrow", '⇐'; "leftrightarrow", '↔'; "mapsto", '↦'
            "Longleftrightarrow", '⟺'; "supset", '⊃'; "supseteq", '⊇'; "perp", '⊥'; "parallel", '∥'
            "angle", '∠'; "ell", 'ℓ'; "nabla", '∇'; "forall", '∀'; "exists", '∃'
            "dagger", '†'; "degree", '°'; "backslash", '\\'
            "circ", '∘'; "triangle", '△'; "square", '□'; "pounds", '£'
            "bullet", '•'; "circlearrowright", '↻'
            "uparrow", '↑'; "downarrow", '↓'; "longrightarrow", '⟶'; "longleftarrow", '⟵'
            "nearrow", '↗'; "searrow", '↘'
        ]

    /// Every function under the name it is called by, with the spellings also written for a few.
    /// ISO 80000-2 names the inverse hyperbolics for the area they take, not an arc.
    let private functions =
        [
            for struct(name, f) in MathFunctions.named do yield name, f
            yield "arcsinh", MathFunction.Arsinh
            yield "arccosh", MathFunction.Arcosh
            yield "arctanh", MathFunction.Artanh
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

    /// Words standing side by side made one, so an alphabet sets \mathrm{sech} as a word not four letters.
    let private joined(elements: ImmutableArray<MA>) =
        let runs = ImmutableArray.CreateBuilder<MA>()
        for element in elements do
            match element, (if runs.Count = 0 then MA.Empty else runs.[runs.Count - 1]) with
            | MA.Text word, MA.Text before -> runs.[runs.Count - 1] <- MA.Text(before + word)
            | _ -> runs.Add element
        runs.ToImmutable()

    /// A formula with its letters set in another alphabet, and whatever that alphabet lacks left alone.
    let rec private set(build: char -> MA voption, ma: MA) =
        let inner(x: MA) = set(build, x)
        let optional = ValueOption.map inner
        match ma with
        | MA.Char c -> build c |> ValueOption.defaultValue ma
        | MA.Row elements -> MA.Row(joined(elements |> ImmArray.map inner))
        | MA.ScriptSuper(main, super, sub) -> MA.ScriptSuper(inner main, inner super, optional sub)
        | MA.ScriptSub(main, sub) -> MA.ScriptSub(inner main, inner sub)
        | MA.Frac(n, d) -> MA.Frac(inner n, inner d)
        | MA.Stack(top, bottom) -> MA.Stack(inner top, inner bottom)
        | MA.Bracketed(brackets, x, completion) -> MA.Bracketed(brackets, inner x, completion)
        | MA.RootN(n, x) -> MA.RootN(inner n, inner x)
        | MA.Sqrt x -> MA.Sqrt(inner x)
        | MA.BigOp(op, lower, upper) -> MA.BigOp(op, optional lower, optional upper)
        | MA.Accented(accent, x) -> MA.Accented(accent, inner x)
        | MA.Spanned(mark, x) -> MA.Spanned(mark, inner x)
        | MA.Overline x -> MA.Overline(inner x)
        | MA.Underline x -> MA.Underline(inner x)
        | MA.Coloured(colour, x) -> MA.Coloured(colour, inner x)
        | MA.Table(cells, alignments, separated) ->
            MA.Table(cells |> ImmA2D.map inner, alignments, separated)
        | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD | MA.Function _ | MA.Text _ | MA.Space _ -> ma

    /// A formula set upright, as \mathrm does, leaving alone the figures and signs that stand upright anyway.
    let upright(ma: MA, position: int) =
        let letter(c: char) =
            if (Glyphs.upright c).IsNone then fail($"{c.ToString()} is no character this sets upright", position)
            if (Letters.upright c).IsSome then ValueSome(MA.Text(string c)) else ValueNone
        set(letter, ma)

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
        | Token.Command("lfloor" | "rfloor") -> ValueSome Bracket.Floor
        | Token.Command("lceil" | "rceil") -> ValueSome Bracket.Ceiling
        | Token.Command("lvert" | "rvert" | "vert" | "mid") -> ValueSome Bracket.Line
        // Only \left and \right read a delimiter, so a slash anywhere else stays an ordinary character.
        | Token.Char '/' -> ValueSome Bracket.Slash
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

    /// The columns an alignment is set in, which are the ones an aligned environment spells.
    let alignColumns = ImmutableArray.Create(Alignment.Right, Alignment.Left)

    let alignment(spelling: char) =
        match spelling with
        | 'l' -> ValueSome Alignment.Left
        | 'c' -> ValueSome Alignment.Centre
        | 'r' -> ValueSome Alignment.Right
        | _ -> ValueNone

    type Reader(tokens: ImmutableArray<struct(Token * int)>, source: string, palette: Palette) =
        let mutable at = 0

        let here() =
            if at < tokens.Length then
                let struct(_, position) = tokens.[at]
                position
            else source.Length

        let peek() =
            if at < tokens.Length then
                let struct(token, _) = tokens.[at]
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
        member t.Formula() = t.Formula false

        /// The same, under a \choose that has already taken what stands before it.
        member private t.Formula(chosenAlready: bool) =
            let elements = ImmutableArray.CreateBuilder<MA>()
            let mutable chosen = ValueNone
            while chosen.IsNone && not t.Ends do
                let position = here()
                match peek() with
                // The one infix command: what it stands between is what it sets over and under.
                | ValueSome(Token.Command("choose" | "atop" as name)) ->
                    if chosenAlready then fail($"a second \\{name} in one group", position)
                    advance()
                    chosen <- ValueSome(struct(name, MA.OfElements(elements.ToImmutable())))
                | _ -> elements.Add(t.Atom())
            match chosen with
            | ValueSome(struct("atop", top)) -> MA.Stack(top, t.Formula true)
            | ValueSome(struct(_, top)) -> MA.Binom(top, t.Formula true)
            | ValueNone -> MA.OfElements(elements.ToImmutable())

        member private t.Atom() =
            match peek() with
            // A script with nothing before it stands on an empty base, as {}^{14}C is written.
            | ValueSome(Token.Super | Token.Sub) -> t.Scripted MA.Empty
            | _ -> t.Scripted(t.Base())

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
            | ValueSome(Token.Super | Token.Sub) -> fail("a script where an atom was expected", position)
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
                let word = source.Substring(opening + 1, closing - opening - 1)
                // What LaTeX keeps for itself stands under a backslash here as it does anywhere.
                let plain = Text.StringBuilder()
                let mutable i = 0
                while i < word.Length do
                    if word.[i] = '\\' && i + 1 < word.Length && kept.Contains word.[i + 1] then i <- i + 1
                    plain.Append word.[i] |> ignore
                    i <- i + 1
                plain.ToString()
            | _ -> fail("a word in braces was expected", position)

        /// Words set upright, which the italic shapes of a formula are no substitute for.
        member private _.Upright(word: string, position: int) =
            let word = word |> String.filter inked
            for c in word do
                if (Glyphs.upright c).IsNone then fail($"{c.ToString()} is no character this sets upright", position)
            word

        member private t.Written(word: string, position: int) = MA.Text(t.Upright(word, position))

        member private t.Named(name: string, position: int) : MA =
            match name with
            | "operatorname" ->
                let name = t.Words position
                match functions |> List.tryFind (fun (spelling, _) -> spelling = name) with
                | Some(_, f) -> MA.Function f
                | None -> fail($"{name} is no function this knows", position)
            | "frac" | "dfrac" | "tfrac" -> MA.Frac(t.Argument(), t.Argument())
            | "binom" -> MA.Binom(t.Argument(), t.Argument())
            | "sqrt" ->
                match t.Degree() with
                | ValueSome degree -> MA.RootN(degree, t.Argument())
                | ValueNone -> MA.Sqrt(t.Argument())
            | "overline" | "overbar" -> MA.Overline(t.Argument())
            | "underline" -> MA.Underline(t.Argument())
            | "left" -> t.Bracketed(position)
            | "right" -> fail("a right delimiter with no left one", position)
            | "begin" -> t.Environment(position)
            | "end" -> fail("an environment ends where none began", position)
            | "text" | "textrm" -> t.Written(t.Words position, position)
            // \mathrm sets mathematics upright, so scripts, gaps and commands all read inside it.
            | "mathrm" ->
                match t.Argument() with
                | MA.Char 'd' -> MA.UprightD
                | argument -> upright(argument, position)
            | "mathbf" | "boldsymbol" | "bf" -> bold(t.Argument())
            | "mathbb" -> blackboard(t.Argument())
            | "color" | "textcolor" ->
                let colour = t.Colour position
                MA.Coloured(colour, t.Argument())
            | "{" | "}" | "%" | "#" | "&" | "_" | "$" | "^" -> MA.Char name.[0]
            | _ ->
                match commands |> ImmutableDictionary.tryFind name with
                | ValueNone -> fail($"{name} is no command this reads", position)
                | ValueSome standing ->
                    match standing with
                    | Standing.Symbol c -> MA.Char c
                    | Standing.Function f -> MA.Function f
                    | Standing.BigOp op -> MA.BigOp(op, ValueNone, ValueNone)
                    | Standing.Space space -> MA.Space space
                    | Standing.Accent accent -> MA.Accented(accent, t.Argument())
                    | Standing.Spanning mark -> MA.Spanned(mark, t.Argument())

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
            let error() = fail($"{name} is no colour", position)
            if name.StartsWith('#') then
                let byteAt(value: uint32, shift: int) = int ((value >>> shift) &&& 255u)
                match UInt32.TryParse(name.AsSpan 1, Globalization.NumberStyles.AllowHexSpecifier, null) with
                | true, value ->
                    if name.Length = 7 then
                        Color.FromArgb(255, byteAt(value, 16), byteAt(value, 8), byteAt(value, 0))
                    elif name.Length = 9 then
                        Color.FromArgb(byteAt(value, 0), byteAt(value, 24), byteAt(value, 16), byteAt(value, 8))
                    else error()
                | false, _ -> error()
            else
                match palette.Named name with
                | ValueSome colour -> colour
                | ValueNone -> error()

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
                    ImmutableArray.Create(Alignment.Right, Alignment.Centre, Alignment.Left),
                    true)
            | "align" | "align*" | "aligned" -> MA.Table(t.Cells(name, position), alignColumns, false)
            | "array" ->
                let alignments =
                    t.Words position
                    |> Seq.map (fun spelling ->
                        match alignment spelling with
                        | ValueSome alignment -> alignment
                        | ValueNone -> fail($"{spelling} is no column alignment", position))
                MA.Table(t.Cells(name, position), alignments.ToImmutableArray(), true)
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

    /// The first spelling of a value, which is the one it is written back out with.
    let private spelt(table: (string * 'a) list, value: 'a) =
        table |> List.pick (fun (name, x) -> if x = value then Some name else None)

    /// Every character a command stands for, greek first, so each is written the one way.
    let private namedChars = greek @ marks

    let private delimiterSpelling(bracket: Bracket, opening: bool) =
        match bracket, opening with
        | Bracket.Normal, true -> "("
        | Bracket.Normal, false -> ")"
        | Bracket.Square, true -> "["
        | Bracket.Square, false -> "]"
        | Bracket.Curly, true -> "\\{"
        | Bracket.Curly, false -> "\\}"
        | Bracket.Angle, true -> "\\langle"
        | Bracket.Angle, false -> "\\rangle"
        | Bracket.Floor, true -> "\\lfloor"
        | Bracket.Floor, false -> "\\rfloor"
        | Bracket.Ceiling, true -> "\\lceil"
        | Bracket.Ceiling, false -> "\\rceil"
        | Bracket.Line, _ -> "|"
        | Bracket.Slash, _ -> "/"
        // Nothing at all, as \left. leaves a side of a formula open.
        | Bracket.None, _ -> "."

    let private alignmentSpelling(alignment: Alignment) =
        match alignment with
        | Alignment.Left -> "l"
        | Alignment.Right -> "r"
        | Alignment.Centre | _ -> "c"

    /// A formula as the LaTeX that reads back as it, every argument in braces.
    let write(ma: MA) =
        let text = Text.StringBuilder()
        /// A control word runs on into a letter after it, so a space is put between them.
        let mutable word = false
        let put(s: string) =
            if word && s.Length > 0 && Char.IsAsciiLetter s.[0] then text.Append ' ' |> ignore
            text.Append s |> ignore
            word <- false
        let command(name: string) =
            put "\\"
            put name
            word <- name.Length > 0 && Char.IsAsciiLetter name.[name.Length - 1]
        /// A delimiter named by a control word is written as one, so a letter after it does not run on.
        let delimiter(bracket: Bracket, opening: bool) =
            let spelling = delimiterSpelling(bracket, opening)
            if spelling.StartsWith '\\' then command(spelling.Substring 1) else put spelling
        let rec formula(ma: MA) =
            match ma with
            | MA.Row elements -> for element in elements do formula element
            | MA.Char c ->
                match namedChars |> List.tryPick (fun (name, x) -> if x = c then Some name else None) with
                | Some name -> command name
                // What LaTeX keeps for itself stands for itself only under a backslash.
                | None when kept.Contains c -> command(string c)
                | None -> put(string c)
            | MA.BoldVar c ->
                command "mathbf"
                braced(MA.Char c)
            | MA.Blackboard c ->
                command "mathbb"
                braced(MA.Char c)
            | MA.UprightD ->
                command "mathrm"
                put "{d}"
            | MA.Function f -> command(MathFunctions.name f)
            | MA.ScriptSuper(main, super, sub) ->
                atom main
                sub |> ValueOption.iter (fun sub -> put "_"; braced sub)
                put "^"
                braced super
            | MA.ScriptSub(main, sub) ->
                atom main
                put "_"
                braced sub
            | MA.Frac(numerator, denominator) ->
                command "frac"
                braced numerator
                braced denominator
            | MA.Stack(top, bottom) ->
                put "{"
                formula top
                command "atop"
                formula bottom
                put "}"
            | MA.Bracketed(brackets, inner, _) ->
                command "left"
                delimiter(brackets.Left, true)
                formula inner
                command "right"
                delimiter(brackets.Right, false)
            | MA.RootN(degree, radicand) ->
                command "sqrt"
                put "["
                braced degree
                put "]"
                braced radicand
            | MA.Sqrt radicand ->
                command "sqrt"
                braced radicand
            | MA.BigOp(op, lower, upper) ->
                command(spelt(bigOps, op))
                lower |> ValueOption.iter (fun lower -> put "_"; braced lower)
                upper |> ValueOption.iter (fun upper -> put "^"; braced upper)
            | MA.Accented(accent, x) ->
                command(spelt(accents, accent))
                braced x
            | MA.Spanned(mark, x) ->
                command(spelt(spanning, mark))
                braced x
            | MA.Overline x ->
                command "overline"
                braced x
            | MA.Underline x ->
                command "underline"
                braced x
            | MA.Coloured(colour, x) ->
                command "color"
                put "{"
                let hex(part: byte) = part.ToString "X2"
                let rgb = $"#{hex colour.R}{hex colour.G}{hex colour.B}"
                // Never the name .NET knows a colour by, which a palette may have given to another.
                put(if colour.A = 255uy then rgb else rgb + hex colour.A)
                put "}"
                braced x
            | MA.Text word ->
                command "text"
                put "{"
                for c in word do
                    if kept.Contains c then put "\\"
                    put(string c)
                put "}"
            | MA.Space space -> command(spelt(spaces, space))
            | MA.Table(cells, alignments, separated) ->
                // Only aligned spells an unseparated grid; any other reads back as a separated array.
                // It is aligned rather than align because what is written is always math mode.
                let name =
                    if alignments.IsEmpty then "matrix"
                    elif not separated && alignments.AsSpan().SequenceEqual(alignColumns.AsSpan()) then
                        "aligned"
                    else "array"
                command "begin"
                put "{"
                put name
                put "}"
                if name = "array" then
                    put "{"
                    for alignment in alignments do put(alignmentSpelling alignment)
                    put "}"
                for row in 0 .. cells.Rows - 1 do
                    if row > 0 then put "\\\\"
                    for col in 0 .. cells.Cols - 1 do
                        if col > 0 then put "&"
                        formula cells.[row, col]
                command "end"
                put "{"
                put name
                put "}"

        and braced(ma: MA) =
            put "{"
            formula ma
            put "}"

        /// What a script goes on, which needs braces where it is more than the one atom before it.
        and atom(ma: MA) =
            match ma with
            | MA.Row elements when elements.Length = 1 -> atom elements.[0]
            | MA.Row _ | MA.ScriptSuper _ | MA.ScriptSub _ | MA.Space _ -> braced ma
            | _ -> formula ma

        formula ma
        text.ToString()

[<AbstractClass; Sealed>]
type Latex =
    /// The formula a string spells, or why not. \color names its colours from the palette given.
    static member Read(latex: string, ?palette: Palette) : Result<MA, LatexError> =
        try
            let palette = defaultArg palette Palette.Default
            let reader = Latexing.Reader(Latexing.lex latex, latex, palette)
            let formula = reader.Formula().Flatten
            match reader.Unread with
            | ValueNone -> Ok formula
            | ValueSome position -> Error(LatexError("the formula ends before the string does", position))
        with Latexing.Rejected(message, position) -> Error(LatexError(message, position))

    /// The math-mode LaTeX a formula is written as, which reads back as a formula that draws the same.
    static member Write(formula: MA) : string = Latexing.write formula
