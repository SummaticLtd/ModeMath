module ModeMath.Tests.LatexTests

open System.Collections.Immutable
open System.Drawing
open FSUtils
open SimpleTests
open ModeMath

let private layout = Layout 20f<px>
let private c(character: char) = MA.Char character
let private row(elements: MA list) = MA.Row(elements.ToImmutableArray())
let private grid(cells: MA list list) = ImmA2D.fromJagged cells
let private cells = grid [ [ c 'a'; c 'b' ]; [ c 'c'; c 'd' ] ]

let private read(latex: string) =
    match Latex.Read latex with
    | Ok ma -> ma
    | Error error -> failwith $"{latex}: {error}"

/// A case for every item, named by what it stands for.
let private named(items: 'a list) = items |> List.map (fun item -> string item, item)

/// A case for every string read, named by the LaTeX it stands for.
let private same(name: string, pairs: (string * MA) list) =
    Test.CasesSync(
        name,
        pairs |> List.map (fun (latex, expected) -> latex, (latex, expected)),
        fun (latex, expected) -> Assert.Equal(expected, read latex, latex))

/// The same for the strings that are refused rather than read.
let private rejected(name: string, strings: string list) =
    Test.CasesSync(
        name,
        named strings,
        fun latex ->
            match Latex.Read latex with
            | Ok ma -> Assert.Fail $"{latex} was read as {ma}"
            | Error _ -> ())

/// A case for every formula written out, named by the formula itself.
let private writes(name: string, pairs: (MA * string) list) =
    Test.CasesSync(
        name,
        pairs |> List.map (fun (formula, expected) -> string formula, (formula, expected)),
        fun (formula, expected) -> Assert.Equal(expected, Latex.Write formula, string formula))

/// Every character a command stands for, under the command's name.
let private namedSymbols =
    [
        for command in Latexing.commands do
            match command.Value with
            | Latexing.Standing.Symbol character -> yield command.Key, character
            | _ -> ()
    ]

let private reading =
    TestList(
        "Reading LaTeX",
        [   same(
                "charactersStandForThemselvesAndSpacesForNothing",
                [
                    "ab", MA.String "ab"
                    "a b", MA.String "ab"
                    "a+b", row [ c 'a'; c '+'; c 'b' ]
                    "{ab}c", MA.String "abc"
                    "{a}", c 'a'
                    "", MA.Empty
                ]
            )
            same(
                "aNamedSymbolStandsForItsCharacter",
                [
                    "\\alpha\\beta", row [ c 'α'; c 'β' ]
                    "\\alpha x", row [ c 'α'; c 'x' ]
                    "\\infty", c '∞'
                    "\\cdot", c '⋅'
                    "\\leq", c '≤'
                    // The divides sign, which spaces as the relation it is where a plain bar would not.
                    "a\\mid b", row [ c 'a'; c '∣'; c 'b' ]
                    "\\circ", c '∘'
                    "\\triangle", c '△'
                    "\\uparrow\\downarrow", row [ c '↑'; c '↓' ]
                    "\\longrightarrow", c '⟶'
                    "\\pounds", c '£'
                    "\\diameter", c '⌀'
                    "a\\ll b\\gg c", row [ c 'a'; c '≪'; c 'b'; c '≫'; c 'c' ]
                    "\\dagger\\ddagger", row [ c '†'; c '‡' ]
                    // A second name for a shape is the same shape, whichever of them a formula writes.
                    "\\varnothing", c '∅'
                    "\\smallsetminus", c '∖'
                    "a\\colon b", row [ c 'a'; c ':'; c 'b' ]
                    // A word processor writes its variables in the italic alphabet Unicode holds.
                    "𝑠𝑜𝑐", MA.String "soc"
                    "𝐴𝜋", row [ c 'A'; c 'π' ]
                    // Unicode keeps italic h among the letterlike symbols and leaves its slot empty,
                    // and fills the empty slot of the italic capitals with the theta symbol.
                    "ℎ", c 'h'
                    "𝛳", c 'Θ'
                    // A mark that gives no ink of its own is nothing to draw, in text as anywhere else.
                    "a​b", MA.String "ab"
                    "\\text{a​b}", MA.Text "ab"
                    // A spreadsheet writes the bar of a conditional probability as a drawing rule.
                    "P(A│B)", read "P(A|B)"
                    // An apostrophe is the prime a derivative is written with, which slants.
                    "f'(x)", read "f\\prime(x)"
                    "f''", row [ c 'f'; c '′'; c '′' ]
                    // Only the 26 letters spell a command, so \cosθ is a function and a letter.
                    "\\cosθ", row [ MA.Function MathFunction.Cos; c 'θ' ]
                ]
            )
            same(
                "anInfixCommandStandsBetweenWhatItSetsOverAndUnder",
                [
                    // An infix command takes what is on either side of it rather than after.
                    "{8 \\choose 6}", MA.Binom(c '8', c '6')
                    "a \\over b", MA.Frac(c 'a', c 'b')
                    "{a+1 \\over 2}c", row [ MA.Frac(row [ c 'a'; c '+'; c '1' ], c '2'); c 'c' ]
                    "a \\atop b", MA.Stack(c 'a', c 'b')
                    "n \\choose k", MA.Binom(c 'n', c 'k')
                    "{n+1 \\choose 2}", MA.Binom(row [ c 'n'; c '+'; c '1' ], c '2')
                    // What stands outside the braces is no part of it.
                    "{2 \\choose n+1}", MA.Binom(c '2', row [ c 'n'; c '+'; c '1' ])
                    "P{4 \\choose 2}", row [ c 'P'; MA.Binom(c '4', c '2') ]
                    // A group of its own is a scope of its own, so the second choose is unambiguous.
                    "{n \\choose {a \\choose b}}", MA.Binom(c 'n', MA.Binom(c 'a', c 'b'))
                ]
            )
            same(
                "aHouseMacroIsReadAsWhatItStandsFor",
                [
                    "\\overbar{Y}", MA.Overline(c 'Y')
                    "\\bf{F}", MA.BoldVar 'F'
                    "\\bullet", c '•'
                ]
            )
            same(
                "notStrikesThroughTheRelationAfterIt",
                [
                    "a \\not= b", row [ c 'a'; c '≠'; c 'b' ]
                    "\\not\\equiv", c '≢'
                    "\\not\\in", c '∉'
                    "\\not\\ni", c '∌'
                    "\\not\\subseteq", c '⊈'
                    "\\not\\mid", c '∤'
                    "\\not<", c '≮'
                    "\\not\\Rightarrow", c '⇏'
                    "\\not\\exists", c '∄'
                    "\\not{=}", c '≠'
                    "≢", c '≢'
                    // The amssymb names stand for the same characters.
                    "\\nmid", c '∤'
                    "\\nsubseteq", c '⊈'
                    "\\nLeftrightarrow", c '⇎'
                ]
            )
            same(
                "aScriptGoesOnTheAtomBeforeIt",
                [
                    "x^2", MA.ScriptSuper(c 'x', c '2', ValueNone)
                    "x_i", MA.ScriptSub(c 'x', c 'i')
                    "x^2_i", MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i'))
                    "x_i^2", MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i'))
                    "x^{10}", MA.ScriptSuper(c 'x', MA.String "10", ValueNone)
                    "ab^2", row [ c 'a'; MA.ScriptSuper(c 'b', c '2', ValueNone) ]
                    // A script with nothing before it stands on an empty base, which draws nothing.
                    "{}^{14}C", row [ MA.ScriptSuper(MA.Empty, MA.String "14", ValueNone); c 'C' ]
                    "^2", MA.ScriptSuper(MA.Empty, c '2', ValueNone)
                ]
            )
            same(
                "aLargeOperatorTakesItsScriptsAsLimits",
                [
                    "\\sum_{i=0}^n",
                    MA.BigOp(BigOperator.Sum, ValueSome(row [ c 'i'; c '='; c '0' ]), ValueSome(c 'n'))
                    "\\int_0^\\infty", MA.BigOp(BigOperator.Integral, ValueSome(c '0'), ValueSome(c '∞'))
                    "\\int", MA.BigOp(BigOperator.Integral, ValueNone, ValueNone)
                ]
            )
            same(
                "aCommandTakesTheGroupOrTheOneAtomAfterIt",
                [
                    "\\frac{a}{b}", MA.Frac(c 'a', c 'b')
                    "\\frac ab", MA.Frac(c 'a', c 'b')
                    "\\frac{a+1}{b}", MA.Frac(row [ c 'a'; c '+'; c '1' ], c 'b')
                    "\\sqrt{x}", MA.Sqrt(c 'x')
                    "\\sqrt[3]{x}", MA.RootN(c '3', c 'x')
                    "\\sqrt[n+1]{x}", MA.RootN(row [ c 'n'; c '+'; c '1' ], c 'x')
                    "\\binom{n}{k}", MA.Binom(c 'n', c 'k')
                    "\\overline{AB}", MA.Overline(MA.String "AB")
                    "\\underline x", MA.Underline(c 'x')
                    "\\hat{x}", MA.Accented(Accent.Hat, c 'x')
                    "\\overbrace{x+1}", MA.Spanned(Spanning.Overbrace, row [ c 'x'; c '+'; c '1' ])
                ]
            )
            same(
                "aFunctionNameIsOneAtomAndTakesNoArgument",
                [
                    "\\sin x", row [ MA.Function MathFunction.Sin; c 'x' ]
                    "\\sin^2 x", row [ MA.ScriptSuper(MA.Function MathFunction.Sin, c '2', ValueNone); c 'x' ]
                    "\\arcsin", MA.Function MathFunction.Asin
                    "\\arg", MA.Function MathFunction.Arg
                    "\\det", MA.Function MathFunction.Det
                    // \operatorname names a function the same way a command of its own does.
                    "\\operatorname{Im}", MA.Function MathFunction.Imaginary
                    "\\operatorname{arcosh}x", row [ MA.Function MathFunction.Arcosh; c 'x' ]
                    // A name outside the set is the upright words it spells, which is what it draws as.
                    "\\operatorname{Aut}(V)", row [ MA.Text "Aut"; c '('; c 'V'; c ')' ]
                    // ISO 80000-2 names the inverse hyperbolics for the area they take, not an arc.
                    "\\arccosh", MA.Function MathFunction.Arcosh
                    "\\arcsinh", MA.Function MathFunction.Arsinh
                    "\\arctanh", MA.Function MathFunction.Artanh
                    "\\artanh", MA.Function MathFunction.Artanh
                ]
            )
            same(
                "onlyAMarkedPairBracketsAFormula",
                [
                    "\\left(\\frac{1}{2}\\right)", MA.RoundBracket(MA.Frac(c '1', c '2'))
                    "\\left[0,1\\right)",
                    MA.Bracketed(
                        Brackets(Bracket.Square, Bracket.Normal),
                        row [ c '0'; c ','; c '1' ],
                        BracketCompletion.Completed)
                    "\\left.x\\right|", MA.Bracketed(Brackets(Bracket.None, Bracket.Line), c 'x', BracketCompletion.Completed)
                    // A bar names the same delimiter either side, whichever of its names is written.
                    "\\left\\mid x\\right\\vert",
                    MA.Bracketed(Brackets.Matching Bracket.Line, c 'x', BracketCompletion.Completed)
                    "\\left\\lfloor x\\right\\rfloor", MA.Paired(Bracket.Floor, c 'x')
                    "\\left\\lceil x\\right\\rceil", MA.Paired(Bracket.Ceiling, c 'x')
                    "\\left.x\\right/",
                    MA.Bracketed(Brackets(Bracket.None, Bracket.Slash), c 'x', BracketCompletion.Completed)
                    // Plain brackets do not grow in LaTeX, so they are read as the characters they are.
                    "(x)", row [ c '('; c 'x'; c ')' ]
                    "\\lfloor x\\rfloor", row [ c '⌊'; c 'x'; c '⌋' ]
                    "\\lceil x\\rceil", row [ c '⌈'; c 'x'; c '⌉' ]
                    "\\lbrace x\\rbrace", row [ c '{'; c 'x'; c '}' ]
                    "\\lbrack x\\rbrack", row [ c '['; c 'x'; c ']' ]
                    "\\lvert x\\rvert", row [ c '|'; c 'x'; c '|' ]
                    "a\\vert b", row [ c 'a'; c '|'; c 'b' ]
                    // Only \left and \right read a delimiter, so a slash elsewhere divides on the line.
                    "a/b", row [ c 'a'; c '/'; c 'b' ]
                ]
            )
            same(
                "wordsInBracesKeepTheirSpacesAndTheirCase",
                [
                    "\\text{if } x", row [ MA.Text "if "; c 'x' ]
                    "\\mbox{if}", MA.Text "if"
                    "\\mathrm{d}x", row [ MA.UprightD; c 'x' ]
                    "\\mathrm{sech}", MA.Text "sech"
                    "\\mathbf{v}", MA.BoldVar 'v'
                    "\\mathbb{R}", MA.Blackboard 'R'
                    "\\mathbf{ij}", row [ MA.BoldVar 'i'; MA.BoldVar 'j' ]
                    "\\mathbf{v_i}", MA.ScriptSub(MA.BoldVar 'v', MA.BoldVar 'i')
                    "\\mathbf{\\frac{a}{b}}", MA.Frac(MA.BoldVar 'a', MA.BoldVar 'b')
                    // The alphabets hold no bold figures or Greek, so those are left as they are.
                    "\\mathbf{2θ}", row [ c '2'; c 'θ' ]
                ]
            )
            same(
                "mathrmSetsAFormulaUprightRatherThanReadingItAsText",
                [
                    "\\mathrm{x^2}", MA.ScriptSuper(MA.Text "x", c '2', ValueNone)
                    "\\mathrm{a\\,b}", row [ MA.Text "a"; MA.Space Space.Thin; MA.Text "b" ]
                    // The SI way to write a compound unit, which is what \mathrm is mostly asked for.
                    "\\mathrm{m\\,s^{-1}}",
                        row [
                            MA.Text "m"
                            MA.Space Space.Thin
                            MA.ScriptSuper(MA.Text "s", row [ c '-'; c '1' ], ValueNone)
                        ]
                    "\\mathrm{\\frac{a}{b}}", MA.Frac(MA.Text "a", MA.Text "b")
                    // Figures stand upright already, so only the letters beside them are set again.
                    "\\mathrm{2θ}", row [ c '2'; MA.Text "θ" ]
                    "\\mathrm x", MA.Text "x"
                ]
            )
            same(
                "aTableIsReadFromItsCellsAndItsRowBreaks",
                [
                    "\\begin{pmatrix}a&b\\\\c&d\\end{pmatrix}",
                    MA.RoundBracket(MA.Matrix(grid [ [ c 'a'; c 'b' ]; [ c 'c'; c 'd' ] ]))
                    "\\begin{matrix}a\\end{matrix}", MA.Matrix(grid [ [ c 'a' ] ])
                    // A row break may end the last row rather than start an empty one.
                    "\\begin{matrix}a\\\\b\\\\\\end{matrix}", MA.Matrix(grid [ [ c 'a' ]; [ c 'b' ] ])
                    "\\begin{matrix}a&b\\\\c\\end{matrix}",
                    MA.Matrix(grid [ [ c 'a'; c 'b' ]; [ c 'c'; MA.Empty ] ])
                    "\\begin{cases}x&y\\end{cases}", MA.Cases(grid [ [ c 'x'; c 'y' ] ])
                    "\\begin{eqnarray}x&=&y\\end{eqnarray}",
                    MA.Table(
                        grid [ [ c 'x'; c '='; c 'y' ] ],
                        ImmutableArray.Create(Alignment.Right, Alignment.Centre, Alignment.Left))
                    "\\begin{array}{lr}a&b\\end{array}",
                    MA.Table(
                        grid [ [ c 'a'; c 'b' ] ],
                        ImmutableArray.Create(Alignment.Left, Alignment.Right))
                    // The environments that align on one mark, which is the grid \begin{align} sets.
                    "\\begin{aligned}a&=b\\end{aligned}", read "\\begin{align}a&=b\\end{align}"
                    "\\begin{split}a&=b\\end{split}", read "\\begin{align}a&=b\\end{align}"
                    // Those that align on none, whose rows are one centred column.
                    "\\begin{gather}a\\\\b\\end{gather}", MA.Matrix(grid [ [ c 'a' ]; [ c 'b' ] ])
                    "\\begin{gathered}a\\end{gathered}", MA.Matrix(grid [ [ c 'a' ] ])
                    "\\begin{gather*}a\\end{gather*}", MA.Matrix(grid [ [ c 'a' ] ])
                ]
            )
            same(
                "spacesAndColoursAreReadAsTheyAreWritten",
                [
                    "a\\,b", row [ c 'a'; MA.Space Space.Thin; c 'b' ]
                    "a\\qquad b", row [ c 'a'; MA.Space Space.QQuad; c 'b' ]
                    "a\\thinspace b", read "a\\,b"
                    "a\\negthinspace b", read "a\\!b"
                    "a\\medspace b", read "a\\:b"
                    "a\\thickspace b", read "a\\;b"
                    // A tie is a space that holds a line against a break, which a formula never takes.
                    "a~b", read "a\\ b"
                    // A colour is what it paints, so a name reads as the hex for it reads.
                    "\\color{red}{x}", read "\\color{#FF0000}{x}"
                    "\\color{RED}{x}", read "\\color{#FF0000}{x}"
                    "\\textcolor{#0000FF}{x}", MA.Coloured(Color.FromArgb(255, 0, 0, 255), c 'x')
                ]
            )
            same(
                "aModulusStandsForTheGapsAndWordsItIsDefinedAs",
                [
                    "a\\bmod b", read "a\\:\\text{mod}\\:b"
                    "a\\pmod{n}", read "a\\quad(\\text{mod}\\;n)"
                    "a\\mod{n}", read "a\\quad\\text{mod}\\;n"
                ]
            )
            rejected(
                "whatIsNotUnderstoodIsRefusedRatherThanGuessedAt",
                [
                    "\\foo"
                    "\\alphax"
                    // A style switch changes how a formula is set, so ignoring one would set it wrongly.
                    "\\displaystyle\\frac{1}{2}"
                    "\\textstyle x"
                    "{a"
                    "a}"
                    "\\right)"
                    "\\left(x"
                    "x^1^2"
                    "$x$"
                    "\\frac{1}"
                    // A script is no argument, so a command asking for one is not given an empty base.
                    "\\frac^2b"
                    "x^_2"
                    "\\begin{matrix}a\\end{cases}"
                    "\\begin{smallmatrix}a\\end{smallmatrix}"
                    // A name the reader would have to invent a shape for, which \operatorname never asks.
                    "\\operatorname{Var\u0416}"
                    // TeX calls two of these in one group ambiguous, and so does this.
                    "a \\choose b \\choose c"
                    "\\color{fuchsias}{x}"
                    // The desktop theme is no colour for a formula to be written in.
                    "\\color{MenuHighlight}{x}"
                    "\\color{Control}{x}"
                    // A space is no hex digit: counting one would read this as an alpha of zero.
                    "\\color{# FF0000 }{x}"
                    "\\color{#FF000 }{x}"
                    // Six digits or eight, so the three CSS allows are not stretched into six.
                    "\\color{#FFF}{x}"
                    "\\"
                    // Only what the font has a struck form of can stand after \not.
                    "\\not"
                    "\\not+"
                    "\\not\\alpha"
                    "\\not{ab}"
                ]
            )
            rejected(
                "aCharacterTheFontCannotDrawIsRefusedWhereItStands",
                [
                    "a☃b"
                    // Beyond the basic plane only the italic alphabet stands for anything a formula holds.
                    "𝔄"
                    // Text is set upright, and the italic shapes of a formula are no substitute.
                    "\\text{ϵ}"
                    "\\mathrm{ϕ}"
                ]
            )
            Test.Sync(
                "whereACharacterIsRefusedIsSaidAlongWithWhy",
                fun () ->
                    match Latex.Read "ab☃" with
                    | Ok ma -> Assert.Fail $"read as {ma}"
                    | Error error ->
                        Assert.Equal(2, error.Position, "the character refused was not the one at fault")
                        Assert.True(error.Message.Contains '☃', $"{error.Message} does not name it")
            )
            // Refusing at the door is what lets laying a formula out be total.
            Test.CasesSync(
                "everythingReadCanBeDrawn",
                named [
                    "x+1"
                    "\\frac{a}{b}"
                    "\\text{cost in £}"
                    "\\mathbf{v}_1"
                    "\\mathbb{R}^n"
                    "\\sqrt[3]{x}"
                    "\\begin{matrix}a&b\\\\c&d\\end{matrix}"
                    "\\mathrm{μg}"
                    "\\mathrm{m\\,s^{-1}}"
                    "\\alpha\\uparrow\\circ\\triangle"
                    "\\not\\equiv\\not\\subseteq\\not\\Rightarrow"
                ],
                fun latex -> Assert.Equal(ImmutableArray<char>.Empty, (read latex).Undrawable, latex)
            )
            Test.CasesSync(
                "everyNamedOrStruckCharacterIsWrittenAsItselfAndReadsBack",
                namedSymbols @ (Latexing.negated |> List.map (fun (_, struck) -> string struck, struck)),
                fun character ->
                    let written = Latex.Write(c character)
                    // Barring the backslash and the characters LaTeX keeps, which stand under one.
                    if not("\\{}" |> Seq.contains character) then Assert.Equal(string character, written)
                    Assert.Equal(c character, read written, written)
            )
            Test.CasesSync(
                "everyStruckCharacterDrawsAndIsClassedAsThePlainOne",
                Latexing.negated |> List.map (fun (plain, struck) -> string struck, (plain, struck)),
                fun (plain, struck) ->
                    layout.Of(MA.Char struck) |> ignore
                    Assert.Equal(Conventions.relations.Contains plain, Conventions.relations.Contains struck)
            )
            Test.Sync(
                "everyNamedSymbolHasAGlyphToDrawIt",
                fun () ->
                    let missing = System.Text.StringBuilder()
                    for name, character in namedSymbols do
                        try layout.Of(MA.Char character) |> ignore
                        with _ -> missing.Append(name).Append(' ') |> ignore
                    Assert.Equal("", missing.ToString(), "named with no glyph to draw them")
            )
        ]
    )

let private writing =
    TestList(
        "Writing LaTeX",
        [   Test.Sync(
                "aPairOfScriptsIsWrittenTheOneWayRoundHoweverItWasRead",
                fun () ->
                    // TeX takes them in either order, and the subscript goes first as it does on \sum.
                    for latex in [ "x_1^2"; "x^2_1" ] do
                        match Latex.Read latex with
                        | Ok ma -> Assert.Equal("x_{1}^{2}", Latex.Write ma, $"reading {latex}")
                        | Error error -> failwith $"{latex} was turned down: {error.Message}"
            )
            writes(
                "everyArgumentIsWrittenInBraces",
                [
                    MA.ScriptSub(c 'f', c 'x'), "f_{x}"
                    MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i')), "x_{i}^{2}"
                    MA.Frac(c '1', c '2'), @"\frac{1}{2}"
                    MA.Sqrt(c 'x'), @"\sqrt{x}"
                    MA.RootN(c '3', c 'x'), @"\sqrt[{3}]{x}"
                    MA.Accented(Accent.Hat, c 'y'), @"\hat{y}"
                    MA.BoldVar 'v', @"\mathbf{v}"
                    MA.Text "in metres", @"\text{in metres}"
                    MA.BigOp(BigOperator.Sum, ValueSome(c 'i'), ValueSome(c 'n')), @"\sum_{i}^{n}"
                    // What a script goes on is braced only where it is more than one atom.
                    MA.ScriptSuper(MA.String "ab", c '2', ValueNone), "{ab}^{2}"
                    MA.ScriptSub(MA.ScriptSuper(c 'x', c '2', ValueNone), c 'i'), "{x^{2}}_{i}"
                ]
            )
            writes(
                "aControlWordIsKeptFromRunningIntoWhatFollowsIt",
                [
                    row [ MA.Function MathFunction.Sin; c 'x' ], @"\sin x"
                    // Letters of a formula are no control word, so nothing stands between them.
                    MA.String "abc", "abc"
                    // A Greek letter spells no control word, so nothing is needed before it either.
                    row [ MA.Function MathFunction.Sin; c 'α' ], @"\sinα"
                ]
            )
            writes(
                "aCharacterIsWrittenAsItselfRatherThanByAName",
                [
                    row [ c 'a'; c '≢'; c 'b' ], "a≢b"
                    // The one whose self is the escape, and which a letter must not run on from.
                    row [ c '\\'; c 'x' ], @"\backslash x"
                ]
            )
            Test.Sync(
                "aCharacterLatexKeepsForItselfIsWrittenUnderABackslash",
                fun () ->
                    // A caret is one of them: it is drawn and typed like any other character.
                    Assert.Equal(@"\{\&\$\_\%\#\^\}", Latex.Write(MA.String "{&$_%#^}"))
                    Assert.Equal(@"\text{\$5}", Latex.Write(MA.Text "$5"), "in text as well")
            )
            Test.Sync(
                "whatLatexHasNoWayToSayIsSaidAsNearlyAsItCan",
                fun () ->
                    // A bracket still waiting for its pair is written as the completed one.
                    let waiting =
                        MA.Bracketed(Brackets.Matching Bracket.Normal, c 'x', BracketCompletion.Left)
                    Assert.Equal(
                        MA.RoundBracket(c 'x'),
                        read(Latex.Write waiting),
                        "a tentative bracket read back as something other than a completed pair")
            )
            Test.CasesSync(
                "everyKindOfAtomIsWrittenSoItReadsBackTheSame",
                named [
                    MA.Row(ImmutableArray.Create(c 'x', c '+', c '1'))
                    c 'x'
                    c 'α'
                    MA.BoldVar 'v'
                    MA.Blackboard 'R'
                    MA.UprightD
                    MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i'))
                    MA.ScriptSub(c 'f', c 'x')
                    MA.Frac(c '1', c '2')
                    MA.Function MathFunction.Sin
                    MA.RoundBracket(MA.String "x+1")
                    MA.Bracketed(Brackets(Bracket.Curly, Bracket.None), c 'x', BracketCompletion.Completed)
                    MA.Bracketed(Brackets(Bracket.Square, Bracket.Angle), c 'x', BracketCompletion.Completed)
                    // A control word naming a delimiter runs on into the letter after it unless spaced.
                    MA.Paired(Bracket.Angle, c 'x')
                    MA.Paired(Bracket.Floor, c 'x')
                    MA.Paired(Bracket.Ceiling, c 'x')
                    MA.Bracketed(Brackets(Bracket.None, Bracket.Slash), c 'x', BracketCompletion.Completed)
                    MA.RootN(c '3', c 'x')
                    MA.Sqrt(c 'x')
                    MA.BigOp(BigOperator.Sum, ValueSome(c 'i'), ValueSome(c 'n'))
                    MA.BigOp(BigOperator.Integral, ValueNone, ValueNone)
                    MA.Accented(Accent.Hat, c 'y')
                    MA.Overline(c 'Y')
                    MA.Underline(c 'Y')
                    MA.Stack(c 'n', c 'k')
                    MA.Binom(c 'n', c 'k')
                    MA.Matrix cells
                    MA.Cases cells
                    MA.Table(cells, ImmutableArray.Create(Alignment.Right, Alignment.Centre))
                    MA.Spanned(Spanning.Overbrace, MA.String "ab")
                    MA.Coloured(Color.FromArgb(255, 255, 0, 0), c 'x')
                    MA.Coloured(Color.FromArgb(255, 1, 2, 3), c 'x')
                    MA.Coloured(Color.FromArgb(128, 1, 2, 3), c 'x')
                    MA.RootN(c ']', c 'x')
                    c '^'
                    MA.Text "$5 {a} 100%"
                    MA.Text "in metres"
                    MA.Space Space.Thin
                    MA.String "{&$_%#}"
                ],
                fun formula ->
                    let written = Latex.Write formula
                    Assert.Equal(formula.Flatten, read written, written)
            )
        ]
    )

let private colouring =
    let differentGreen = Color.FromArgb(255, 130, 212, 20)
    let ours = Palette.Default.With("green", differentGreen)
    let coloured(latex: string, palette: Palette) =
        match Latex.Read(latex, palette) with
        | Ok(MA.Coloured(colour, _)) -> colour
        | Ok other -> failwith $"{latex} was read as {other}"
        | Error error -> failwith $"{latex}: {error}"
    TestList(
        "Palettes",
        [   Test.Sync(
                "aPaletteGivesTheColourAName",
                fun () ->
                    Assert.Equal(differentGreen, coloured("\\color{green}{x}", ours), "the name given went unread")
                    Assert.Equal(
                        Color.FromArgb(255, 0, 128, 0),
                        coloured("\\color{green}{x}", Palette.Default),
                        "one palette answered for another")
            )
            Test.Sync(
                "aNameGivenTakesThePlaceOfTheOneAlreadyThere",
                fun () ->
                    Assert.Equal(differentGreen, coloured("\\color{GREEN}{x}", ours), "case decided the colour")
                    Assert.Equal(
                        Color.FromArgb(255, 255, 0, 0),
                        coloured("\\color{red}{x}", ours),
                        "a name not given was lost with the one that was")
            )
            Test.Sync(
                "aColourOutsideThePaletteIsRefused",
                fun () ->
                    let bare = Palette(ImmutableDictionary.Empty.Add("green", differentGreen))
                    Assert.Equal(differentGreen, coloured("\\color{green}{x}", bare), "the one name given")
                    match Latex.Read("\\color{red}{x}", bare) with
                    | Ok ma -> Assert.Fail $"red was read as {ma} from a palette without it"
                    | Error _ -> ()
            )
            Test.Sync(
                "aColourNamedIsWrittenAsWhatItIsRatherThanWhatItWasCalled",
                fun () ->
                    let formula = MA.Coloured(differentGreen, c 'x')
                    Assert.Equal("\\color{#82D414}{x}", Latex.Write formula, "green was written by name")
                    Assert.Equal(formula, read(Latex.Write formula), "it did not read back the same")
            )
            Test.Sync(
                "aColourBuiltFromAKnownOneComesBackAsWhatItPaints",
                fun () ->
                    // Color.Red is equal to no other Color, so it comes back as its ARGB and not itself.
                    let written = Latex.Write(MA.Coloured(Color.Red, c 'x'))
                    Assert.Equal("\\color{#FF0000}{x}", written, "a known colour was written by name")
                    Assert.Equal(MA.Coloured(Color.FromArgb(255, 255, 0, 0), c 'x'), read written, written)
                    Assert.Equal(Color.Red.ToArgb(), (Color.FromArgb(255, 255, 0, 0)).ToArgb(), "what it paints")
            )
        ]
    )

let tests = TestFolder("Latex", [ reading; writing; colouring ])
