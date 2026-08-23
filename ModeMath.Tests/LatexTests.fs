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

let private read(latex: string) =
    match Latex.Read latex with
    | Ok ma -> ma
    | Error error -> failwith $"{latex}: {error}"

let private same(pairs: (string * MA) list) =
    fun () ->
        for latex, expected in pairs do
            Assert.Equal(expected, read latex, latex)

let private rejected(strings: string list) =
    fun () ->
        for latex in strings do
            match Latex.Read latex with
            | Ok ma -> Assert.Fail $"{latex} was read as {ma}"
            | Error _ -> ()

let private reading =
    TestList(
        "Reading LaTeX",
        [   Test.Sync(
                "charactersStandForThemselvesAndSpacesForNothing",
                same [
                    "ab", MA.String "ab"
                    "a b", MA.String "ab"
                    "a+b", row [ c 'a'; c '+'; c 'b' ]
                    "{ab}c", MA.String "abc"
                    "{a}", c 'a'
                    "", MA.Empty
                ]
            )
            Test.Sync(
                "aNamedSymbolStandsForItsCharacter",
                same [
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
                    // Only the 26 letters spell a command, so \cosθ is a function and a letter.
                    "\\cosθ", row [ MA.Function MathFunction.Cos; c 'θ' ]
                ]
            )
            Test.Sync(
                "chooseStandsBetweenWhatItSetsOverAndUnder",
                same [
                    // The one infix command, which takes what is on either side of it rather than after.
                    "{8 \\choose 6}", MA.Binom(c '8', c '6')
                    "n \\choose k", MA.Binom(c 'n', c 'k')
                    "{n+1 \\choose 2}", MA.Binom(row [ c 'n'; c '+'; c '1' ], c '2')
                    // What stands outside the braces is no part of it.
                    "{2 \\choose n+1}", MA.Binom(c '2', row [ c 'n'; c '+'; c '1' ])
                    "P{4 \\choose 2}", row [ c 'P'; MA.Binom(c '4', c '2') ]
                    // A group of its own is a scope of its own, so the second choose is unambiguous.
                    "{n \\choose {a \\choose b}}", MA.Binom(c 'n', MA.Binom(c 'a', c 'b'))
                ]
            )
            Test.Sync(
                "aHouseMacroIsReadAsWhatItStandsFor",
                same [
                    "\\overbar{Y}", MA.Overline(c 'Y')
                    "\\bf{F}", MA.BoldVar 'F'
                    "\\bullet", c '•'
                ]
            )
            Test.Sync(
                "aScriptGoesOnTheAtomBeforeIt",
                same [
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
            Test.Sync(
                "aLargeOperatorTakesItsScriptsAsLimits",
                same [
                    "\\sum_{i=0}^n",
                    MA.BigOp(BigOperator.Sum, ValueSome(row [ c 'i'; c '='; c '0' ]), ValueSome(c 'n'))
                    "\\int_0^\\infty", MA.BigOp(BigOperator.Integral, ValueSome(c '0'), ValueSome(c '∞'))
                    "\\int", MA.BigOp(BigOperator.Integral, ValueNone, ValueNone)
                ]
            )
            Test.Sync(
                "aCommandTakesTheGroupOrTheOneAtomAfterIt",
                same [
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
            Test.Sync(
                "aFunctionNameIsOneAtomAndTakesNoArgument",
                same [
                    "\\sin x", row [ MA.Function MathFunction.Sin; c 'x' ]
                    "\\sin^2 x", row [ MA.ScriptSuper(MA.Function MathFunction.Sin, c '2', ValueNone); c 'x' ]
                    "\\arcsin", MA.Function MathFunction.Asin
                    "\\arg", MA.Function MathFunction.Arg
                    "\\det", MA.Function MathFunction.Det
                    // \operatorname names a function the same way a command of its own does.
                    "\\operatorname{Im}", MA.Function MathFunction.Imaginary
                    "\\operatorname{arcosh}x", row [ MA.Function MathFunction.Arcosh; c 'x' ]
                    // ISO 80000-2 names the inverse hyperbolics for the area they take, not an arc.
                    "\\arccosh", MA.Function MathFunction.Arcosh
                    "\\arcsinh", MA.Function MathFunction.Arsinh
                    "\\arctanh", MA.Function MathFunction.Artanh
                    "\\artanh", MA.Function MathFunction.Artanh
                ]
            )
            Test.Sync(
                "onlyAMarkedPairBracketsAFormula",
                same [
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
                    // Plain brackets do not grow in LaTeX, so they are read as the characters they are.
                    "(x)", row [ c '('; c 'x'; c ')' ]
                ]
            )
            Test.Sync(
                "wordsInBracesKeepTheirSpacesAndTheirCase",
                same [
                    "\\text{if } x", row [ MA.Text "if "; c 'x' ]
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
            Test.Sync(
                "aTableIsReadFromItsCellsAndItsRowBreaks",
                same [
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
                ]
            )
            Test.Sync(
                "spacesAndColoursAreReadAsTheyAreWritten",
                same [
                    "a\\,b", row [ c 'a'; MA.Space Space.Thin; c 'b' ]
                    "a\\qquad b", row [ c 'a'; MA.Space Space.QQuad; c 'b' ]
                    "\\color{red}{x}", MA.Coloured(Color.Red, c 'x')
                    "\\textcolor{#0000FF}{x}", MA.Coloured(Color.FromArgb(255, 0, 0, 255), c 'x')
                ]
            )
            Test.Sync(
                "whatIsNotUnderstoodIsRefusedRatherThanGuessedAt",
                rejected [
                    "\\foo"
                    "\\alphax"
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
                    // A function is named from a closed set, so a name outside it is not guessed at.
                    "\\operatorname{Var}"
                    // TeX calls two of these in one group ambiguous, and so does this.
                    "a \\choose b \\choose c"
                    "\\color{fuchsias}{x}"
                    // A space is no hex digit: counting one would read this as an alpha of zero.
                    "\\color{# FF0000 }{x}"
                    "\\color{#FF000 }{x}"
                    // Six digits or eight, so the three CSS allows are not stretched into six.
                    "\\color{#FFF}{x}"
                    "\\"
                ]
            )
            Test.Sync(
                "aCharacterTheFontCannotDrawIsRefusedWhereItStands",
                rejected [
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
            Test.Sync(
                "everythingReadCanBeDrawn",
                fun () ->
                    // Refusing at the door is what lets laying a formula out be total.
                    let readable =
                        [ "x+1"; "\\frac{a}{b}"; "\\text{cost in £}"; "\\mathbf{v}_1"
                          "\\mathbb{R}^n"; "\\sqrt[3]{x}"; "\\begin{matrix}a&b\\\\c&d\\end{matrix}"
                          "\\mathrm{μg}"; "\\alpha\\uparrow\\circ\\triangle" ]
                    for latex in readable do
                        Assert.Equal(ImmutableArray<char>.Empty, (read latex).Undrawable, latex)
            )
            Test.Sync(
                "everyNamedSymbolHasAGlyphToDrawIt",
                fun () ->
                    let missing = System.Text.StringBuilder()
                    for command in Latexing.commands do
                        match command.Value with
                        | Latexing.Standing.Symbol character ->
                            try layout.Of(MA.Char character) |> ignore
                            with _ -> missing.Append(command.Key).Append(' ') |> ignore
                        | Latexing.Standing.Function _ | Latexing.Standing.BigOp _
                        | Latexing.Standing.Space _ | Latexing.Standing.Accent _
                        | Latexing.Standing.Spanning _ -> ()
                    Assert.Equal("", missing.ToString(), "named with no glyph to draw them")
            )
        ]
    )

let private writing =
    TestList(
        "Writing LaTeX",
        [   Test.Sync(
                "everyArgumentIsWrittenInBraces",
                fun () ->
                    let written =
                        [
                            MA.ScriptSub(c 'f', c 'x'), "f_{x}"
                            MA.ScriptSuper(c 'x', c '2', ValueSome(c 'i')), "x^{2}_{i}"
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
                    for formula, expected in written do
                        Assert.Equal(expected, Latex.Write formula, string formula)
            )
            Test.Sync(
                "aControlWordIsKeptFromRunningIntoWhatFollowsIt",
                fun () ->
                    let written =
                        [
                            row [ c 'α'; c 'x' ], @"\alpha x"
                            row [ MA.Function MathFunction.Sin; c 'x' ], @"\sin x"
                            // Letters of a formula are no control word, so nothing stands between them.
                            MA.String "abc", "abc"
                            // A backslash ends the word before it, so no space is needed either.
                            row [ c 'α'; c 'β' ], @"\alpha\beta"
                        ]
                    for formula, expected in written do
                        Assert.Equal(expected, Latex.Write formula, string formula)
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
            Test.Sync(
                "everyKindOfAtomIsWrittenSoItReadsBackTheSame",
                fun () ->
                    let grid = ImmA2D.fromJagged [ [ c 'a'; c 'b' ]; [ c 'c'; c 'd' ] ]
                    let atoms =
                        [
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
                            MA.RootN(c '3', c 'x')
                            MA.Sqrt(c 'x')
                            MA.BigOp(BigOperator.Sum, ValueSome(c 'i'), ValueSome(c 'n'))
                            MA.BigOp(BigOperator.Integral, ValueNone, ValueNone)
                            MA.Accented(Accent.Hat, c 'y')
                            MA.Overline(c 'Y')
                            MA.Underline(c 'Y')
                            MA.Stack(c 'n', c 'k')
                            MA.Binom(c 'n', c 'k')
                            MA.Matrix grid
                            MA.Cases grid
                            MA.Table(grid, ImmutableArray.Create(Alignment.Right, Alignment.Centre))
                            MA.Spanned(Spanning.Overbrace, MA.String "ab")
                            MA.Coloured(Color.Red, c 'x')
                            MA.Coloured(Color.FromArgb(255, 1, 2, 3), c 'x')
                            MA.Coloured(Color.FromArgb(128, 1, 2, 3), c 'x')
                            MA.RootN(c ']', c 'x')
                            c '^'
                            MA.Text "$5 {a} 100%"
                            MA.Text "in metres"
                            MA.Space Space.Thin
                            MA.String "{&$_%#}"
                        ]
                    for formula in atoms do
                        let written = Latex.Write formula
                        Assert.Equal(formula.Flatten, read written, written)
            )
        ]
    )

let tests = TestFolder("Latex", [ reading; writing ])
