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
                    "\\circ", c '∘'
                    "\\triangle", c '△'
                    "\\uparrow\\downarrow", row [ c '↑'; c '↓' ]
                    "\\longrightarrow", c '⟶'
                    "\\pounds", c '£'
                    // A word processor writes its variables in the italic alphabet Unicode holds.
                    "𝑠𝑜𝑐", MA.String "soc"
                    "𝐴𝜋", row [ c 'A'; c 'π' ]
                    // A mark that gives no ink of its own is nothing to draw.
                    "a​b", MA.String "ab"
                    // A spreadsheet writes the bar of a conditional probability as a drawing rule.
                    "P(A│B)", read "P(A|B)"
                    // Only the 26 letters spell a command, so \cosθ is a function and a letter.
                    "\\cosθ", row [ MA.Function MathFunction.Cos; c 'θ' ]
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
                    "\\color{fuchsias}{x}"
                    "\\"
                ]
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

let tests = TestFolder("Latex", [ reading ])
