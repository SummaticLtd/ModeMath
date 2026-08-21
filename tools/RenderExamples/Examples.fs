module RenderExamples.Examples

open System.Collections.Immutable
open System.Drawing
open FSUtils
open ModeMath

let private row(elements: MA list) = MA.Row(elements.ToImmutableArray())
let private c(character: char) = MA.Char character
let private s(text: string) = MA.String text
let private frac(numerator: MA, denominator: MA) = MA.Frac(numerator, denominator)
let private sup(main: MA, super: MA) = MA.ScriptSuper(main, super, ValueNone)
let private sub(main: MA, subscript: MA) = MA.ScriptSub(main, subscript)
let private supsub(main: MA, super: MA, subscript: MA) = MA.ScriptSuper(main, super, ValueSome subscript)
let private sqrt(radicand: MA) = MA.Sqrt radicand
let private root(degree: MA, radicand: MA) = MA.RootN(degree, radicand)
let private paren(inner: MA) = MA.RoundBracket inner
let private bars(inner: MA) = MA.Bracketed(Brackets.Matching Bracket.Line, inner, BracketCompletion.Completed)
let private square(inner: MA) = MA.Paired(Bracket.Square, inner)
let private curly(inner: MA) = MA.Paired(Bracket.Curly, inner)
let private pair(left: Bracket, right: Bracket, inner: MA) =
    MA.Bracketed(Brackets(left, right), inner, BracketCompletion.Completed)
let private bra(inner: MA) = pair(Bracket.Angle, Bracket.Line, inner)
let private ket(inner: MA) = pair(Bracket.Line, Bracket.Angle, inner)
let private fn(f: MathFunction) = MA.Function f
let private op(o: Operator) = MA.Operator o
let private bigOp(o: BigOperator) = MA.BigOp(o, ValueNone, ValueNone)
let private bigOpSub(o: BigOperator, lower: MA) = MA.BigOp(o, ValueSome lower, ValueNone)
let private bigOpSup(o: BigOperator, upper: MA) = MA.BigOp(o, ValueNone, ValueSome upper)
let private bigOpSubSup(o: BigOperator, lower: MA, upper: MA) = MA.BigOp(o, ValueSome lower, ValueSome upper)
let private acc(accent: Accent, x: MA) = MA.Accented(accent, x)
let private spanned(mark: Spanning, x: MA) = MA.Spanned(mark, x)
let private text(words: string) = MA.Text words
let private coloured(colour: Color, x: MA) = MA.Coloured(colour, x)
let private space(width: Space) = MA.Space width
let private bb(letter: char) = MA.Blackboard letter
let private overline(x: MA) = MA.Overline x
let private underline(x: MA) = MA.Underline x
let private binom(top: MA, bottom: MA) = MA.Binom(top, bottom)
let private matrix(cells: MA list list) = MA.Matrix(ImmA2D.fromJagged cells)
let private cases(cells: MA list list) = MA.Cases(ImmA2D.fromJagged cells)
let private grid(cells: MA list list, alignments: Alignment list) =
    MA.Table(ImmA2D.fromJagged cells, alignments.ToImmutableArray())
/// The right-hand side of an evaluated integral: an open left side and a bar carrying the limits.
let private evaluatedAt(x: MA, lower: MA, upper: MA) =
    supsub(pair(Bracket.None, Bracket.Line, x), upper, lower)

/// 1 + e^(-2pi) / (1 + e^(-4pi) / (...)), the right-hand side of Ramanujan's identity.
let private continuedFraction(depth: int) =
    let rec build(level: int) =
        let exponent = row [ c '-'; c (char (int '0' + 2 * level)); c 'π' ]
        let inner = if level = depth then row [ c '1'; c '+'; c '⋯' ] else build (level + 1)
        row [ c '1'; c '+'; frac(sup(c 'e', exponent), inner) ]
    build 1

let private nestedRadical(depth: int) =
    let rec build(level: int) = if level = 0 then c 'x' else sqrt(build (level - 1))
    build depth

/// The left-hand side of Ramanujan's identity.
let private leftSide =
    frac(
        c '1',
        row [
            paren(row [ sqrt(row [ c 'ϕ'; sqrt(c '5') ]); c '-'; c 'ϕ' ])
            sup(c 'e', row [ frac(c '2', c '5'); c 'π' ])
        ])

/// Examples from CSharpMath.Rendering.Tests/MathDisplay, plus a few that exercise MA alone.
let implemented = [
    "Alphabets", s "abcdefghijklmnopqrstuvwxyz"
    "Capitals", s "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "CapitalGreeks", s "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
    "Greeks", s "αβγδεζηθικλμνξοπρςστυφχψω"
    "Numbers", s "1234567890"

    "Color",
    row [
        coloured(Color.FromArgb(0x00, 0x00, 0x88), c 'a')
        coloured(Color.FromArgb(0x00, 0x00, 0xFF), c 'b')
    ]

    "Commands",
    row [
        c '5'
        op Operator.Times
        paren(row [ c '-'; c '2'; op Operator.Divide; c '1' ])
        op Operator.Equals
        c '-'
        s "10"
    ]

    "Exponential", sup(c 'e', c '2')
    "ExponentWithFraction", sup(c 'e', row [ c '4'; frac(c '2', c '5') ])
    "ExponentWithPi", sup(c 'e', row [ c '2'; c 'π' ])
    "ExponentWithProduct", sup(c 'e', s "2x")

    "Fraction", frac(c '2', s "34")
    "FractionNested",
    frac(
        sup(c 'e', row [ c '-'; c '6'; c 'π' ]),
        row [ c '1'; c '+'; frac(sup(c 'e', row [ c '-'; c '8'; c 'π' ]), row [ c '1'; c '+'; c '⋯' ]) ])
    "FractionWithRoot", frac(c '1', sqrt(c '2'))
    "FunctionDomainCodomain", row [ c 'f'; c ':'; c 'ℕ'; c '→'; c 'ℕ' ]

    "IntPlusFraction", row [ c '1'; c '+'; frac(c '2', c '3') ]
    "BraSum",
    row [
        frac(c '1', sqrt(sup(c '2', c 'n')))
        bigOpSubSup(
            BigOperator.Sum,
            row [ c 'i'; op Operator.Equals; c '0' ],
            row [ sup(c '2', c 'n'); c '-'; c '1' ])
        bra(c 'i')
    ]
    "KetSum",
    row [
        frac(c '1', sqrt(sup(c '2', c 'n')))
        bigOpSubSup(
            BigOperator.Sum,
            row [ c 'i'; op Operator.Equals; c '0' ],
            row [ sup(c '2', c 'n'); c '-'; c '1' ])
        ket(c 'i')
    ]
    "LargeBra", bra(row [ frac(c 'a', c '2'); c '+'; frac(c 'b', c '3') ])
    "LargeKet", ket(row [ frac(c 'a', c '2'); c '+'; frac(c 'b', c '3') ])
    "LargerDelimiters", sup(paren(sup(square(sup(curly(c '□'), c '□')), c '□')), c '□')
    "IntegralScripts",
    row [
        bigOp BigOperator.Integral
        bigOp BigOperator.Integral
        bigOpSup(BigOperator.Integral, c '∞')
        bigOpSub(BigOperator.Integral, c '0')
        bigOpSubSup(BigOperator.Integral, c '0', c '∞')
        bigOp BigOperator.Integral
    ]
    "ItalicScripts",
    row [
        supsub(c 'U', c '2', c '3')
        c 'U'
        supsub(c 'Y', c '2', c '3')
        sub(c 'U', c '3')
        sup(c 'Y', c '2')
        sub(c 'f', c '1')
        sup(c 'f', c '2')
        c 'f'
        c 'f'
    ]

    "LeftRight", paren(frac(c '2', c '3'))
    "LeftRightMinus", row [ paren(frac(c '2', c '3')); c '-' ]
    "LeftSide", leftSide
    "FractionNestedDeep", row [ leftSide; op Operator.Equals; continuedFraction 4 ]
    "LnEquation",
    row [
        fn MathFunction.Ln
        paren(c 'P')
        op Operator.Equals
        sub(c 'a', c '0')
        c '-'
        frac(sub(c 'a', c '1'), row [ sub(c 'a', c '2'); c '+'; c 'T' ])
    ]
    "Logic",
    row [
        c '¬'
        paren(row [ c 'P'; c '∧'; c 'Q' ])
        c '⟺'
        paren(row [ c '¬'; c 'P' ])
        c '∨'
        paren(row [ c '¬'; c 'Q' ])
    ]

    "Nothing", MA.Empty
    "Phi", c 'ϕ'
    "Pi", c 'π'

    "QuadraticFormula",
    row [
        c 'x'
        op Operator.Equals
        c '-'
        c 'b'
        c '±'
        frac(sqrt(row [ sup(c 'b', c '2'); c '-'; s "4ac" ]), s "2a")
    ]

    "ShortIntegral", bigOpSubSup(BigOperator.Integral, c '0', c '1')
    "SimpleLimit",
    row [
        bigOpSub(BigOperator.Limit, row [ c 'x'; c '→'; c '∞' ])
        c '3'
        op Operator.Equals
        c '3'
    ]
    "SummationBigCup", row [ s "234"; bigOpSub(BigOperator.Union, c '1') ]
    "SummationDouble", row [ bigOp BigOperator.Sum; bigOp BigOperator.Sum ]
    "SummationWithBigLimits",
    row [
        bigOpSubSup(
            BigOperator.Sum,
            row [ c 'i'; op Operator.Equals; sub(c '3', sub(c '2', c '1')) ],
            sup(c '4', sup(c '5', c '6')))
        c 'i'
    ]
    "SummationWithCup",
    row [
        bigOpSubSup(BigOperator.Sum, row [ c 'n'; op Operator.Equals; c '1' ], c '∞')
        frac(row [ c '1'; c '+'; c 'n' ], row [ c '1'; c '-'; c 'n' ])
        op Operator.Equals
        bigOpSub(BigOperator.Union, c '1')
        c 'C'
        c '∪'
        c 'B'
    ]
    "SummationWithLimits",
    bigOpSubSup(BigOperator.Sum, row [ c 'n'; op Operator.Equals; c '1' ], c '∞')

    "QuarticSolutions",
    curly(
        row [
            c '-'
            c '1'
            c '+'
            frac(
                row [
                    c '-'
                    sqrt(
                        row [
                            s "10"
                            c '+'
                            c '2'
                            op Operator.Times
                            paren(frac(row [ c '-'; s "25" ], c '3'))
                        ])
                ],
                c '2')
        ])

    "Radical", sqrt(c '3')
    "RadicalFraction", row [ c '2'; c '+'; frac(sqrt(c '3'), c '2') ]
    "RadicalNested", sqrt(sqrt(c 'x'))
    "RadicalNestedDeep", nestedRadical 8
    "RadicalPower", sup(sqrt(c '2'), sqrt(c '3'))
    "RadicalSum", row [ c '2'; c '+'; sqrt(c '3') ]
    "RightSide", continuedFraction 4

    "TangentPeriodShift",
    row [
        fn MathFunction.Tan
        paren(row [ c 'θ'; c '±'; frac(c 'π', c '4') ])
        op Operator.Equals
        frac(
            row [ fn MathFunction.Tan; c 'θ'; c '±'; c '1' ],
            row [ c '1'; c '∓'; fn MathFunction.Tan; c 'θ' ])
    ]
    "TwoSin", row [ c '2'; fn MathFunction.Sin ]

    "Abs",
    row [
        bars(c 'x')
        op Operator.Equals
        cases [
            [ row [ c '-'; c 'x'; c ',' ]; row [ text " if "; c 'x'; c '<'; c '0' ] ]
            [ row [ c 'x'; c ',' ]; row [ text " if "; c 'x'; c '≥'; c '0' ] ]
        ]
    ]
    "AccentOver", acc(Accent.Acute, c 'x')
    "AccentOverMultiple", acc(Accent.WideHat, s "ABcd")
    "ArcsinSin",
    row [
        fn MathFunction.Asin
        paren(row [ fn MathFunction.Sin; c 'x' ])
        op Operator.Equals
        c 'x'
        space Space.Quad
        text "for"
        space Space.Quad
        bars(c 'x')
        c '≤'
        frac(c 'π', c '2')
    ]
    "Cases",
    row [
        c 'w'
        c '≡'
        cases
            [ [ c '0'
                row [
                    text "for"
                    space Space.Thin
                    c 'c'
                    op Operator.Equals
                    c 'd'
                    op Operator.Equals
                    c '0'
                ] ] ]
    ]
    "EvalIntegral",
    row [
        bigOpSubSup(BigOperator.Integral, c '1', c '2')
        c 'x'
        space Space.Thick
        MA.UprightD
        c 'x'
        op Operator.Equals
        evaluatedAt(frac(sup(c 'x', c '2'), c '2'), c '1', c '2')
    ]
    "Integral",
    row [
        bigOpSubSup(BigOperator.Integral, c '0', c '∞')
        sup(c 'e', c 'x')
        space Space.Thin
        MA.UprightD
        c 'x'
        op Operator.Equals
        bigOpSubSup(BigOperator.ContourIntegral, c '0', c 'Δ')
        c '5'
        fn MathFunction.Gamma
    ]
    "SolveEquations",
    row [
        text "Solve "
        cases [ [ row [ c 'y'; op Operator.Equals; sup(c 'x', c '2'); c '-'; c 'x'; c '+'; c '3' ] ] ]
    ]
    "Underbrace", spanned(Spanning.Underbrace, s "abcd")
    "UnderbraceSubscript", sub(spanned(Spanning.Underbrace, s "abcdefghklmnopqrst"), s "eee")
    "AccentOverF", acc(Accent.Hat, c 'f')
    "Choose", binom(c '6', c 'x')
    "Matrix", paren(matrix [ [ c 'a'; c 'b' ]; [ c 'c'; c 'd' ] ])
    "BMartix",
    square(
        matrix
            [ [ sub(c 'x', s "11"); sub(c 'x', s "12"); c '.'; c '.'; sub(c 'x', s "1n") ] ])
    "Overline", overline(s "Overline")
    "Underline", underline(s "Underline")
    "SimpleShortProof",
    grid(
        [
            [ row [ c '∵'; c 'x'; c '+'; c '3'; op Operator.Equals; c '5' ] ]
            [ row [ c '∴'; c 'x'; op Operator.Equals; c '2' ] ]
        ],
        [ Alignment.Left ])
    "Taylor",
    grid(
        [ [ sup(c 'e', c 'x')
            op Operator.Equals
            row [
                bigOpSubSup(BigOperator.Sum, row [ c 'n'; op Operator.Equals; c '0' ], c '∞')
                frac(sup(c 'x', c 'n'), row [ c 'n'; fn MathFunction.Fact ])
            ] ] ],
        [ Alignment.Right; Alignment.Centre; Alignment.Left ])
    "VectorProjection",
    row [
        s "Pro"
        sub(c 'j', acc(Accent.Vec, c 'v'))
        acc(Accent.Vec, c 'u')
        op Operator.Equals
        bars(acc(Accent.Vec, c 'u'))
        fn MathFunction.Cos
        c 'θ'
    ]

    "ModeMathAbsolute", row [ bars(row [ c 'x'; c '-'; c '1' ]); op Operator.Equals; c '3' ]
    "ModeMathColoured",
    row [
        coloured(Color.Crimson, MA.String "3x")
        op Operator.Plus
        coloured(Color.SeaGreen, frac(c '1', c '2'))
        op Operator.Equals
        c '5'
    ]
    "ModeMathOverbrace",
    sup(spanned(Spanning.Overbrace, row [ c 'a'; c '+'; c 'b'; c '+'; c 'c' ]), c 'n')
    "ModeMathSpacing",
    row [
        c 'a'
        space Space.NegativeThin
        c 'b'
        space Space.Thin
        c 'c'
        space Space.Medium
        c 'd'
        space Space.Thick
        c 'e'
        space Space.Quad
        c 'f'
        space Space.QQuad
        c 'g'
    ]
    "ModeMathVectorArrow",
    row [ spanned(Spanning.Overrightarrow, s "AB"); op Operator.Equals; acc(Accent.Vec, MA.BoldVar 'v') ]
    "ModeMathWideTilde", acc(Accent.WideTilde, s "xyz")
    "ModeMathAccents",
    row [
        acc(Accent.Hat, c 'a')
        acc(Accent.Tilde, c 'b')
        acc(Accent.Bar, c 'c')
        acc(Accent.Vec, c 'd')
        acc(Accent.Dot, c 'e')
        acc(Accent.DoubleDot, c 'f')
        acc(Accent.Check, c 'g')
        acc(Accent.Acute, c 'h')
        acc(Accent.Grave, c 'i')
        acc(Accent.Breve, c 'j')
    ]
    "ModeMathBlackboard",
    row [ c 'ℕ'; c '⊂'; c 'ℤ'; c '⊂'; c 'ℚ'; c '⊂'; c 'ℝ'; c '⊂'; c 'ℂ'; c '⊂'; bb 'O' ]
    "ModeMathCases",
    row [
        bars(c 'x')
        op Operator.Equals
        cases [
            [ row [ c '-'; c 'x' ]; row [ c 'x'; c '<'; c '0' ] ]
            [ c 'x'; row [ c 'x'; c '≥'; c '0' ] ]
        ]
    ]
    "ModeMathEvaluatedAt", evaluatedAt(frac(sup(c 'x', c '2'), c '2'), c '1', c '2')
    "ModeMathBigOperators",
    row [
        bigOpSubSup(BigOperator.Product, row [ c 'k'; op Operator.Equals; c '1' ], c 'n')
        bigOp BigOperator.Coproduct
        bigOpSub(BigOperator.Intersection, c 'i')
        bigOpSubSup(BigOperator.ContourIntegral, c '0', c 'Δ')
    ]
    "ModeMathBoldVectors",
    row [ MA.BoldVar 'v'; op Operator.Equals; MA.BoldVar 'a'; MA.Cdot; MA.BoldVar 'b' ]
    "ModeMathBrackets", row [ paren(c 'a'); square(c 'b'); curly(c 'c'); bars(c 'd') ]
    "ModeMathHalfOpenInterval", pair(Bracket.Square, Bracket.Normal, row [ c '0'; c ','; c '1' ])
    "ModeMathCubeRoot", root(c '3', row [ c 'x'; c '+'; c '1' ])
    "ModeMathDerivative", frac(row [ MA.UprightD; c 'y' ], row [ MA.UprightD; c 'x' ])
    "ModeMathTentativeBracket", MA.Bracketed(Brackets.Matching Bracket.Normal, s "x+1", BracketCompletion.Left)
    "ModeMathEmptySlots", frac(MA.Empty, MA.Empty)
]

/// The same, with the cursor somewhere in each. Rows and fractions only until the spike is finished.
let cursored =
    let formula = (row [ c 'a'; frac(s "b+1", c 'c'); c 'd' ]).Flatten
    let positions = MACurs.Positions formula |> List.ofSeq
    let slot = (frac(MA.Empty, c 'c')).Flatten
    [
        "CursorBeforeAFraction", positions.[1]
        "CursorAfterALetterInTheNumerator", positions.[3]
        "CursorInAnEmptySlot", (MACurs.Positions slot |> Seq.item 1)
    ]

/// Examples from the same folder that MA cannot express yet, with the roadmap item each waits on.
let unimplemented = [
    "AccentUnder", @"\threeunderdot{x}", "accents below"
    "AccentUnderThin", @"\threeunderdot{i}", "accents below"
    "Cyrillic", @"А а\ Б б\ В в", "Cyrillic in the font"
    "FontStyles", @"\mathnormal F\mathrm F\mathbf F\mathcal F\mathtt F", "Styled"
    "ItalicAlignment", @"\colorbox{yellow}P\\\begin{array}{r}\colorbox{yellow}{PF}\end{array}", "colorbox"
    "LineStyles", @"a \displaystyle a \textstyle a \scriptstyle a \scriptscriptstyle a", "style commands"
    "Matrixception", @"\begin{Vmatrix}\begin{vmatrix}a&b\end{vmatrix}\end{Vmatrix}", "double bar delimiter"
    "RaiseBox", @"a\raisebox{1mu}a\raisebox{2mu}a", "raisebox"
    "SomeLimit", @"\lim_{x\to\infty}\frac{e^2}{1-x}=\limsup_{\sigma}5", "limsup"
]
