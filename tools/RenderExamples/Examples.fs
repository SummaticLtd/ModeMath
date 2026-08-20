module RenderExamples.Examples

open System.Collections.Immutable
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
let private bars(inner: MA) = MA.Bracketed(Bracket.Line, inner, BracketCompletion.Completed)
let private fn(f: MathFunction) = MA.Function f
let private op(o: Operator) = MA.Operator o

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

    "ModeMathAbsolute", row [ bars(row [ c 'x'; c '-'; c '1' ]); op Operator.Equals; c '3' ]
    "ModeMathBoldVectors",
    row [ MA.BoldVar 'v'; op Operator.Equals; MA.BoldVar 'a'; MA.Cdot; MA.BoldVar 'b' ]
    "ModeMathCubeRoot", root(c '3', row [ c 'x'; c '+'; c '1' ])
    "ModeMathDerivative", frac(row [ MA.UprightD; c 'y' ], row [ MA.UprightD; c 'x' ])
    "ModeMathTentativeBracket", MA.Bracketed(Bracket.Normal, s "x+1", BracketCompletion.Left)
]

/// Examples from the same folder that MA cannot express yet, with the roadmap item each waits on.
let unimplemented = [
    "Abs", @"|x|=\begin{cases} -x, & \text{ if } x < 0 \\ x, & \text{ if } x \geq 0 \end{cases}", "tables"
    "AccentOver", @"\acute{x}", "accents"
    "AccentOverF", @"\hat{f}", "accents"
    "AccentOverMultiple", @"\widehat{ABcd}", "accents"
    "AccentUnder", @"\threeunderdot{x}", "accents"
    "AccentUnderThin", @"\threeunderdot{i}", "accents"
    "ArcsinSin", @"\arcsin(\sin x)=x\quad\mathrm{for}\quad|x|\le\frac\pi2", "spacing, Styled"
    "BMartix", @"\begin{bmatrix} x_{11}&x_{12}&.&.&x_{1n} \end{bmatrix}", "tables"
    "BraSum", @"\frac{1}{\sqrt{2^n}} \sum_{i=0}^{2^n-1} \Bra{i}", "large operators, delimiters"
    "Cases", @"w \equiv \begin{cases} 0 & \text{for}\ c = d = 0 \end{cases}", "tables"
    "Choose", @"{6 \choose x}", "fraction with no rule"
    "Color", @"\color{#000088}a\color{#0000FF}b", "Coloured"
    "Cyrillic", @"А а\ Б б\ В в", "Text"
    "EvalIntegral", @"\int_1^2 x\; dx=\left.\frac{x^2}{2}\right|_1^2", "large operators"
    "FontStyles", @"\mathnormal F\mathrm F\mathbf F\mathcal F\mathtt F", "Styled"
    "Integral", @"\int_{0}^{\infty}e^x \,dx=\oint_0^{\Delta}5\Gamma", "large operators"
    "IntegralScripts", @"\int\int\int^{\infty}\int_0\int^{\infty}_0\int", "large operators"
    "ItalicAlignment", @"\colorbox{yellow}P\\\begin{array}{r}\colorbox{yellow}{PF}\end{array}", "tables"
    "KetSum", @"\frac{1}{\sqrt{2^n}} \sum_{i=0}^{2^n-1} \Ket{i}", "large operators, delimiters"
    "LargeBra", @"\Bra{\frac{a}{2}+\frac{b}{3}}", "delimiters"
    "LargeKet", @"\Ket{\frac{a}{2}+\frac{b}{3}}", "delimiters"
    "LargerDelimiters", @"\left(\left[\left\{\square\right\}^\square\right]^\square\right)^\square",
    "delimiters"
    "LineStyles", @"a \displaystyle a \textstyle a \scriptstyle a \scriptscriptstyle a", "style commands"
    "Matrix", @"\begin{pmatrix}a & b\\ c & d\end{pmatrix}", "tables"
    "Matrixception", @"\begin{Vmatrix}\begin{vmatrix}a&b\end{vmatrix}\end{Vmatrix}", "tables"
    "Overline", @"\overline{Overline}", "overline"
    "QuarticSolutions", @"\left\{-1+\frac{-\sqrt{10+2\times\left(\frac{-25}{3}\right)}}{2}\right\}", "delimiters"
    "RaiseBox", @"a\raisebox{1mu}a\raisebox{2mu}a", "raisebox"
    "SimpleLimit", @"\lim_{x\to\infty}3=3", "large operators"
    "SimpleShortProof", @"\begin{aligned}&\because x+3=5\\&\therefore x=2\end{aligned}", "tables"
    "ShortIntegral", @"\int_0^1", "large operators"
    "SolveEquations", @"\text{Solve } \begin{cases} y=x^2-x+3 \end{cases}", "tables, Text"
    "SomeLimit", @"\lim_{x\to\infty}\frac{e^2}{1-x}=\limsup_{\sigma}5", "large operators"
    "SummationBigCup", @"234 \bigcup_1", "large operators"
    "SummationDouble", @"\sum \sum", "large operators"
    "SummationWithBigLimits", @"\sum^{4^{5^{6}}}_{i=3_{2_{1}}}i", "large operators"
    "SummationWithCup", @"\sum_{n=1}^{\infty}\frac{1+n}{1-n}=\bigcup_{1}C\cup B", "large operators"
    "SummationWithLimits", @"\sum_{n=1}^{\infty}", "large operators"
    "Taylor", @"\begin{eqnarray} e^x &=& \sum_{n=0}^{\infty}\frac{x^n}{n!} \end{eqnarray}", "tables"
    "Underbrace", @"\underbrace{abcd}", "horizontal stretch"
    "UnderbraceSubscript", @"\underbrace{abcdefghklmnopqrst} _{eee}", "horizontal stretch"
    "Underline", @"\underline{Underline}", "underline"
    "VectorProjection", @"Proj_\vec{v}\vec{u}=|\vec u|\cos\theta", "accents"
]
