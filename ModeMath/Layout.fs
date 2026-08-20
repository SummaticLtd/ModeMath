namespace ModeMath

open System.Collections.Immutable

/// TeX's four sizes.
type MathSize =
    | Display = 0
    | Text = 1
    | Script = 2
    | ScriptScript = 3

/// A size, with cramping, which stops superscripts rising to make room above.
[<Struct>]
type Style(size: MathSize, cramped: bool) =
    member _.Size = size
    member _.Cramped = cramped
    member _.IsDisplay = size = MathSize.Display

    member _.ScaleFactor =
        match size with
        | MathSize.Script -> float32 MathConstants.ScriptPercentScaleDown / 100f
        | MathSize.ScriptScript -> float32 MathConstants.ScriptScriptPercentScaleDown / 100f
        | _ -> 1f

    member _.Cramp = Style(size, true)

    member _.Superscript =
        let smaller =
            match size with
            | MathSize.Display | MathSize.Text -> MathSize.Script
            | _ -> MathSize.ScriptScript
        Style(smaller, cramped)

    member t.Subscript = t.Superscript.Cramp

    member _.Numerator =
        let smaller =
            match size with
            | MathSize.Display -> MathSize.Text
            | MathSize.Text -> MathSize.Script
            | _ -> MathSize.ScriptScript
        Style(smaller, cramped)

    member t.Denominator = t.Numerator.Cramp

/// TeX's atom classes, which decide the space between neighbours in a row.
type AtomClass =
    | Ordinary = 0
    | Operator = 1
    | Binary = 2
    | Relation = 3
    | Open = 4
    | Close = 5
    | Punctuation = 6
    | Inner = 7

module private Symbols =
    let relations =
        set [ '='; '<'; '>'; '≤'; '≥'; '≠'; '≈'; '≡'; '∈'; '∉'
              '⊂'; '⊆'; '→'; '⇒'; '⇔'; '⟺'; '∴'; '∵'
              '∼'; '≅'; '∝'; '≡' ]

    let binaries =
        set [ '+'; '-'; '−'; '±'; '∓'; '×'; '÷'; '⋅'; '∗'
              '∩'; '∪'; '∧'; '∨'; '∖'; '⊕'; '⊗' ]

    let opens = set [ '('; '['; '{'; '⟨' ]
    let closes = set [ ')'; ']'; '}'; '⟩' ]
    let punctuation = set [ ','; ';' ]

    /// The italic forms of the Greek letters that have a second shape.
    let private greekVariants =
        dict [ '∂', 0x1D715; 'ϵ', 0x1D716; 'ϑ', 0x1D717; 'ϰ', 0x1D718; 'ϕ', 0x1D719; 'ϱ', 0x1D71A; 'ϖ', 0x1D71B ]

    /// The codepoint a character is drawn with: letters become the math italics.
    let codepoint(c: char) =
        if c >= 'a' && c <= 'z' then (if c = 'h' then 0x210E else 0x1D44E + int c - int 'a')
        elif c >= 'A' && c <= 'Z' then 0x1D434 + int c - int 'A'
        elif c >= 'α' && c <= 'ω' then 0x1D6FC + int c - 0x03B1
        elif c = '-' then 0x2212
        else
            match greekVariants.TryGetValue c with
            | true, italic -> italic
            | _ -> int c

    /// The codepoint a bold variable is drawn with.
    let boldCodepoint(c: char) =
        if c >= 'a' && c <= 'z' then 0x1D482 + int c - int 'a'
        elif c >= 'A' && c <= 'Z' then 0x1D468 + int c - int 'A'
        else int c

    let operatorCodepoint(o: Operator) =
        match o with
        | Operator.Times -> 0x00D7
        | Operator.Plus -> int '+'
        | Operator.Minus -> 0x2212
        | Operator.Divide -> 0x00F7
        | _ -> int '='

    let functionName(f: MathFunction) =
        match f with
        | MathFunction.Sin -> "sin"
        | MathFunction.Cos -> "cos"
        | MathFunction.Tan -> "tan"
        | MathFunction.Asin -> "arcsin"
        | MathFunction.Acos -> "arccos"
        | MathFunction.Atan -> "arctan"
        | MathFunction.Sinh -> "sinh"
        | MathFunction.Cosh -> "cosh"
        | MathFunction.Tanh -> "tanh"
        | MathFunction.Exp -> "exp"
        | MathFunction.Log -> "log"
        | MathFunction.Ln -> "ln"
        | MathFunction.Fact -> "!"
        | MathFunction.Sec -> "sec"
        | MathFunction.Csc -> "csc"
        | MathFunction.Cot -> "cot"
        | MathFunction.Min -> "min"
        | MathFunction.Max -> "max"
        | MathFunction.Erf -> "erf"
        | MathFunction.Indicator -> "1"
        | MathFunction.Identity -> "id"
        | MathFunction.Real -> "Re"
        | MathFunction.Imaginary -> "Im"
        | MathFunction.Gamma -> "Γ"
        | _ -> "sgn"

    let charClass(c: char) =
        if relations.Contains c then AtomClass.Relation
        elif binaries.Contains c then AtomClass.Binary
        elif opens.Contains c then AtomClass.Open
        elif closes.Contains c then AtomClass.Close
        elif punctuation.Contains c then AtomClass.Punctuation
        else AtomClass.Ordinary

    let rec atomClass(ma: MA) =
        match ma with
        | MA.Char c -> charClass c
        | MA.Operator Operator.Equals -> AtomClass.Relation
        | MA.Operator _ | MA.Cdot -> AtomClass.Binary
        | MA.Function _ -> AtomClass.Operator
        | MA.Frac _ | MA.Bracketed _ -> AtomClass.Inner
        | MA.ScriptSuper(main, _, _) | MA.ScriptSub(main, _) -> atomClass main
        | MA.Row _ | MA.BoldVar _ | MA.UprightD | MA.RootN _ | MA.Sqrt _ -> AtomClass.Ordinary

module private Spacing =
    /// Eighteenths of an em by left then right class, negated where only display and text styles space.
    let table =
        [|
             0;  1; -2; -3;  0;  0;  0; -1;
             1;  1;  0; -3;  0;  0;  0; -1;
            -2; -2;  0;  0; -2;  0;  0; -2;
            -3; -3;  0;  0; -3;  0;  0; -3;
             0;  0;  0;  0;  0;  0;  0;  0;
             0;  1; -2; -3;  0;  0;  0; -1;
            -1; -1;  0; -1; -1; -1; -1; -1;
            -1;  1; -2; -3; -1;  0; -1; -1
        |]

    /// A binary operator with nothing to bind on its left is ordinary, as in a leading minus sign.
    let isUnaryPosition(previous: AtomClass voption) =
        match previous with
        | ValueNone -> true
        | ValueSome AtomClass.Binary
        | ValueSome AtomClass.Operator
        | ValueSome AtomClass.Relation
        | ValueSome AtomClass.Open
        | ValueSome AtomClass.Punctuation -> true
        | ValueSome _ -> false

/// Lays out an MA at a base font size in points.
type Layout(fontSize: float32) =
    let scale(style: Style) = fontSize * style.ScaleFactor / float32 MathConstants.UnitsPerEm

    let glyphDisplay(glyph: Glyph, style: Style, ink: Ink) =
        let s = scale style
        Display(
            float32 glyph.Advance * s,
            float32 glyph.Top * s,
            -(float32 glyph.Bottom) * s,
            float32 glyph.ItalicCorrection * s,
            Content.Glyph(glyph, fontSize * style.ScaleFactor, ink))

    let symbol(codepoint: int, style: Style, ink: Ink) =
        match MathFont.OfCodepoint codepoint with
        | ValueSome glyph -> glyphDisplay(glyph, style, ink)
        | ValueNone -> Display.Empty

    /// A glyph laid out with its ink resting on the origin, so that callers place it by its bottom.
    let bottomAnchored(glyph: Glyph, style: Style, ink: Ink) =
        let s = scale style
        let child = Placed(glyphDisplay(glyph, style, ink), 0f, -(float32 glyph.Bottom) * s)
        Display.OfChildren(float32 glyph.Advance * s, 0f, ImmutableArray.Create child)

    let spacing(left: AtomClass, right: AtomClass, style: Style) =
        let entry = Spacing.table.[int left * 8 + int right]
        let code =
            if entry >= 0 then entry
            elif style.Size = MathSize.Display || style.Size = MathSize.Text then -entry
            else 0
        let eighteenths =
            match code with
            | 1 -> 3
            | 2 -> 4
            | 3 -> 5
            | _ -> 0
        float32 eighteenths * fontSize * style.ScaleFactor / 18f

    member private t.Row(elements: ImmutableArray<MA>, style: Style) =
        let classes = Array.init elements.Length (fun i -> Symbols.atomClass elements.[i])
        for i in 0 .. classes.Length - 1 do
            let previous = if i = 0 then ValueNone else ValueSome classes.[i - 1]
            if classes.[i] = AtomClass.Binary && Spacing.isUnaryPosition previous then
                classes.[i] <- AtomClass.Ordinary
        let children = ImmutableArray.CreateBuilder<Placed>()
        let mutable x = 0f
        let mutable italicCorrection = 0f
        for i in 0 .. elements.Length - 1 do
            if i > 0 then x <- x + spacing(classes.[i - 1], classes.[i], style)
            let child = t.Of(elements.[i], style)
            children.Add(Placed(child, x, 0f))
            x <- x + child.Width
            italicCorrection <- child.ItalicCorrection
        Display.OfChildren(x, italicCorrection, children.ToImmutable())

    /// Upright letters, as function names are set.
    member private t.Upright(text: string, style: Style) =
        let children = ImmutableArray.CreateBuilder<Placed>()
        let mutable x = 0f
        for c in text do
            let child = symbol(int c, style, Ink.Solid)
            children.Add(Placed(child, x, 0f))
            x <- x + child.Width
        Display.OfChildren(x, 0f, children.ToImmutable())

    member private t.Fraction(numerator: MA, denominator: MA, style: Style) =
        let n = t.Of(numerator, style.Numerator)
        let d = t.Of(denominator, style.Denominator)
        let s = scale style
        let axis = float32 MathConstants.AxisHeight * s
        let thickness = float32 MathConstants.FractionRuleThickness * s
        let value(displayStyle: int, textStyle: int) =
            float32 (if style.IsDisplay then displayStyle else textStyle) * s
        let numeratorGap =
            value(MathConstants.FractionNumDisplayStyleGapMin, MathConstants.FractionNumeratorGapMin)
        let denominatorGap =
            value(MathConstants.FractionDenomDisplayStyleGapMin, MathConstants.FractionDenominatorGapMin)
        let ruleTop = axis + thickness / 2f
        let ruleBottom = axis - thickness / 2f
        let up =
            max
                (value(
                    MathConstants.FractionNumeratorDisplayStyleShiftUp,
                    MathConstants.FractionNumeratorShiftUp))
                (n.Descent + ruleTop + numeratorGap)
        let down =
            max
                (value(
                    MathConstants.FractionDenominatorDisplayStyleShiftDown,
                    MathConstants.FractionDenominatorShiftDown))
                (d.Ascent + denominatorGap - ruleBottom)
        let width = max n.Width d.Width
        let children =
            ImmutableArray.Create(
                Placed(n, (width - n.Width) / 2f, up),
                Placed(Display.OfRule(width, thickness, Ink.Solid), 0f, ruleBottom),
                Placed(d, (width - d.Width) / 2f, -down))
        Display.OfChildren(width, 0f, children)

    member private t.Scripts(main: MA, super: MA voption, sub: MA voption, style: Style) =
        let b = t.Of(main, style)
        let s = scale style
        let children = ImmutableArray.CreateBuilder<Placed>()
        children.Add(Placed(b, 0f, 0f))
        let mutable width = b.Width
        let mutable up = 0f
        let mutable down = 0f
        let superscript = super |> ValueOption.map (fun ma -> t.Of(ma, style.Superscript))
        let subscript = sub |> ValueOption.map (fun ma -> t.Of(ma, style.Subscript))
        match superscript with
        | ValueSome display ->
            let start =
                float32 (
                    if style.Cramped then MathConstants.SuperscriptShiftUpCramped
                    else MathConstants.SuperscriptShiftUp)
                * s
            up <-
                max
                    (max start (b.Ascent - float32 MathConstants.SuperscriptBaselineDropMax * s))
                    (display.Descent + float32 MathConstants.SuperscriptBottomMin * s)
        | ValueNone -> ()
        match subscript with
        | ValueSome display ->
            down <-
                max
                    (max
                        (float32 MathConstants.SubscriptShiftDown * s)
                        (b.Descent + float32 MathConstants.SubscriptBaselineDropMin * s))
                    (display.Ascent - float32 MathConstants.SubscriptTopMax * s)
        | ValueNone -> ()
        match superscript, subscript with
        | ValueSome above, ValueSome below ->
            let gapMin = float32 MathConstants.SubSuperscriptGapMin * s
            let gap = (up - above.Descent) - (below.Ascent - down)
            if gap < gapMin then
                down <- down + gapMin - gap
                let shortfall =
                    float32 MathConstants.SuperscriptBottomMaxWithSubscript * s - (up - above.Descent)
                if shortfall > 0f then
                    up <- up + shortfall
                    down <- down - shortfall
        | _ -> ()
        match superscript with
        | ValueSome display ->
            children.Add(Placed(display, b.Width + b.ItalicCorrection, up))
            width <- max width (b.Width + b.ItalicCorrection + display.Width)
        | ValueNone -> ()
        match subscript with
        | ValueSome display ->
            children.Add(Placed(display, b.Width, -down))
            width <- max width (b.Width + display.Width)
        | ValueNone -> ()
        let after = float32 MathConstants.SpaceAfterScript * s
        Display.OfChildren(width + after, 0f, children.ToImmutable())

    /// The glyph grown to at least the given height, laid out resting on the origin.
    member private t.Stretched(codepoint: int, style: Style, minHeight: float32, ink: Ink) =
        let s = scale style
        match MathFont.OfCodepoint codepoint with
        | ValueNone -> Display.Empty
        | ValueSome glyph ->
            match glyph.VerticalStretch with
            | ValueNone -> bottomAnchored(glyph, style, ink)
            | ValueSome stretch ->
                let mutable chosen = ValueNone
                let mutable i = 0
                while chosen.IsNone && i < stretch.VariantCount do
                    let variant = stretch.Variant i
                    if float32 variant.Advance * s >= minHeight then chosen <- ValueSome variant.Glyph
                    i <- i + 1
                match chosen with
                | ValueSome variant -> bottomAnchored(variant, style, ink)
                | ValueNone when stretch.PartCount > 0 -> t.Assembly(stretch, style, minHeight, ink)
                | ValueNone when stretch.VariantCount > 0 ->
                    bottomAnchored(stretch.Variant(stretch.VariantCount - 1).Glyph, style, ink)
                | ValueNone -> bottomAnchored(glyph, style, ink)

    member private _.Assembly(stretch: Stretch, style: Style, minHeight: float32, ink: Ink) =
        let s = scale style
        let overlap = float32 MathConstants.MinConnectorOverlap * s
        let parts = Array.init stretch.PartCount stretch.Part
        let extenders = parts |> Array.filter (fun part -> part.IsExtender) |> Array.length
        let sequence(repeats: int) =
            let items = ResizeArray<StretchPart>()
            for part in parts do
                for _ in 1 .. (if part.IsExtender then repeats else 1) do
                    items.Add part
            items
        let height(items: ResizeArray<StretchPart>) =
            let mutable total = 0f
            for part in items do
                total <- total + float32 part.FullAdvance * s
            total - overlap * float32 (items.Count - 1)
        let mutable repeats = if extenders = 0 then 1 else 0
        let mutable items = sequence repeats
        while extenders > 0 && height items < minHeight && repeats < 64 do
            repeats <- repeats + 1
            items <- sequence repeats
        let children = ImmutableArray.CreateBuilder<Placed>()
        let mutable y = 0f
        let mutable width = 0f
        for part in items do
            let glyph = part.Glyph
            children.Add(Placed(glyphDisplay(glyph, style, ink), 0f, y - (float32 glyph.Bottom) * s))
            width <- max width (float32 glyph.Advance * s)
            y <- y + float32 part.FullAdvance * s - overlap
        Display.OfChildren(width, 0f, children.ToImmutable())

    member private t.Brackets(bracket: Bracket, inner: MA, completion: BracketCompletion, style: Style) =
        let content = t.Of(inner, style)
        let s = scale style
        let axis = float32 MathConstants.AxisHeight * s
        let reach = 2f * max (content.Ascent - axis) (content.Descent + axis)
        // TeX lets a delimiter fall a little short rather than jump to the next size up.
        let needed = max (reach * 0.901f) (reach - 0.5f * fontSize * style.ScaleFactor)
        let leftCodepoint, rightCodepoint =
            match bracket with
            | Bracket.Line -> int '|', int '|'
            | _ -> int '(', int ')'
        let ink(completed: bool) = if completed then Ink.Solid else Ink.Tentative
        let left = t.Stretched(leftCodepoint, style, needed, ink completion.LeftCompleted)
        let right = t.Stretched(rightCodepoint, style, needed, ink completion.RightCompleted)
        let onAxis(display: Display) = axis - display.Ascent / 2f
        let children =
            ImmutableArray.Create(
                Placed(left, 0f, onAxis left),
                Placed(content, left.Width, 0f),
                Placed(right, left.Width + content.Width, onAxis right))
        Display.OfChildren(left.Width + content.Width + right.Width, 0f, children)

    member private t.Radical(degree: MA voption, radicand: MA, style: Style) =
        let x = t.Of(radicand, style.Cramp)
        let s = scale style
        let thickness = float32 MathConstants.RadicalRuleThickness * s
        let gap =
            float32 (
                if style.IsDisplay then MathConstants.RadicalDisplayStyleVerticalGap
                else MathConstants.RadicalVerticalGap)
            * s
        let needed = x.Ascent + x.Descent + gap + thickness
        let surd = t.Stretched(0x221A, style, needed, Ink.Solid)
        // A surd taller than needed hangs half its surplus below and lifts the rule by the other half.
        let clearance = gap + max 0f (surd.Ascent - needed) / 2f
        let ruleTop = x.Ascent + clearance + thickness
        let bottom = ruleTop - surd.Ascent
        let index = degree |> ValueOption.map (fun ma -> t.Of(ma, Style(MathSize.ScriptScript, style.Cramped)))
        let before = float32 MathConstants.RadicalKernBeforeDegree * s
        let after = float32 MathConstants.RadicalKernAfterDegree * s
        let indexWidth = match index with ValueSome display -> display.Width | ValueNone -> 0f
        let surdX = match index with ValueNone -> 0f | ValueSome _ -> max 0f (before + indexWidth + after)
        let children = ImmutableArray.CreateBuilder<Placed>()
        match index with
        | ValueSome display ->
            let raise = float32 MathConstants.RadicalDegreeBottomRaisePercent / 100f * surd.Ascent
            children.Add(Placed(display, surdX - indexWidth - after, bottom + raise + display.Descent))
        | ValueNone -> ()
        children.Add(Placed(surd, surdX, bottom))
        children.Add(Placed(Display.OfRule(x.Width, thickness, Ink.Solid), surdX + surd.Width, ruleTop - thickness))
        children.Add(Placed(x, surdX + surd.Width, 0f))
        let extra = float32 MathConstants.RadicalExtraAscender * s
        let body = Display.OfChildren(surdX + surd.Width + x.Width, 0f, children.ToImmutable())
        Display(body.Width, body.Ascent + extra, body.Descent, 0f, body.Content)

    member private t.Of(ma: MA, style: Style): Display =
        match ma with
        | MA.Row elements -> t.Row(elements, style)
        | MA.Char c -> symbol(Symbols.codepoint c, style, Ink.Solid)
        | MA.BoldVar c -> symbol(Symbols.boldCodepoint c, style, Ink.Solid)
        | MA.Cdot -> symbol(0x22C5, style, Ink.Solid)
        | MA.UprightD -> symbol(int 'd', style, Ink.Solid)
        | MA.Function f -> t.Upright(Symbols.functionName f, style)
        | MA.Operator o -> symbol(Symbols.operatorCodepoint o, style, Ink.Solid)
        | MA.Frac(numerator, denominator) -> t.Fraction(numerator, denominator, style)
        | MA.ScriptSuper(main, super, sub) -> t.Scripts(main, ValueSome super, sub, style)
        | MA.ScriptSub(main, sub) -> t.Scripts(main, ValueNone, ValueSome sub, style)
        | MA.Bracketed(bracket, inner, completion) -> t.Brackets(bracket, inner, completion, style)
        | MA.Sqrt x -> t.Radical(ValueNone, x, style)
        | MA.RootN(n, x) -> t.Radical(ValueSome n, x, style)

    /// Laid out on a line of its own, where fractions and radicals are given their full height.
    member t.Of(ma: MA) = t.Of(ma, MathSize.Display)

    member t.Of(ma: MA, size: MathSize) = t.Of(ma.Flatten, Style(size, false))
