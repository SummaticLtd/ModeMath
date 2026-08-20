namespace ModeMath

open System.Collections.Immutable
open FSUtils

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
        | MathSize.Display | MathSize.Text -> 1f
        | MathSize.Script -> float32 MathConstants.ScriptPercentScaleDown / 100f
        | MathSize.ScriptScript -> float32 MathConstants.ScriptScriptPercentScaleDown / 100f

    member _.Cramp = Style(size, true)

    member _.Superscript =
        let smaller =
            match size with
            | MathSize.Display | MathSize.Text -> MathSize.Script
            | MathSize.Script | MathSize.ScriptScript -> MathSize.ScriptScript
        Style(smaller, cramped)

    member t.Subscript = t.Superscript.Cramp

    member _.Numerator =
        let smaller =
            match size with
            | MathSize.Display -> MathSize.Text
            | MathSize.Text -> MathSize.Script
            | MathSize.Script | MathSize.ScriptScript -> MathSize.ScriptScript
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

module internal Conventions =
    let relations = set [
        '='; '<'; '>'; '≤'; '≥'; '≠'; '≈'; '≡'; '∈'; '∉'
        '⊂'; '⊆'; '→'; '⇒'; '⇔'; '⟺'; '∴'; '∵'
        '∼'; '≅'; '∝'
    ]

    let binaries = set [
        '+'; '-'; '−'; '±'; '∓'; '×'; '÷'; '⋅'; '∗'
        '∩'; '∪'; '∧'; '∨'; '∖'; '⊕'; '⊗'
    ]

    let opens = set [ '('; '['; '{'; '⟨' ]
    let closes = set [ ')'; ']'; '}'; '⟩' ]
    let punctuation = set [ ','; ';'; ':' ]

    /// Latin and small Greek variables are italic, capital Greek upright, as TeX sets them.
    let variable(c: char) =
        Letters.italic c
        |> ValueOption.orElseWith (fun () -> Letters.upright c)
        |> ValueOption.orElseWith (fun () -> Digits.glyph c)
        |> ValueOption.orElseWith (fun () ->
            if c = '-' then ValueSome Operators.minus else MathFont.OfChar c)

    let boldVariable(c: char) = Letters.bold c

    /// LaTeX keeps an integral's limits beside it; the rest take them above and below in display style.
    let takesLimits(op: BigOperator) =
        match op with
        | BigOperator.Integral | BigOperator.ContourIntegral -> false
        | BigOperator.Sum | BigOperator.Product | BigOperator.Coproduct
        | BigOperator.Union | BigOperator.Intersection | BigOperator.Limit -> true

    let operator(o: Operator) =
        match o with
        | Operator.Times -> Operators.times
        | Operator.Plus -> Operators.plus
        | Operator.Minus -> Operators.minus
        | Operator.Divide -> Operators.divide
        | Operator.Equals -> Operators.equals

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
        | MathFunction.Sign -> "sgn"

    let charClass(c: char) =
        if relations.Contains c then AtomClass.Relation
        elif binaries.Contains c then AtomClass.Binary
        elif opens.Contains c then AtomClass.Open
        elif closes.Contains c then AtomClass.Close
        elif punctuation.Contains c then AtomClass.Punctuation
        else AtomClass.Ordinary

    /// The classes an atom presents to its left and right neighbours, which differ for a bracketed group.
    let rec atomClasses(ma: MA): struct (AtomClass * AtomClass) =
        let both(atomClass: AtomClass) = struct (atomClass, atomClass)
        match ma with
        | MA.Char c -> both(charClass c)
        | MA.Operator Operator.Equals -> both AtomClass.Relation
        | MA.Operator _ | MA.Cdot -> both AtomClass.Binary
        | MA.Function MathFunction.Fact -> both AtomClass.Close
        | MA.Function _ -> both AtomClass.Operator
        | MA.Frac _ -> both AtomClass.Inner
        | MA.Bracketed _ -> struct (AtomClass.Open, AtomClass.Close)
        | MA.ScriptSuper(main, _, _) | MA.ScriptSub(main, _) -> atomClasses main
        | MA.BigOp _ -> both AtomClass.Operator
        | MA.Row _ | MA.BoldVar _ | MA.UprightD | MA.RootN _ | MA.Sqrt _ -> both AtomClass.Ordinary

module private Spacing =
    /// Eighteenths of an em by left then right class, negated where only display and text styles space.
    let table =
        [|
            0; 3; -4; -5; 0; 0; 0; -3;
            3; 3; 0; -5; 0; 0; 0; -3;
            -4; -4; 0; 0; -4; 0; 0; -4;
            -5; -5; 0; 0; -5; 0; 0; -5;
            0; 0; 0; 0; 0; 0; 0; 0;
            0; 3; -4; -5; 0; 0; 0; -3;
            -3; -3; 0; -3; -3; -3; -3; -3;
            -3; 3; -4; -5; -3; 0; -3; -3
        |]

    /// A binary operator with nothing to bind on its left is ordinary, as in a leading minus sign.
    let isUnaryPosition(previous: AtomClass voption) =
        match previous with
        | ValueNone -> true
        | ValueSome previous ->
            match previous with
            | AtomClass.Binary | AtomClass.Operator | AtomClass.Relation
            | AtomClass.Open | AtomClass.Punctuation -> true
            | AtomClass.Ordinary | AtomClass.Close | AtomClass.Inner -> false

    /// A binary operator with nothing to bind on its right is ordinary too, as in a trailing minus sign.
    let leavesNothingToBind(next: AtomClass) =
        next = AtomClass.Relation || next = AtomClass.Close || next = AtomClass.Punctuation

/// Lays out an MA at a base font size in points.
type Layout(fontSize: float32) =
    let scale(style: Style) = fontSize * style.ScaleFactor / float32 MathConstants.UnitsPerEm

    /// The italic correction trails the advance, as TeX kerns after every character it sets.
    let glyphDisplay(glyph: Glyph, style: Style, ink: Ink) =
        let s = scale style
        Display(
            float32 (glyph.Advance + glyph.ItalicCorrection) * s,
            float32 glyph.Top * s,
            -(float32 glyph.Bottom) * s,
            float32 glyph.ItalicCorrection * s,
            Content.Glyph(glyph, fontSize * style.ScaleFactor, ink))

    /// A character the font cannot draw takes no room, since there is nothing to show for it.
    let symbol(glyph: Glyph voption, style: Style, ink: Ink) =
        match glyph with
        | ValueSome found -> glyphDisplay(found, style, ink)
        | ValueNone -> Display.Empty

    /// A large operator's italic correction measures its lean rather than ink past its advance, so it
    /// does not widen the box, though scripts and limits are still placed by it.
    let operatorDisplay(glyph: Glyph, style: Style, ink: Ink) =
        let s = scale style
        Display(
            float32 glyph.Advance * s,
            float32 glyph.Top * s,
            -(float32 glyph.Bottom) * s,
            float32 glyph.ItalicCorrection * s,
            Content.Glyph(glyph, fontSize * style.ScaleFactor, ink))

    /// A glyph laid out with its ink resting on the origin, so that callers place it by its bottom.
    let bottomAnchored(glyph: Glyph, style: Style, ink: Ink) =
        let s = scale style
        let child = Placed(glyphDisplay(glyph, style, ink), 0f, -(float32 glyph.Bottom) * s)
        Display.OfChildren(float32 glyph.Advance * s, 0f, ImmutableArray.Create child)

    let spacing(left: AtomClass, right: AtomClass, style: Style) =
        let entry = Spacing.table.[int left * 8 + int right]
        let eighteenths =
            if entry >= 0 then entry
            elif style.Size = MathSize.Display || style.Size = MathSize.Text then -entry
            else 0
        float32 eighteenths * fontSize * style.ScaleFactor / 18f

    member private t.Row(elements: ImmutableArray<MA>, style: Style) =
        let classes = Array.init elements.Length (fun i -> Conventions.atomClasses elements.[i])
        let facingLeft(i: int) = let struct (left, _) = classes.[i] in left
        let facingRight(i: int) = let struct (_, right) = classes.[i] in right
        let ordinary = struct (AtomClass.Ordinary, AtomClass.Ordinary)
        // A binary atom with nothing to bind is ordinary, so each gap demotes whichever side it strands.
        for i in 0 .. classes.Length - 1 do
            let previous = if i = 0 then ValueNone else ValueSome(facingRight (i - 1))
            if facingLeft i = AtomClass.Binary && Spacing.isUnaryPosition previous then
                classes.[i] <- ordinary
            elif i > 0 && facingRight (i - 1) = AtomClass.Binary && Spacing.leavesNothingToBind(facingLeft i) then
                classes.[i - 1] <- ordinary
        // No gap follows the last atom, so a binary ending the row is stranded too.
        if classes.Length > 0 && facingRight (classes.Length - 1) = AtomClass.Binary then
            classes.[classes.Length - 1] <- ordinary
        let children = ImmutableArray.CreateBuilder<Placed>()
        let mutable x = 0f
        let mutable italicCorrection = 0f
        for i in 0 .. elements.Length - 1 do
            if i > 0 then x <- x + spacing(facingRight (i - 1), facingLeft i, style)
            let child = t.Of(elements.[i], style)
            children.Add(Placed(child, x, 0f))
            x <- x + child.Width
            italicCorrection <- child.ItalicCorrection
        Display.OfChildren(x, italicCorrection, children.ToImmutable())

    /// Upright letters, as function names are set: one box, so no italic correction trails them.
    member private t.Upright(text: string, style: Style) =
        let children = ImmutableArray.CreateBuilder<Placed>()
        let mutable x = 0f
        for c in text do
            let child = symbol(Letters.upright c, style, Ink.Solid)
            children.Add(Placed(child, x, 0f))
            x <- x + child.Width - child.ItalicCorrection
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
        t.ScriptsOn(t.Of(main, style), super, sub, style)

    member private t.ScriptsOn(b: Display, super: MA voption, sub: MA voption, style: Style) =
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
        | ValueNone, _ | _, ValueNone -> ()
        match superscript with
        | ValueSome display ->
            children.Add(Placed(display, b.Width, up))
            width <- max width (b.Width + display.Width)
        | ValueNone -> ()
        // The italic correction leans the base right, so the subscript steps back over it.
        let subscriptX = b.Width - b.ItalicCorrection
        match subscript with
        | ValueSome display ->
            children.Add(Placed(display, subscriptX, -down))
            width <- max width (subscriptX + display.Width)
        | ValueNone -> ()
        let after = float32 MathConstants.SpaceAfterScript * s
        Display.OfChildren(width + after, 0f, children.ToImmutable())

    /// Display style takes the first variant tall enough, which is how a sum grows with the formula.
    member private _.BigOperatorGlyph(stretchy: StretchyGlyph, style: Style) =
        if not style.IsDisplay then stretchy.Glyph
        else
            let mutable chosen = ValueNone
            let mutable i = 0
            while chosen.IsNone && i < stretchy.SizeCount do
                let size = stretchy.Size i
                if size.Advance >= MathConstants.DisplayOperatorMinHeight then chosen <- ValueSome size.Glyph
                i <- i + 1
            match chosen with
            | ValueSome glyph -> glyph
            | ValueNone when stretchy.SizeCount > 0 -> stretchy.Size(stretchy.SizeCount - 1).Glyph
            | ValueNone -> stretchy.Glyph

    /// The operator alone: a glyph for the sum and its kin, upright letters for lim.
    member private t.BigOperator(op: BigOperator, style: Style) =
        let big(stretchy: StretchyGlyph) =
            operatorDisplay(t.BigOperatorGlyph(stretchy, style), style, Ink.Solid)
        match op with
        | BigOperator.Sum -> big BigOperators.sum
        | BigOperator.Product -> big BigOperators.product
        | BigOperator.Coproduct -> big BigOperators.coproduct
        | BigOperator.Integral -> big BigOperators.integral
        | BigOperator.ContourIntegral -> big BigOperators.contourIntegral
        | BigOperator.Union -> big BigOperators.union
        | BigOperator.Intersection -> big BigOperators.intersection
        | BigOperator.Limit -> t.Upright("lim", style)

    member private t.BigOp(op: BigOperator, lower: MA voption, upper: MA voption, style: Style) =
        let operator = t.BigOperator(op, style)
        if style.IsDisplay && Conventions.takesLimits op then t.Limits(operator, lower, upper, style)
        else t.ScriptsOn(operator, upper, lower, style)

    /// Limits above and below, centred on the operator and each nudged by half its italic correction.
    member private t.Limits(operator: Display, lower: MA voption, upper: MA voption, style: Style) =
        let s = scale style
        let above = upper |> ValueOption.map (fun ma -> t.Of(ma, style.Superscript))
        let below = lower |> ValueOption.map (fun ma -> t.Of(ma, style.Subscript))
        let half = operator.ItalicCorrection / 2f
        let centre = operator.Width / 2f
        let placed = ImmutableArray.CreateBuilder<Placed>()
        placed.Add(Placed(operator, 0f, 0f))
        match above with
        | ValueSome display ->
            let rise =
                max
                    (float32 MathConstants.UpperLimitBaselineRiseMin * s)
                    (float32 MathConstants.UpperLimitGapMin * s + display.Descent)
            placed.Add(Placed(display, centre + half - display.Width / 2f, operator.Ascent + rise))
        | ValueNone -> ()
        match below with
        | ValueSome display ->
            let drop =
                max
                    (float32 MathConstants.LowerLimitBaselineDropMin * s)
                    (float32 MathConstants.LowerLimitGapMin * s + display.Ascent)
            placed.Add(Placed(display, centre - half - display.Width / 2f, -(operator.Descent + drop)))
        | ValueNone -> ()
        // A limit wider than the operator overhangs on both sides, so the whole row shifts right.
        let mutable left = 0f
        for child in placed do left <- min left child.X
        let children = placed.ToImmutable() |> ImmArray.map (fun c -> Placed(c.Display, c.X - left, c.Y))
        let width = ImmArray.maxWithSafe(children, 0f, fun c -> c.X + c.Display.Width)
        Display.OfChildren(width, 0f, children)

    /// The glyph grown to at least the given height, laid out resting on the origin.
    member private t.Stretched(stretchy: StretchyGlyph, style: Style, minHeight: float32, ink: Ink) =
        let s = scale style
        let mutable chosen = ValueNone
        let mutable i = 0
        while chosen.IsNone && i < stretchy.SizeCount do
            let size = stretchy.Size i
            if float32 size.Advance * s >= minHeight then chosen <- ValueSome size.Glyph
            i <- i + 1
        match chosen with
        | ValueSome size -> bottomAnchored(size, style, ink)
        | ValueNone when stretchy.PartCount > 0 -> t.Assembly(stretchy, style, minHeight, ink)
        | ValueNone when stretchy.SizeCount > 0 ->
            bottomAnchored(stretchy.Size(stretchy.SizeCount - 1).Glyph, style, ink)
        | ValueNone -> bottomAnchored(stretchy.Glyph, style, ink)

    member private _.Assembly(stretchy: StretchyGlyph, style: Style, minHeight: float32, ink: Ink) =
        let s = scale style
        let overlap = float32 MathConstants.MinConnectorOverlap * s
        let parts = Array.init stretchy.PartCount stretchy.Part
        let advance(part: AssemblyPart) = float32 part.FullAdvance * s
        let extenders = parts |> Array.filter (fun part -> part.IsExtender)
        // Each further round of extenders lengthens the assembly by this much, overlaps allowed for.
        let round = (extenders |> Array.sumBy advance) - overlap * float32 extenders.Length
        let shortest =
            (parts |> Array.filter (fun part -> not part.IsExtender) |> Array.sumBy advance)
            - overlap * float32 (parts.Length - extenders.Length - 1)
        let repeats =
            if extenders.Length = 0 || round <= 0f then 0
            else min 256 (max 0 (int (ceil ((minHeight - shortest) / round))))
        let items = ResizeArray<AssemblyPart>()
        for part in parts do
            for _ in 1 .. (if part.IsExtender then repeats else 1) do
                items.Add part
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
        let leftDelimiter, rightDelimiter =
            match bracket with
            | Bracket.Line -> Delimiters.bar, Delimiters.bar
            | Bracket.Normal -> Delimiters.roundLeft, Delimiters.roundRight
        let ink(completed: bool) = if completed then Ink.Solid else Ink.Tentative
        let left = t.Stretched(leftDelimiter, style, needed, ink completion.LeftCompleted)
        let right = t.Stretched(rightDelimiter, style, needed, ink completion.RightCompleted)
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
        let surd = t.Stretched(Radicals.surd, style, needed, Ink.Solid)
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
        | MA.Char c -> symbol(Conventions.variable c, style, Ink.Solid)
        | MA.BoldVar c -> symbol(Conventions.boldVariable c, style, Ink.Solid)
        | MA.Cdot -> symbol(ValueSome Operators.cdot, style, Ink.Solid)
        | MA.UprightD -> symbol(ValueSome Symbols.uprightD, style, Ink.Solid)
        | MA.Function f -> t.Upright(Conventions.functionName f, style)
        | MA.Operator o -> symbol(ValueSome(Conventions.operator o), style, Ink.Solid)
        | MA.Frac(numerator, denominator) -> t.Fraction(numerator, denominator, style)
        | MA.ScriptSuper(main, super, sub) -> t.Scripts(main, ValueSome super, sub, style)
        | MA.ScriptSub(main, sub) -> t.Scripts(main, ValueNone, ValueSome sub, style)
        | MA.BigOp(op, lower, upper) -> t.BigOp(op, lower, upper, style)
        | MA.Bracketed(bracket, inner, completion) -> t.Brackets(bracket, inner, completion, style)
        | MA.Sqrt x -> t.Radical(ValueNone, x, style)
        | MA.RootN(n, x) -> t.Radical(ValueSome n, x, style)

    /// Laid out on a line of its own, where fractions and radicals are given their full height.
    member t.Of(ma: MA) = t.Of(ma, MathSize.Display)

    member t.Of(ma: MA, size: MathSize) = t.Of(ma.Flatten, Style(size, false))
