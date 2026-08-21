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
        |> ValueOption.orElseWith (fun () -> Letters.letterlikeBlackboard c)
        |> ValueOption.orElseWith (fun () ->
            if c = '-' then ValueSome Operators.minus else MathFont.OfChar c)

    let boldVariable(c: char) = Letters.bold c

    /// Function names are set upright, though two are the indicator's 1 and the factorial's !.
    let uprightGlyph(c: char) =
        Letters.upright c
        |> ValueOption.orElseWith (fun () -> Digits.glyph c)
        |> ValueOption.orElseWith (fun () -> MathFont.OfChar c)

    let blackboardVariable(c: char) = Letters.blackboard c

    /// ValueNone where the side carries no bracket at all.
    let leftDelimiter(bracket: Bracket) =
        match bracket with
        | Bracket.Normal -> ValueSome Delimiters.roundLeft
        | Bracket.Square -> ValueSome Delimiters.squareLeft
        | Bracket.Curly -> ValueSome Delimiters.curlyLeft
        | Bracket.Angle -> ValueSome Delimiters.angleLeft
        | Bracket.Line -> ValueSome Delimiters.bar
        | Bracket.None -> ValueNone

    /// ValueNone where the side carries no bracket at all.
    let rightDelimiter(bracket: Bracket) =
        match bracket with
        | Bracket.Normal -> ValueSome Delimiters.roundRight
        | Bracket.Square -> ValueSome Delimiters.squareRight
        | Bracket.Curly -> ValueSome Delimiters.curlyRight
        | Bracket.Angle -> ValueSome Delimiters.angleRight
        | Bracket.Line -> ValueSome Delimiters.bar
        | Bracket.None -> ValueNone

    let accent(accent: Accent) =
        match accent with
        | Accent.Hat -> Accents.hat
        | Accent.Tilde -> Accents.tilde
        | Accent.Bar -> Accents.bar
        | Accent.Vec -> Accents.vec
        | Accent.Dot -> Accents.dot
        | Accent.DoubleDot -> Accents.doubleDot
        | Accent.Check -> Accents.check
        | Accent.Acute -> Accents.acute
        | Accent.Grave -> Accents.grave
        | Accent.Breve -> Accents.breve

    /// Eighteenths of an em between a table's columns, which is what TeX sets a matrix with.
    let tableColumnGap = 18

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
        | MA.Frac _ | MA.Stack _ | MA.Table _ -> both AtomClass.Inner
        | MA.Bracketed _ -> struct (AtomClass.Open, AtomClass.Close)
        | MA.ScriptSuper(main, _, _) | MA.ScriptSub(main, _) -> atomClasses main
        | MA.BigOp _ -> both AtomClass.Operator
        | MA.Row _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD | MA.RootN _ | MA.Sqrt _
        | MA.Accented _ | MA.Overline _ | MA.Underline _ -> both AtomClass.Ordinary

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
    let pointSize(style: Style) = fontSize * style.ScaleFactor

    /// An atom of the given width, reaching as far as the parts it draws.
    let atomOf(pma: PlacedMA, width: float32, italicCorrection: float32) =
        let parts = pma.Parts
        Placed(pma, parts, Extent.OfParts(width, italicCorrection, parts), 0f, 0f)

    /// An atom reaching beyond what it draws, as the font asks above a bar and below an underbar.
    let paddedAtom(pma: PlacedMA, width: float32, ascender: float32, descender: float32) =
        let parts = pma.Parts
        let body = Extent.OfParts(width, 0f, parts)
        Placed(pma, parts, Extent(width, body.Ascent + ascender, body.Descent + descender, 0f), 0f, 0f)

    let markOf(glyphs: ImmutableArray<PlacedGlyph>, width: float32) =
        let ascent = ImmArray.maxWithSafe(glyphs, 0f, fun g -> g.Top)
        let descent = -ImmArray.minWithSafe(glyphs, 0f, fun g -> g.Bottom)
        PlacedGlyphs(glyphs, Extent(width, ascent, descent, 0f))

    /// A gap where a glyph should be would be wrong output, so a missing one is an error.
    let required(glyph: Glyph voption, what: string) =
        match glyph with
        | ValueSome found -> found
        | ValueNone -> failwith $"the font cannot draw {what}"

    /// The atom reaches past the advance by the glyph lean, which is what its scripts are placed by.
    let single(glyph: Glyph, style: Style, make: PlacedGlyph -> PlacedMA) =
        let s = scale style
        let pma = make(PlacedGlyph(glyph, pointSize style, 0f, 0f))
        atomOf(pma, float32 (glyph.Advance + glyph.ItalicCorrection) * s, float32 glyph.ItalicCorrection * s)

    /// A large operator's italic correction measures its lean rather than ink past its advance, so it
    /// does not widen the mark, though scripts and limits are still placed by it.
    let operatorMark(glyph: Glyph, style: Style) =
        let s = scale style
        PlacedGlyphs(
            ImmutableArray.Create(PlacedGlyph(glyph, pointSize style, 0f, 0f)),
            Extent(
                float32 glyph.Advance * s,
                float32 glyph.Top * s,
                -(float32 glyph.Bottom) * s,
                float32 glyph.ItalicCorrection * s))

    /// A glyph with its ink resting on the origin, so that callers place it by its bottom.
    let bottomAnchored(glyph: Glyph, style: Style) =
        let s = scale style
        let placed = PlacedGlyph(glyph, pointSize style, 0f, -(float32 glyph.Bottom) * s)
        markOf(ImmutableArray.Create placed, float32 glyph.Advance * s)

    /// Upright letters, as function names are set: one mark, so no italic correction trails them.
    let upright(text: string, style: Style) =
        let s = scale style
        let glyphs = ImmutableArray.CreateBuilder<PlacedGlyph>()
        let mutable x = 0f
        for c in text do
            let glyph = required(Conventions.uprightGlyph c, $"the character {c}")
            glyphs.Add(PlacedGlyph(glyph, pointSize style, x, 0f))
            x <- x + float32 glyph.Advance * s
        markOf(glyphs.ToImmutable(), x)

    let spacing(left: AtomClass, right: AtomClass, style: Style) =
        let entry = Spacing.table.[int left * 8 + int right]
        let eighteenths =
            if entry >= 0 then entry
            elif style.Size = MathSize.Display || style.Size = MathSize.Text then -entry
            else 0
        float32 eighteenths * fontSize * style.ScaleFactor / 18f

    let extentOf(placed: Placed voption) = placed |> ValueOption.map (fun p -> p.Extent)

    /// How far the scripts beside a base sit above and below it, and how wide the three come to.
    let scriptPlacement(b: Extent, above: Extent voption, below: Extent voption, style: Style) =
        let s = scale style
        let mutable width = b.Width
        let mutable up = 0f
        let mutable down = 0f
        match above with
        | ValueSome extent ->
            let start =
                float32 (
                    if style.Cramped then MathConstants.SuperscriptShiftUpCramped
                    else MathConstants.SuperscriptShiftUp)
                * s
            up <-
                max
                    (max start (b.Ascent - float32 MathConstants.SuperscriptBaselineDropMax * s))
                    (extent.Descent + float32 MathConstants.SuperscriptBottomMin * s)
        | ValueNone -> ()
        match below with
        | ValueSome extent ->
            down <-
                max
                    (max
                        (float32 MathConstants.SubscriptShiftDown * s)
                        (b.Descent + float32 MathConstants.SubscriptBaselineDropMin * s))
                    (extent.Ascent - float32 MathConstants.SubscriptTopMax * s)
        | ValueNone -> ()
        match above, below with
        | ValueSome over, ValueSome under ->
            let gapMin = float32 MathConstants.SubSuperscriptGapMin * s
            let gap = (up - over.Descent) - (under.Ascent - down)
            if gap < gapMin then
                down <- down + gapMin - gap
                let shortfall =
                    float32 MathConstants.SuperscriptBottomMaxWithSubscript * s - (up - over.Descent)
                if shortfall > 0f then
                    up <- up + shortfall
                    down <- down - shortfall
        | ValueNone, _ | _, ValueNone -> ()
        match above with
        | ValueSome extent -> width <- max width (b.Width + extent.Width)
        | ValueNone -> ()
        // The italic correction leans the base right, so the subscript steps back over it.
        let subscriptX = b.Width - b.ItalicCorrection
        match below with
        | ValueSome extent -> width <- max width (subscriptX + extent.Width)
        | ValueNone -> ()
        // Only a script is followed by the space after a script; a bare operator is not.
        let after =
            match above, below with
            | ValueNone, ValueNone -> 0f
            | ValueSome _, _ | _, ValueSome _ -> float32 MathConstants.SpaceAfterScript * s
        struct (up, down, subscriptX, width + after)

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
        let mutable reach = 0f
        let mutable italicCorrection = 0f
        for i in 0 .. elements.Length - 1 do
            if i > 0 then x <- x + spacing(facingRight (i - 1), facingLeft i, style)
            let child = t.Of(elements.[i], style)
            children.Add(child.At(x, 0f))
            // A lean is ink above the baseline, which the next atom sets under rather than after.
            x <- x + child.Width - child.ItalicCorrection
            reach <- x + child.ItalicCorrection
            italicCorrection <- child.ItalicCorrection
        atomOf(PlacedMA.Row(children.ToImmutable()), reach, italicCorrection)

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
        let pma =
            PlacedMA.Frac(
                n.At((width - n.Width) / 2f, up),
                PlacedRule(width, thickness, 0f, ruleBottom),
                d.At((width - d.Width) / 2f, -down))
        atomOf(pma, width, 0f)

    /// The base with its scripts placed, which both script atoms share.
    member private t.ScriptSuper(main: MA, super: MA, sub: MA voption, style: Style) =
        let b = t.Of(main, style)
        let above = t.Of(super, style.Superscript)
        let below = sub |> ValueOption.map (fun ma -> t.Of(ma, style.Subscript))
        let struct (up, down, subscriptX, width) =
            scriptPlacement(b.Extent, ValueSome above.Extent, extentOf below, style)
        let placedBelow = below |> ValueOption.map (fun p -> p.At(subscriptX, -down))
        atomOf(PlacedMA.ScriptSuper(b, above.At(b.Width, up), placedBelow), width, 0f)

    member private t.ScriptSub(main: MA, sub: MA, style: Style) =
        let b = t.Of(main, style)
        let below = t.Of(sub, style.Subscript)
        let struct (_, down, subscriptX, width) =
            scriptPlacement(b.Extent, ValueNone, ValueSome below.Extent, style)
        atomOf(PlacedMA.ScriptSub(b, below.At(subscriptX, -down)), width, 0f)

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
        let big(stretchy: StretchyGlyph) = operatorMark(t.BigOperatorGlyph(stretchy, style), style)
        match op with
        | BigOperator.Sum -> big BigOperators.sum
        | BigOperator.Product -> big BigOperators.product
        | BigOperator.Coproduct -> big BigOperators.coproduct
        | BigOperator.Integral -> big BigOperators.integral
        | BigOperator.ContourIntegral -> big BigOperators.contourIntegral
        | BigOperator.Union -> big BigOperators.union
        | BigOperator.Intersection -> big BigOperators.intersection
        | BigOperator.Limit -> upright("lim", style)

    member private t.BigOp(op: BigOperator, lower: MA voption, upper: MA voption, style: Style) =
        let operator = t.BigOperator(op, style)
        let above = upper |> ValueOption.map (fun ma -> t.Of(ma, style.Superscript))
        let below = lower |> ValueOption.map (fun ma -> t.Of(ma, style.Subscript))
        if style.IsDisplay && Conventions.takesLimits op then t.Limits(op, operator, below, above, style)
        else
            let struct (up, down, subscriptX, width) =
                scriptPlacement(operator.Extent, extentOf above, extentOf below, style)
            let pma =
                PlacedMA.BigOp(
                    op,
                    operator,
                    below |> ValueOption.map (fun p -> p.At(subscriptX, -down)),
                    above |> ValueOption.map (fun p -> p.At(operator.Width, up)))
            atomOf(pma, width, 0f)

    /// Limits above and below, centred on the operator and each nudged by half its italic correction.
    member private _.Limits
        (op: BigOperator, operator: PlacedGlyphs, below: Placed voption, above: Placed voption, style: Style) =
        let s = scale style
        let half = operator.ItalicCorrection / 2f
        let centre = operator.Width / 2f
        let upper =
            above
            |> ValueOption.map (fun placed ->
                let rise =
                    max
                        (float32 MathConstants.UpperLimitBaselineRiseMin * s)
                        (float32 MathConstants.UpperLimitGapMin * s + placed.Descent)
                placed.At(centre + half - placed.Width / 2f, operator.Ascent + rise))
        let lower =
            below
            |> ValueOption.map (fun placed ->
                let drop =
                    max
                        (float32 MathConstants.LowerLimitBaselineDropMin * s)
                        (float32 MathConstants.LowerLimitGapMin * s + placed.Ascent)
                placed.At(centre - half - placed.Width / 2f, -(operator.Descent + drop)))
        // A limit wider than the operator overhangs on both sides, so the whole atom shifts right.
        let leftmost(placed: Placed voption) =
            match placed with
            | ValueSome found -> min 0f found.X
            | ValueNone -> 0f
        let left = min (leftmost upper) (leftmost lower)
        let shift(placed: Placed voption) = placed |> ValueOption.map (fun p -> p.At(p.X - left, p.Y))
        let moved = operator.At(-left, 0f)
        let upper = shift upper
        let lower = shift lower
        let far(placed: Placed voption) =
            match placed with
            | ValueSome found -> found.X + found.Width
            | ValueNone -> 0f
        let width = max (operator.Width - left) (max (far upper) (far lower))
        atomOf(PlacedMA.BigOp(op, moved, lower, upper), width, 0f)

    /// The glyph grown to at least the given height, laid out resting on the origin.
    member private t.Stretched(stretchy: StretchyGlyph, style: Style, minHeight: float32) =
        let s = scale style
        let mutable chosen = ValueNone
        let mutable i = 0
        while chosen.IsNone && i < stretchy.SizeCount do
            let size = stretchy.Size i
            if float32 size.Advance * s >= minHeight then chosen <- ValueSome size.Glyph
            i <- i + 1
        match chosen with
        | ValueSome size -> bottomAnchored(size, style)
        | ValueNone when stretchy.PartCount > 0 -> t.Assembly(stretchy, style, minHeight)
        | ValueNone when stretchy.SizeCount > 0 ->
            bottomAnchored(stretchy.Size(stretchy.SizeCount - 1).Glyph, style)
        | ValueNone -> bottomAnchored(stretchy.Glyph, style)

    member private _.Assembly(stretchy: StretchyGlyph, style: Style, minHeight: float32) =
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
        let glyphs = ImmutableArray.CreateBuilder<PlacedGlyph>()
        let mutable y = 0f
        let mutable width = 0f
        for part in items do
            let glyph = part.Glyph
            glyphs.Add(PlacedGlyph(glyph, pointSize style, 0f, y - (float32 glyph.Bottom) * s))
            width <- max width (float32 glyph.Advance * s)
            y <- y + float32 part.FullAdvance * s - overlap
        markOf(glyphs.ToImmutable(), width)

    /// One side of a bracketed formula, which \left. leaves out altogether.
    member private t.Delimiter(delimiter: StretchyGlyph voption, style: Style, minHeight: float32) =
        match delimiter with
        | ValueSome stretchy -> t.Stretched(stretchy, style, minHeight)
        | ValueNone -> PlacedGlyphs(ImmutableArray.Empty, Extent(0f, 0f, 0f, 0f))

    member private t.Brackets(brackets: Brackets, inner: MA, completion: BracketCompletion, style: Style) =
        let content = t.Of(inner, style)
        let s = scale style
        let axis = float32 MathConstants.AxisHeight * s
        let reach = 2f * max (content.Ascent - axis) (content.Descent + axis)
        // TeX lets a delimiter fall a little short rather than jump to the next size up.
        let needed = max (reach * 0.901f) (reach - 0.5f * fontSize * style.ScaleFactor)
        let left = t.Delimiter(Conventions.leftDelimiter brackets.Left, style, needed)
        let right = t.Delimiter(Conventions.rightDelimiter brackets.Right, style, needed)
        let onAxis(mark: PlacedGlyphs, x: float32) = mark.At(x, axis - mark.Ascent / 2f)
        let contentX = left.Width
        let rightX = contentX + content.Width
        let pma =
            PlacedMA.Bracketed(
                brackets,
                onAxis(left, 0f),
                content.At(contentX, 0f),
                onAxis(right, rightX),
                completion)
        atomOf(pma, rightX + right.Width, 0f)

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
        let surd = t.Stretched(Radicals.surd, style, needed)
        // A surd taller than needed hangs half its surplus below and lifts the rule by the other half.
        let clearance = gap + max 0f (surd.Ascent - needed) / 2f
        let ruleTop = x.Ascent + clearance + thickness
        let bottom = ruleTop - surd.Ascent
        let index = degree |> ValueOption.map (fun ma -> t.Of(ma, Style(MathSize.ScriptScript, style.Cramped)))
        let before = float32 MathConstants.RadicalKernBeforeDegree * s
        let after = float32 MathConstants.RadicalKernAfterDegree * s
        let indexWidth =
            match index with
            | ValueSome placed -> placed.Width
            | ValueNone -> 0f
        let surdX =
            match index with
            | ValueNone -> 0f
            | ValueSome _ -> max 0f (before + indexWidth + after)
        let placedSurd = surd.At(surdX, bottom)
        let barX = surdX + surd.Width
        let bar = PlacedRule(x.Width, thickness, barX, ruleTop - thickness)
        let radicand = x.At(barX, 0f)
        let pma =
            match index with
            | ValueSome placed ->
                let raise = float32 MathConstants.RadicalDegreeBottomRaisePercent / 100f * surd.Ascent
                let degreeY = bottom + raise + placed.Descent
                PlacedMA.RootN(placed.At(surdX - indexWidth - after, degreeY), placedSurd, bar, radicand)
            | ValueNone -> PlacedMA.Sqrt(placedSurd, bar, radicand)
        paddedAtom(pma, barX + x.Width, float32 MathConstants.RadicalExtraAscender * s, 0f)

    /// Where an accent sits over an atom: its glyph's attachment, or the middle of one drawn from more.
    member private _.Attachment(placed: Placed, style: Style) =
        match placed.Pma.SingleGlyph with
        | ValueSome glyph -> glyph.X + float32 glyph.Glyph.TopAccentAttachment * scale style
        | ValueNone -> placed.Width / 2f

    /// The accent rises clear of a base taller than the height the font draws its accents for.
    member private t.Accented(accent: Accent, x: MA, style: Style) =
        let b = t.Of(x, style.Cramp)
        let s = scale style
        let glyph = Conventions.accent accent
        let mark =
            PlacedGlyph(
                glyph,
                pointSize style,
                t.Attachment(b, style) - float32 glyph.TopAccentAttachment * s,
                max 0f (b.Ascent - float32 MathConstants.AccentBaseHeight * s))
        atomOf(PlacedMA.Accented(accent, mark, b), b.Width, b.ItalicCorrection)

    /// A rule over the atom, clear of its ink by the gap the font names.
    member private t.Overline(x: MA, style: Style) =
        let b = t.Of(x, style.Cramp)
        let s = scale style
        let thickness = float32 MathConstants.OverbarRuleThickness * s
        let gap = float32 MathConstants.OverbarVerticalGap * s
        let pma = PlacedMA.Overline(PlacedRule(b.Width, thickness, 0f, b.Ascent + gap), b)
        paddedAtom(pma, b.Width, float32 MathConstants.OverbarExtraAscender * s, 0f)

    /// A rule under the atom, clear of its ink by the gap the font names.
    member private t.Underline(x: MA, style: Style) =
        let b = t.Of(x, style)
        let s = scale style
        let thickness = float32 MathConstants.UnderbarRuleThickness * s
        let gap = float32 MathConstants.UnderbarVerticalGap * s
        let pma = PlacedMA.Underline(b, PlacedRule(b.Width, thickness, 0f, -(b.Descent + gap + thickness)))
        paddedAtom(pma, b.Width, 0f, float32 MathConstants.UnderbarExtraDescender * s)

    /// A fraction with no rule, where only the gap keeps the two apart.
    member private t.Stack(top: MA, bottom: MA, style: Style) =
        let above = t.Of(top, style.Numerator)
        let below = t.Of(bottom, style.Denominator)
        let s = scale style
        let value(displayStyle: int, textStyle: int) =
            float32 (if style.IsDisplay then displayStyle else textStyle) * s
        let mutable up = value(MathConstants.StackTopDisplayStyleShiftUp, MathConstants.StackTopShiftUp)
        let mutable down =
            value(MathConstants.StackBottomDisplayStyleShiftDown, MathConstants.StackBottomShiftDown)
        let gapMin = value(MathConstants.StackDisplayStyleGapMin, MathConstants.StackGapMin)
        let gap = (up - above.Descent) - (below.Ascent - down)
        if gap < gapMin then
            up <- up + (gapMin - gap) / 2f
            down <- down + (gapMin - gap) / 2f
        let width = max above.Width below.Width
        let pma =
            PlacedMA.Stack(
                above.At((width - above.Width) / 2f, up),
                below.At((width - below.Width) / 2f, -down))
        atomOf(pma, width, 0f)

    /// A grid centred on the axis, its rows a line's leading apart and its columns an em apart.
    member private t.Table(cells: ImmA2D<MA>, alignments: ImmutableArray<Alignment>, style: Style) =
        let s = scale style
        let placed = cells |> ImmA2D.map (fun cell -> t.Of(cell, style))
        /// How far the tallest, deepest or widest cell of a row reaches.
        let furthest(row: int, reach: Placed -> float32) =
            let mutable value = 0f
            for col in 0 .. placed.Cols - 1 do
                value <- max value (reach placed.[row, col])
            value
        let widths = Array.zeroCreate<float32> placed.Cols
        for row in 0 .. placed.Rows - 1 do
            for col in 0 .. placed.Cols - 1 do
                widths.[col] <- max widths.[col] placed.[row, col].Width
        let gap = float32 Conventions.tableColumnGap * fontSize * style.ScaleFactor / 18f
        let lefts = Array.zeroCreate<float32> placed.Cols
        let mutable right = 0f
        for col in 0 .. placed.Cols - 1 do
            lefts.[col] <- right
            right <- right + widths.[col] + gap
        let width = max 0f (right - gap)
        let leading = float32 MathConstants.MathLeading * s
        let baselines = Array.zeroCreate<float32> placed.Rows
        let mutable y = 0f
        for row in 0 .. placed.Rows - 1 do
            if row > 0 then y <- y - furthest(row - 1, fun cell -> cell.Descent) - leading
            y <- y - furthest(row, fun cell -> cell.Ascent)
            baselines.[row] <- y
        let height =
            if placed.Rows = 0 then 0f else furthest(placed.Rows - 1, fun cell -> cell.Descent) - y
        // The grid is centred on the axis, as a fraction of the same height would be.
        let rise = height / 2f + float32 MathConstants.AxisHeight * s
        let laid =
            placed
            |> ImmA2D.mapi (fun row col cell ->
                let offset =
                    match MA.AlignmentOf(alignments, col) with
                    | Alignment.Centre -> (widths.[col] - cell.Width) / 2f
                    | Alignment.Left -> 0f
                    | Alignment.Right -> widths.[col] - cell.Width
                cell.At(lefts.[col] + offset, baselines.[row] + rise))
        atomOf(PlacedMA.Table(laid, alignments), width, 0f)

    member private t.Of(ma: MA, style: Style): Placed =
        match ma with
        | MA.Row elements -> t.Row(elements, style)
        | MA.Char c ->
            single(required(Conventions.variable c, $"the character {c}"), style, fun g -> PlacedMA.Char(c, g))
        | MA.BoldVar c ->
            single(
                required(Conventions.boldVariable c, $"a bold {c}"),
                style,
                fun g -> PlacedMA.BoldVar(c, g))
        | MA.Blackboard c ->
            single(
                required(Conventions.blackboardVariable c, $"a blackboard bold {c}"),
                style,
                fun g -> PlacedMA.Blackboard(c, g))
        | MA.Cdot -> single(Operators.cdot, style, PlacedMA.Cdot)
        | MA.UprightD -> single(Symbols.uprightD, style, PlacedMA.UprightD)
        | MA.Function f ->
            let letters = upright(Conventions.functionName f, style)
            atomOf(PlacedMA.Function(f, letters), letters.Width, 0f)
        | MA.Operator o -> single(Conventions.operator o, style, fun g -> PlacedMA.Operator(o, g))
        | MA.Frac(numerator, denominator) -> t.Fraction(numerator, denominator, style)
        | MA.ScriptSuper(main, super, sub) -> t.ScriptSuper(main, super, sub, style)
        | MA.ScriptSub(main, sub) -> t.ScriptSub(main, sub, style)
        | MA.BigOp(op, lower, upper) -> t.BigOp(op, lower, upper, style)
        | MA.Bracketed(brackets, inner, completion) -> t.Brackets(brackets, inner, completion, style)
        | MA.Sqrt x -> t.Radical(ValueNone, x, style)
        | MA.RootN(n, x) -> t.Radical(ValueSome n, x, style)
        | MA.Accented(accent, x) -> t.Accented(accent, x, style)
        | MA.Overline x -> t.Overline(x, style)
        | MA.Underline x -> t.Underline(x, style)
        | MA.Stack(top, bottom) -> t.Stack(top, bottom, style)
        | MA.Table(cells, alignments) -> t.Table(cells, alignments, style)

    /// Laid out on a line of its own, where fractions and radicals are given their full height.
    member t.Of(ma: MA) = t.Of(ma, MathSize.Display)

    member t.Of(ma: MA, size: MathSize) = t.Of(ma.Flatten, Style(size, false))
