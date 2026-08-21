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
        | MathSize.Script -> MathConstants.ScriptPercentScaleDown / 100f
        | MathSize.ScriptScript -> MathConstants.ScriptScriptPercentScaleDown / 100f

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

/// How an accent is drawn: from the one glyph, or from a size of one that grows to cover its base.
[<RequireQualifiedAccess>]
type internal AccentMark =
    | Fixed of Glyph
    | Wide of StretchyGlyph

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
        | Accent.Hat -> AccentMark.Fixed Accents.hat
        | Accent.Tilde -> AccentMark.Fixed Accents.tilde
        | Accent.Bar -> AccentMark.Fixed Accents.bar
        | Accent.Vec -> AccentMark.Fixed Accents.vec
        | Accent.Dot -> AccentMark.Fixed Accents.dot
        | Accent.DoubleDot -> AccentMark.Fixed Accents.doubleDot
        | Accent.Check -> AccentMark.Fixed Accents.check
        | Accent.Acute -> AccentMark.Fixed Accents.acute
        | Accent.Grave -> AccentMark.Fixed Accents.grave
        | Accent.Breve -> AccentMark.Fixed Accents.breve
        | Accent.WideHat -> AccentMark.Wide HorizontalMarks.wideHat
        | Accent.WideTilde -> AccentMark.Wide HorizontalMarks.wideTilde

    let spanning(mark: Spanning) =
        match mark with
        | Spanning.Overbrace -> HorizontalMarks.overbrace
        | Spanning.Underbrace -> HorizontalMarks.underbrace
        | Spanning.Overrightarrow -> HorizontalMarks.rightArrow

    /// Whether the mark is set under what it spans rather than over it.
    let spansBelow(mark: Spanning) =
        match mark with
        | Spanning.Underbrace -> true
        | Spanning.Overbrace | Spanning.Overrightarrow -> false

    /// Eighteenths of an em between a table's columns, which is what TeX sets a matrix with.
    let tableColumnGap = 18

    /// Eighteenths of an em a space is wide, which is how TeX measures its spacing commands.
    let space(space: Space) =
        match space with
        | Space.Thin -> 3
        | Space.Medium -> 4
        | Space.Thick -> 5
        | Space.NegativeThin -> -3
        | Space.Quad -> 18
        | Space.QQuad -> 36

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

    /// The classes an atom presents to its neighbours. ValueNone for a gap, which stands between none.
    let rec atomClasses(ma: MA): struct (AtomClass * AtomClass) voption =
        let both(atomClass: AtomClass) = ValueSome(struct (atomClass, atomClass))
        match ma with
        | MA.Char c -> both(charClass c)
        | MA.Operator Operator.Equals -> both AtomClass.Relation
        | MA.Operator _ | MA.Cdot -> both AtomClass.Binary
        | MA.Function MathFunction.Fact -> both AtomClass.Close
        | MA.Function _ -> both AtomClass.Operator
        | MA.Frac _ | MA.Stack _ | MA.Table _ -> both AtomClass.Inner
        | MA.Bracketed _ -> ValueSome(struct (AtomClass.Open, AtomClass.Close))
        | MA.ScriptSuper(main, _, _) | MA.ScriptSub(main, _) -> atomClasses main
        // A colour changes how an atom is drawn, not what it binds to on either side.
        | MA.Coloured(_, x) -> atomClasses x
        | MA.BigOp _ -> both AtomClass.Operator
        | MA.Row _ | MA.BoldVar _ | MA.Blackboard _ | MA.UprightD | MA.RootN _ | MA.Sqrt _
        | MA.Accented _ | MA.Spanned _ | MA.Overline _ | MA.Underline _ | MA.Text _ ->
            both AtomClass.Ordinary
        | MA.Space _ -> ValueNone

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


/// Lays out an MA at a base font size, which is pixels to the em.
type Layout(fontSize: float32<px>) =
    let emSize(style: Style) = fontSize * style.ScaleFactor
    let scale(style: Style) = emSize style / MathConstants.UnitsPerEm

    /// An atom of the given width, reaching as far as the parts it draws.
    let atomOf(pma: PlacedMA, width: float32<px>, italicCorrection: float32<px>, style: Style) =
        let parts = pma.Parts
        Placed(pma, parts, Extent.OfParts(width, italicCorrection, parts), emSize style, 0f<px>, 0f<px>)

    /// An atom reaching beyond what it draws, as the font asks above a bar and below an underbar.
    let paddedAtom
        (
            pma: PlacedMA,
            width: float32<px>,
            ascender: float32<px>,
            descender: float32<px>,
            style: Style
        ) =
        let parts = pma.Parts
        let body = Extent.OfParts(width, 0f<px>, parts)
        Placed(
            pma,
            parts,
            Extent(width, body.Ascent + ascender, body.Descent + descender, 0f<px>),
            emSize style,
            0f<px>,
            0f<px>)

    /// The parts of an assembly in the order they are laid, the extenders repeated as often as it
    /// takes to reach the length, up to a cap no formula reaches.
    let assembled(stretchy: StretchyGlyph, overlap: float32<px>, s: float32<px/du>, length: float32<px>) =
        let parts = Array.init stretchy.PartCount stretchy.Part
        let advance(part: AssemblyPart) = part.FullAdvance * s
        let extenders = parts |> Array.filter (fun part -> part.IsExtender)
        // Each further round of extenders lengthens the assembly by this much, overlaps allowed for.
        let round = (extenders |> Array.sumBy advance) - overlap * float32 extenders.Length
        let shortest =
            (parts |> Array.filter (fun part -> not part.IsExtender) |> Array.sumBy advance)
            - overlap * float32 (parts.Length - extenders.Length - 1)
        let repeats =
            if extenders.Length = 0 || round <= 0f<px> then 0
            else min 256 (max 0 (int (ceil ((length - shortest) / round))))
        [| for part in parts do
            for _ in 1 .. (if part.IsExtender then repeats else 1) do
                yield part |]

    let markOf(glyphs: ImmutableArray<PlacedGlyph>, width: float32<px>) =
        let ascent = ImmArray.maxWithSafe(glyphs, 0f<px>, fun g -> g.Top)
        let descent = -ImmArray.minWithSafe(glyphs, 0f<px>, fun g -> g.Bottom)
        PlacedGlyphs(glyphs, Extent(width, ascent, descent, 0f<px>))

    /// A gap where a glyph should be would be wrong output, so a missing one is an error.
    let required(glyph: Glyph voption, what: string) =
        match glyph with
        | ValueSome found -> found
        | ValueNone -> failwith $"the font cannot draw {what}"

    /// The atom reaches past the advance by the glyph lean, which is what its scripts are placed by.
    let single(glyph: Glyph, style: Style, make: PlacedGlyph -> PlacedMA) =
        let s = scale style
        let pma = make(PlacedGlyph(glyph, emSize style, 0f<px>, 0f<px>))
        atomOf(pma, (glyph.Advance + glyph.ItalicCorrection) * s, glyph.ItalicCorrection * s, style)

    /// A large operator's italic correction measures its lean rather than ink past its advance, so it
    /// does not widen the mark, though scripts and limits are still placed by it.
    let operatorMark(glyph: Glyph, style: Style) =
        let s = scale style
        PlacedGlyphs(
            ImmutableArray.Create(PlacedGlyph(glyph, emSize style, 0f<px>, 0f<px>)),
            Extent(
                glyph.Advance * s,
                glyph.Top * s,
                -glyph.Bottom * s,
                glyph.ItalicCorrection * s))

    /// A glyph with its ink resting on the origin, so that callers place it by its bottom.
    let bottomAnchored(glyph: Glyph, style: Style) =
        let s = scale style
        let placed = PlacedGlyph(glyph, emSize style, 0f<px>, -glyph.Bottom * s)
        markOf(ImmutableArray.Create placed, glyph.Advance * s)

    /// Eighteenths of an em as a width, which is what TeX's spacing table and commands are given in.
    let eighteenths(count: int, style: Style) = float32 count * emSize style / 18f

    /// Upright letters, as function names are set: one mark, so no italic correction trails them.
    let upright(text: string, style: Style) =
        let s = scale style
        let glyphs = ImmutableArray.CreateBuilder<PlacedGlyph>()
        let mutable x = 0f<px>
        for c in text do
            let glyph = required(Conventions.uprightGlyph c, $"the character {c}")
            glyphs.Add(PlacedGlyph(glyph, emSize style, x, 0f<px>))
            x <- x + glyph.Advance * s
        markOf(glyphs.ToImmutable(), x)

    let spacing(left: AtomClass, right: AtomClass, style: Style) =
        let entry = Spacing.table.[int left * 8 + int right]
        let count =
            if entry >= 0 then entry
            elif style.Size = MathSize.Display || style.Size = MathSize.Text then -entry
            else 0
        eighteenths(count, style)

    let extentOf(placed: Placed voption) = placed |> ValueOption.map (fun p -> p.Extent)

    /// The lowest ink of a mark, which its descent reads as none where the mark clears the baseline.
    let inkBottom(mark: PlacedGlyphs) = ImmArray.minWithSafe(mark.Glyphs, mark.Ascent, fun g -> g.Bottom)

    /// The highest ink of a mark, which its ascent reads as none where the mark hangs below it.
    let inkTop(mark: PlacedGlyphs) = ImmArray.maxWithSafe(mark.Glyphs, -mark.Descent, fun g -> g.Top)

    /// How far the scripts beside a base sit above and below it, and how wide the three come to.
    let scriptPlacement(b: Extent, above: Extent voption, below: Extent voption, style: Style) =
        let s = scale style
        let mutable width = b.Width
        let mutable up = 0f<px>
        let mutable down = 0f<px>
        match above with
        | ValueSome extent ->
            let start =
                (
                    if style.Cramped then MathConstants.SuperscriptShiftUpCramped
                    else MathConstants.SuperscriptShiftUp)
                * s
            up <-
                max
                    (max start (b.Ascent - MathConstants.SuperscriptBaselineDropMax * s))
                    (extent.Descent + MathConstants.SuperscriptBottomMin * s)
        | ValueNone -> ()
        match below with
        | ValueSome extent ->
            down <-
                max
                    (max
                        (MathConstants.SubscriptShiftDown * s)
                        (b.Descent + MathConstants.SubscriptBaselineDropMin * s))
                    (extent.Ascent - MathConstants.SubscriptTopMax * s)
        | ValueNone -> ()
        match above, below with
        | ValueSome over, ValueSome under ->
            let gapMin = MathConstants.SubSuperscriptGapMin * s
            let gap = (up - over.Descent) - (under.Ascent - down)
            if gap < gapMin then
                down <- down + gapMin - gap
                let shortfall =
                    MathConstants.SuperscriptBottomMaxWithSubscript * s - (up - over.Descent)
                if shortfall > 0f<px> then
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
            | ValueNone, ValueNone -> 0f<px>
            | ValueSome _, _ | _, ValueSome _ -> MathConstants.SpaceAfterScript * s
        struct (up, down, subscriptX, width + after)

    member private t.Row(elements: ImmutableArray<MA>, style: Style) =
        if elements.IsEmpty then single(Slot.box, style, PlacedMA.Placeholder)
        else
            let classes = Array.init elements.Length (fun i -> Conventions.atomClasses elements.[i])
            // A space is a gap rather than an atom, so an operator binds straight through it.
            let bound = [| for i in 0 .. elements.Length - 1 do if classes.[i].IsSome then yield i |]
            let facingLeft(i: int) = let struct (left, _) = classes.[i].Value in left
            let facingRight(i: int) = let struct (_, right) = classes.[i].Value in right
            let ordinary = ValueSome(struct (AtomClass.Ordinary, AtomClass.Ordinary))
            // A binary atom with nothing to bind is ordinary, so each gap demotes whichever side it strands.
            for n in 0 .. bound.Length - 1 do
                let i = bound.[n]
                let previous = if n = 0 then ValueNone else ValueSome(facingRight bound.[n - 1])
                if facingLeft i = AtomClass.Binary && Spacing.isUnaryPosition previous then
                    classes.[i] <- ordinary
                elif
                    n > 0
                    && facingRight bound.[n - 1] = AtomClass.Binary
                    && Spacing.leavesNothingToBind(facingLeft i)
                then
                    classes.[bound.[n - 1]] <- ordinary
            // No atom follows the last, so a binary ending the row is stranded too.
            if bound.Length > 0 && facingRight bound.[bound.Length - 1] = AtomClass.Binary then
                classes.[bound.[bound.Length - 1]] <- ordinary
            let children = ImmutableArray.CreateBuilder<Placed>()
            let mutable x = 0f<px>
            let mutable reach = 0f<px>
            let mutable italicCorrection = 0f<px>
            let mutable previous = ValueNone
            for i in 0 .. elements.Length - 1 do
                if classes.[i].IsSome then
                    match previous with
                    | ValueSome before -> x <- x + spacing(facingRight before, facingLeft i, style)
                    | ValueNone -> ()
                    previous <- ValueSome i
                let child = t.Of(elements.[i], style)
                children.Add(child.At(x, 0f<px>))
                // A lean is ink above the baseline, which the next atom sets under rather than after.
                x <- x + child.Width - child.ItalicCorrection
                reach <- max reach (x + child.ItalicCorrection)
                italicCorrection <- child.ItalicCorrection
            atomOf(PlacedMA.Row(children.ToImmutable()), reach, italicCorrection, style)

    member private t.Fraction(numerator: MA, denominator: MA, style: Style) =
        let n = t.Of(numerator, style.Numerator)
        let d = t.Of(denominator, style.Denominator)
        let s = scale style
        let axis = MathConstants.AxisHeight * s
        let thickness = MathConstants.FractionRuleThickness * s
        let value(displayStyle: float32<du>, textStyle: float32<du>) =
            (if style.IsDisplay then displayStyle else textStyle) * s
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
                PlacedRule(width, thickness, 0f<px>, ruleBottom),
                d.At((width - d.Width) / 2f, -down))
        atomOf(pma, width, 0f<px>, style)

    /// The base with its scripts placed, which both script atoms share.
    member private t.ScriptSuper(main: MA, super: MA, sub: MA voption, style: Style) =
        let b = t.Of(main, style)
        let above = t.Of(super, style.Superscript)
        let below = sub |> ValueOption.map (fun ma -> t.Of(ma, style.Subscript))
        let struct (up, down, subscriptX, width) =
            scriptPlacement(b.Extent, ValueSome above.Extent, extentOf below, style)
        let placedBelow = below |> ValueOption.map (fun p -> p.At(subscriptX, -down))
        atomOf(PlacedMA.ScriptSuper(b, above.At(b.Width, up), placedBelow), width, 0f<px>, style)

    member private t.ScriptSub(main: MA, sub: MA, style: Style) =
        let b = t.Of(main, style)
        let below = t.Of(sub, style.Subscript)
        let struct (_, down, subscriptX, width) =
            scriptPlacement(b.Extent, ValueNone, ValueSome below.Extent, style)
        atomOf(PlacedMA.ScriptSub(b, below.At(subscriptX, -down)), width, 0f<px>, style)

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
            atomOf(pma, width, 0f<px>, style)

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
                        (MathConstants.UpperLimitBaselineRiseMin * s)
                        (MathConstants.UpperLimitGapMin * s + placed.Descent)
                placed.At(centre + half - placed.Width / 2f, operator.Ascent + rise))
        let lower =
            below
            |> ValueOption.map (fun placed ->
                let drop =
                    max
                        (MathConstants.LowerLimitBaselineDropMin * s)
                        (MathConstants.LowerLimitGapMin * s + placed.Ascent)
                placed.At(centre - half - placed.Width / 2f, -(operator.Descent + drop)))
        // A limit wider than the operator overhangs on both sides, so the whole atom shifts right.
        let leftmost(placed: Placed voption) =
            match placed with
            | ValueSome found -> min 0f<px> found.X
            | ValueNone -> 0f<px>
        let left = min (leftmost upper) (leftmost lower)
        let shift(placed: Placed voption) = placed |> ValueOption.map (fun p -> p.At(p.X - left, p.Y))
        let moved = operator.At(-left, 0f<px>)
        let upper = shift upper
        let lower = shift lower
        let far(placed: Placed voption) =
            match placed with
            | ValueSome found -> found.X + found.Width
            | ValueNone -> 0f<px>
        let width = max (operator.Width - left) (max (far upper) (far lower))
        atomOf(PlacedMA.BigOp(op, moved, lower, upper), width, 0f<px>, style)

    /// The glyph grown to at least the given height, laid out resting on the origin.
    member private t.Stretched(stretchy: StretchyGlyph, style: Style, minHeight: float32<px>) =
        let s = scale style
        let mutable chosen = ValueNone
        let mutable i = 0
        while chosen.IsNone && i < stretchy.SizeCount do
            let size = stretchy.Size i
            if size.Advance * s >= minHeight then chosen <- ValueSome size.Glyph
            i <- i + 1
        match chosen with
        | ValueSome size -> bottomAnchored(size, style)
        | ValueNone when stretchy.PartCount > 0 -> t.Assembly(stretchy, style, minHeight)
        | ValueNone when stretchy.SizeCount > 0 ->
            bottomAnchored(stretchy.Size(stretchy.SizeCount - 1).Glyph, style)
        | ValueNone -> bottomAnchored(stretchy.Glyph, style)

    member private _.Assembly(stretchy: StretchyGlyph, style: Style, minHeight: float32<px>) =
        let s = scale style
        let overlap = MathConstants.MinConnectorOverlap * s
        let glyphs = ImmutableArray.CreateBuilder<PlacedGlyph>()
        let mutable y = 0f<px>
        let mutable width = 0f<px>
        for part in assembled(stretchy, overlap, s, minHeight) do
            let glyph = part.Glyph
            glyphs.Add(PlacedGlyph(glyph, emSize style, 0f<px>, y - glyph.Bottom * s))
            width <- max width (glyph.Advance * s)
            y <- y + part.FullAdvance * s - overlap
        markOf(glyphs.ToImmutable(), width)

    /// One side of a bracketed formula, which \left. leaves out altogether.
    member private t.Delimiter
        (delimiter: StretchyGlyph voption, style: Style, minHeight: float32<px>) =
        match delimiter with
        | ValueSome stretchy -> t.Stretched(stretchy, style, minHeight)
        | ValueNone -> PlacedGlyphs(ImmutableArray.Empty, Extent(0f<px>, 0f<px>, 0f<px>, 0f<px>))

    member private t.Brackets(brackets: Brackets, inner: MA, completion: BracketCompletion, style: Style) =
        let content = t.Of(inner, style)
        let s = scale style
        let axis = MathConstants.AxisHeight * s
        let reach = 2f * max (content.Ascent - axis) (content.Descent + axis)
        // TeX lets a delimiter fall a little short rather than jump to the next size up.
        let needed = max (reach * 0.901f) (reach - 0.5f * emSize style)
        let left = t.Delimiter(Conventions.leftDelimiter brackets.Left, style, needed)
        let right = t.Delimiter(Conventions.rightDelimiter brackets.Right, style, needed)
        let onAxis(mark: PlacedGlyphs, x: float32<px>) = mark.At(x, axis - mark.Ascent / 2f)
        let contentX = left.Width
        let rightX = contentX + content.Width
        let pma =
            PlacedMA.Bracketed(
                brackets,
                onAxis(left, 0f<px>),
                content.At(contentX, 0f<px>),
                onAxis(right, rightX),
                completion)
        atomOf(pma, rightX + right.Width, 0f<px>, style)

    member private t.Radical(degree: MA voption, radicand: MA, style: Style) =
        let x = t.Of(radicand, style.Cramp)
        let s = scale style
        let thickness = MathConstants.RadicalRuleThickness * s
        let gap =
            (
                if style.IsDisplay then MathConstants.RadicalDisplayStyleVerticalGap
                else MathConstants.RadicalVerticalGap)
            * s
        let needed = x.Ascent + x.Descent + gap + thickness
        let surd = t.Stretched(Radicals.surd, style, needed)
        // A surd taller than needed hangs half its surplus below and lifts the rule by the other half.
        let clearance = gap + max 0f<px> (surd.Ascent - needed) / 2f
        let ruleTop = x.Ascent + clearance + thickness
        let bottom = ruleTop - surd.Ascent
        let index = degree |> ValueOption.map (fun ma -> t.Of(ma, Style(MathSize.ScriptScript, style.Cramped)))
        let before = MathConstants.RadicalKernBeforeDegree * s
        let after = MathConstants.RadicalKernAfterDegree * s
        let indexWidth =
            match index with
            | ValueSome placed -> placed.Width
            | ValueNone -> 0f<px>
        let surdX =
            match index with
            | ValueNone -> 0f<px>
            | ValueSome _ -> max 0f<px> (before + indexWidth + after)
        let placedSurd = surd.At(surdX, bottom)
        let barX = surdX + surd.Width
        let bar = PlacedRule(x.Width, thickness, barX, ruleTop - thickness)
        let radicand = x.At(barX, 0f<px>)
        let pma =
            match index with
            | ValueSome placed ->
                let raise = MathConstants.RadicalDegreeBottomRaisePercent / 100f * surd.Ascent
                let degreeY = bottom + raise + placed.Descent
                PlacedMA.RootN(placed.At(surdX - indexWidth - after, degreeY), placedSurd, bar, radicand)
            | ValueNone -> PlacedMA.Sqrt(placedSurd, bar, radicand)
        paddedAtom(pma, barX + x.Width, MathConstants.RadicalExtraAscender * s, 0f<px>, style)

    /// The size of a growing mark that covers the width, which is how \widehat takes to its base.
    member private _.Widened(stretchy: StretchyGlyph, style: Style, minWidth: float32<px>) =
        let s = scale style
        let mutable chosen = ValueNone
        let mutable i = 0
        while chosen.IsNone && i < stretchy.SizeCount do
            let size = stretchy.Size i
            if size.Advance * s >= minWidth then chosen <- ValueSome size.Glyph
            i <- i + 1
        match chosen with
        | ValueSome glyph -> glyph
        | ValueNone when stretchy.SizeCount > 0 -> stretchy.Size(stretchy.SizeCount - 1).Glyph
        | ValueNone -> stretchy.Glyph

    /// A mark grown to span the width, drawn from its own origin rather than over a base's middle.
    member private t.Spanning(stretchy: StretchyGlyph, style: Style, minWidth: float32<px>) =
        let s = scale style
        let sized(glyph: Glyph, width: float32<du>) =
            markOf(ImmutableArray.Create(PlacedGlyph(glyph, emSize style, 0f<px>, 0f<px>)), width * s)
        let mutable chosen = ValueNone
        let mutable i = 0
        while chosen.IsNone && i < stretchy.SizeCount do
            let size = stretchy.Size i
            // A combining form draws to the left of its origin, so only a spacing size can span.
            if size.Glyph.Advance > 0f<du> && size.Advance * s >= minWidth then chosen <- ValueSome size
            i <- i + 1
        match chosen with
        | ValueSome size -> sized(size.Glyph, size.Advance)
        | ValueNone when stretchy.PartCount > 0 -> t.Spread(stretchy, style, minWidth)
        | ValueNone when stretchy.SizeCount > 0 ->
            let size = stretchy.Size(stretchy.SizeCount - 1)
            sized(size.Glyph, size.Advance)
        | ValueNone -> sized(stretchy.Glyph, stretchy.Glyph.Advance)

    /// The parts laid along the line, repeating the extenders up to a cap no formula reaches.
    member private _.Spread(stretchy: StretchyGlyph, style: Style, minWidth: float32<px>) =
        let s = scale style
        let overlap = MathConstants.MinConnectorOverlap * s
        let glyphs = ImmutableArray.CreateBuilder<PlacedGlyph>()
        let mutable x = 0f<px>
        for part in assembled(stretchy, overlap, s, minWidth) do
            glyphs.Add(PlacedGlyph(part.Glyph, emSize style, x, 0f<px>))
            x <- x + part.FullAdvance * s - overlap
        markOf(glyphs.ToImmutable(), x + overlap)

    /// Where an accent sits over an atom: its glyph's attachment, or the middle of one drawn from more.
    member private _.Attachment(placed: Placed, style: Style) =
        match placed.Pma.SingleGlyph with
        | ValueSome glyph -> glyph.X + glyph.Glyph.TopAccentAttachment * scale style
        | ValueNone -> placed.Width / 2f

    /// The accent rises clear of a base taller than the height the font draws its accents for.
    member private t.Accented(accent: Accent, x: MA, style: Style) =
        let b = t.Of(x, style.Cramp)
        let s = scale style
        let glyph =
            match Conventions.accent accent with
            | AccentMark.Fixed found -> found
            | AccentMark.Wide stretchy -> t.Widened(stretchy, style, b.Width)
        let mark =
            PlacedGlyph(
                glyph,
                emSize style,
                t.Attachment(b, style) - glyph.TopAccentAttachment * s,
                max 0f<px> (b.Ascent - MathConstants.AccentBaseHeight * s))
        atomOf(PlacedMA.Accented(accent, mark, b), b.Width, b.ItalicCorrection, style)

    /// A mark grown to span the atom, set clear of its ink above or below.
    member private t.Spanned(spanning: Spanning, x: MA, style: Style) =
        let below = Conventions.spansBelow spanning
        let b = t.Of(x, (if below then style else style.Cramp))
        let s = scale style
        let mark = t.Spanning(Conventions.spanning spanning, style, b.Width)
        let width = max b.Width mark.Width
        let y =
            if below then -(b.Descent + MathConstants.StretchStackGapBelowMin * s) - inkTop mark
            else b.Ascent + MathConstants.StretchStackGapAboveMin * s - inkBottom mark
        let pma =
            PlacedMA.Spanned(
                spanning,
                mark.At((width - mark.Width) / 2f, y),
                b.At((width - b.Width) / 2f, 0f<px>))
        atomOf(pma, width, 0f<px>, style)

    /// A rule over the atom, clear of its ink by the gap the font names.
    member private t.Overline(x: MA, style: Style) =
        let b = t.Of(x, style.Cramp)
        let s = scale style
        let thickness = MathConstants.OverbarRuleThickness * s
        let gap = MathConstants.OverbarVerticalGap * s
        let pma = PlacedMA.Overline(PlacedRule(b.Width, thickness, 0f<px>, b.Ascent + gap), b)
        paddedAtom(pma, b.Width, MathConstants.OverbarExtraAscender * s, 0f<px>, style)

    /// A rule under the atom, clear of its ink by the gap the font names.
    member private t.Underline(x: MA, style: Style) =
        let b = t.Of(x, style)
        let s = scale style
        let thickness = MathConstants.UnderbarRuleThickness * s
        let gap = MathConstants.UnderbarVerticalGap * s
        let pma = PlacedMA.Underline(b, PlacedRule(b.Width, thickness, 0f<px>, -(b.Descent + gap + thickness)))
        paddedAtom(pma, b.Width, 0f<px>, MathConstants.UnderbarExtraDescender * s, style)

    /// A fraction with no rule, where only the gap keeps the two apart.
    member private t.Stack(top: MA, bottom: MA, style: Style) =
        let above = t.Of(top, style.Numerator)
        let below = t.Of(bottom, style.Denominator)
        let s = scale style
        let value(displayStyle: float32<du>, textStyle: float32<du>) =
            (if style.IsDisplay then displayStyle else textStyle) * s
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
        atomOf(pma, width, 0f<px>, style)

    /// A grid centred on the axis, its rows a line's leading apart and its columns an em apart.
    member private t.Table(cells: ImmA2D<MA>, alignments: ImmutableArray<Alignment>, style: Style) =
        let s = scale style
        let placed = cells |> ImmA2D.map (fun cell -> t.Of(cell, style))
        /// How far the tallest, deepest or widest cell of a row reaches.
        let furthest(row: int, reach: Placed -> float32<px>) =
            let mutable value = 0f<px>
            for col in 0 .. placed.Cols - 1 do
                value <- max value (reach placed.[row, col])
            value
        let widths = Array.zeroCreate<float32<px>> placed.Cols
        for row in 0 .. placed.Rows - 1 do
            for col in 0 .. placed.Cols - 1 do
                widths.[col] <- max widths.[col] placed.[row, col].Width
        let gap = eighteenths(Conventions.tableColumnGap, style)
        let lefts = Array.zeroCreate<float32<px>> placed.Cols
        let mutable right = 0f<px>
        for col in 0 .. placed.Cols - 1 do
            lefts.[col] <- right
            right <- right + widths.[col] + gap
        let width = max 0f<px> (right - gap)
        let leading = MathConstants.MathLeading * s
        let baselines = Array.zeroCreate<float32<px>> placed.Rows
        let mutable y = 0f<px>
        for row in 0 .. placed.Rows - 1 do
            if row > 0 then y <- y - furthest(row - 1, fun cell -> cell.Descent) - leading
            y <- y - furthest(row, fun cell -> cell.Ascent)
            baselines.[row] <- y
        let height =
            if placed.Rows = 0 then 0f<px> else furthest(placed.Rows - 1, fun cell -> cell.Descent) - y
        // The grid is centred on the axis, as a fraction of the same height would be.
        let rise = height / 2f + MathConstants.AxisHeight * s
        let laid =
            placed
            |> ImmA2D.mapi (fun row col cell ->
                let offset =
                    match MA.AlignmentOf(alignments, col) with
                    | Alignment.Centre -> (widths.[col] - cell.Width) / 2f
                    | Alignment.Left -> 0f<px>
                    | Alignment.Right -> widths.[col] - cell.Width
                cell.At(lefts.[col] + offset, baselines.[row] + rise))
        atomOf(PlacedMA.Table(laid, alignments), width, 0f<px>, style)

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
            atomOf(PlacedMA.Function(f, letters), letters.Width, 0f<px>, style)
        | MA.Operator o -> single(Conventions.operator o, style, fun g -> PlacedMA.Operator(o, g))
        | MA.Frac(numerator, denominator) -> t.Fraction(numerator, denominator, style)
        | MA.ScriptSuper(main, super, sub) -> t.ScriptSuper(main, super, sub, style)
        | MA.ScriptSub(main, sub) -> t.ScriptSub(main, sub, style)
        | MA.BigOp(op, lower, upper) -> t.BigOp(op, lower, upper, style)
        | MA.Bracketed(brackets, inner, completion) -> t.Brackets(brackets, inner, completion, style)
        | MA.Sqrt x -> t.Radical(ValueNone, x, style)
        | MA.RootN(n, x) -> t.Radical(ValueSome n, x, style)
        | MA.Accented(accent, x) -> t.Accented(accent, x, style)
        | MA.Spanned(mark, x) -> t.Spanned(mark, x, style)
        | MA.Overline x -> t.Overline(x, style)
        | MA.Underline x -> t.Underline(x, style)
        | MA.Stack(top, bottom) -> t.Stack(top, bottom, style)
        | MA.Table(cells, alignments) -> t.Table(cells, alignments, style)
        | MA.Text text ->
            let letters = upright(text, style)
            atomOf(PlacedMA.Text(text, letters), letters.Width, 0f<px>, style)
        | MA.Space space ->
            atomOf(PlacedMA.Space space, eighteenths(Conventions.space space, style), 0f<px>, style)
        | MA.Coloured(colour, x) ->
            let inner = t.Of(x, style)
            atomOf(PlacedMA.Coloured(colour, inner), inner.Width, inner.ItalicCorrection, style)

    /// Laid out on a line of its own, where fractions and radicals are given their full height.
    member t.Of(ma: MA) = t.Of(ma, MathSize.Display)

    member t.Of(ma: MA, size: MathSize) = t.Of(ma.Flatten, Style(size, false))

    /// Laid out with a cursor in it, which is drawn over the formula rather than among it.
    member internal t.Of(curs: MACurs) = t.Of(curs, MathSize.Display)

    member internal t.Of(curs: MACurs, size: MathSize) =
        let flat = curs.Flatten
        PlacedCurs.Of(flat, t.Of(flat.ToMA, Style(size, false)))

