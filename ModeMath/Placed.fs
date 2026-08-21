namespace ModeMath

open System.Collections.Immutable
open FSUtils

/// Whether something is part of the formula or only offered, such as an unclosed bracket's partner.
type Ink =
    | Solid = 0
    | Tentative = 1

/// What a laid-out atom measures, in points from its own origin on the baseline, y upwards.
[<Struct>]
type Extent
    (
        width: float32<px>,
        ascent: float32<px>,
        descent: float32<px>,
        italicCorrection: float32<px>
    ) =
    member _.Width = width
    member _.Ascent = ascent
    /// Depth below the baseline, positive downwards.
    member _.Descent = descent
    /// How far the last glyph leans past its advance.
    member _.ItalicCorrection = italicCorrection
    member _.Height = ascent + descent

/// One glyph at a point size, offset from the origin of the atom that drew it.
[<Struct>]
type PlacedGlyph(glyph: Glyph, size: float32<px>, x: float32<px>, y: float32<px>) =
    member _.Glyph = glyph
    member _.Size = size
    member _.X = x
    member _.Y = y
    member private _.Scale = size / design MathConstants.UnitsPerEm
    member t.Top = y + design glyph.Top * t.Scale
    member t.Bottom = y + design glyph.Bottom * t.Scale

/// Glyphs forming one mark: a delimiter grown by stacking, a surd, or a function's letters.
[<Struct>]
type PlacedGlyphs(glyphs: ImmutableArray<PlacedGlyph>, extent: Extent) =
    member _.Glyphs = glyphs
    member _.Extent = extent
    member _.Width = extent.Width
    member _.Ascent = extent.Ascent
    member _.Descent = extent.Descent
    member _.ItalicCorrection = extent.ItalicCorrection
    member _.Height = extent.Height
    /// The same mark, moved by an offset its atom has chosen for it.
    member _.At(x: float32<px>, y: float32<px>) =
        PlacedGlyphs(glyphs |> ImmArray.map (fun g -> PlacedGlyph(g.Glyph, g.Size, g.X + x, g.Y + y)), extent)

/// A filled rectangle: a fraction's bar or a radical's overbar.
[<Struct>]
type PlacedRule(width: float32<px>, thickness: float32<px>, x: float32<px>, y: float32<px>) =
    member _.Width = width
    member _.Thickness = thickness
    member _.X = x
    member _.Y = y

/// A laid-out MA, which keeps the shape of the MA it was laid out from.
[<RequireQualifiedAccess>]
type PlacedMA =
    | Row of ImmutableArray<Placed>
    | Char of char * PlacedGlyph
    | BoldVar of char * PlacedGlyph
    | Blackboard of char * PlacedGlyph
    | Cdot of PlacedGlyph
    | UprightD of PlacedGlyph
    | ScriptSuper of main: Placed * super: Placed * sub: Placed voption
    | ScriptSub of main: Placed * sub: Placed
    | Frac of numerator: Placed * rule: PlacedRule * denominator: Placed
    | Function of MathFunction * letters: PlacedGlyphs
    | Operator of Operator * PlacedGlyph
    | Bracketed of Brackets * left: PlacedGlyphs * inner: Placed * right: PlacedGlyphs * BracketCompletion
    | RootN of degree: Placed * surd: PlacedGlyphs * bar: PlacedRule * radicand: Placed
    | Sqrt of surd: PlacedGlyphs * bar: PlacedRule * radicand: Placed
    | BigOp of BigOperator * operator: PlacedGlyphs * lower: Placed voption * upper: Placed voption
    | Accented of accent: Accent * mark: PlacedGlyph * x: Placed
    | Overline of rule: PlacedRule * x: Placed
    | Underline of x: Placed * rule: PlacedRule
    | Stack of top: Placed * bottom: Placed
    | Table of cells: ImmA2D<Placed> * alignments: ImmutableArray<Alignment>

/// A laid-out MA at an offset from its parent's origin, holding what it draws so that painting a
/// second time builds nothing.
and [<Struct>] Placed
    (
        pma: PlacedMA,
        parts: ImmutableArray<Part>,
        extent: Extent,
        x: float32<px>,
        y: float32<px>
    ) =
    member _.Pma = pma
    member _.Parts = parts
    member _.Extent = extent
    member _.X = x
    member _.Y = y
    member _.Width = extent.Width
    member _.Ascent = extent.Ascent
    member _.Descent = extent.Descent
    member _.ItalicCorrection = extent.ItalicCorrection
    member _.Height = extent.Height
    /// The same atom, moved to an offset its parent has chosen for it.
    member _.At(x: float32<px>, y: float32<px>) = Placed(pma, parts, extent, x, y)

/// One thing drawn, so that a painter need know nothing of the atom that drew it.
and [<RequireQualifiedAccess>] Part =
    | Glyph of glyph: PlacedGlyph * ink: Ink
    | Rule of rule: PlacedRule * ink: Ink
    | Child of Placed

type PlacedMA with
    /// Everything this atom draws, in the order it is drawn. Built once, and kept by the Placed that
    /// holds it, so painting reads Placed.Parts rather than building them again.
    member internal t.Parts: ImmutableArray<Part> =
        let b = ImmutableArray.CreateBuilder<Part>()
        let glyph(g: PlacedGlyph) = b.Add(Part.Glyph(g, Ink.Solid))
        let marks(m: PlacedGlyphs, ink: Ink) = for g in m.Glyphs do b.Add(Part.Glyph(g, ink))
        let rule(r: PlacedRule) = b.Add(Part.Rule(r, Ink.Solid))
        let child(c: Placed) = b.Add(Part.Child c)
        let optional(c: Placed voption) =
            match c with
            | ValueSome found -> child found
            | ValueNone -> ()
        match t with
        | PlacedMA.Row children -> for c in children do child c
        | PlacedMA.Char(_, g) | PlacedMA.BoldVar(_, g) | PlacedMA.Blackboard(_, g) | PlacedMA.Cdot g
        | PlacedMA.UprightD g | PlacedMA.Operator(_, g) -> glyph g
        | PlacedMA.ScriptSuper(main, super, sub) ->
            child main
            child super
            optional sub
        | PlacedMA.ScriptSub(main, sub) ->
            child main
            child sub
        | PlacedMA.Frac(numerator, bar, denominator) ->
            child numerator
            rule bar
            child denominator
        | PlacedMA.Function(_, letters) -> marks(letters, Ink.Solid)
        | PlacedMA.Bracketed(_, left, inner, right, completion) ->
            let ink(completed: bool) = if completed then Ink.Solid else Ink.Tentative
            marks(left, ink completion.LeftCompleted)
            child inner
            marks(right, ink completion.RightCompleted)
        | PlacedMA.RootN(degree, surd, bar, radicand) ->
            child degree
            marks(surd, Ink.Solid)
            rule bar
            child radicand
        | PlacedMA.Sqrt(surd, bar, radicand) ->
            marks(surd, Ink.Solid)
            rule bar
            child radicand
        | PlacedMA.BigOp(_, operator, lower, upper) ->
            marks(operator, Ink.Solid)
            optional lower
            optional upper
        | PlacedMA.Accented(_, mark, x) ->
            child x
            glyph mark
        | PlacedMA.Overline(bar, x) ->
            child x
            rule bar
        | PlacedMA.Underline(x, bar) ->
            child x
            rule bar
        | PlacedMA.Stack(top, bottom) ->
            child top
            child bottom
        | PlacedMA.Table(cells, _) -> for cell in cells.Elements do child cell
        b.ToImmutable()

    /// The glyph this atom draws, where it draws exactly one and nothing besides.
    member t.SingleGlyph: PlacedGlyph voption =
        match t with
        | PlacedMA.Char(_, g) | PlacedMA.BoldVar(_, g) | PlacedMA.Blackboard(_, g) | PlacedMA.Cdot g
        | PlacedMA.UprightD g | PlacedMA.Operator(_, g) -> ValueSome g
        | PlacedMA.Row _ | PlacedMA.ScriptSuper _ | PlacedMA.ScriptSub _ | PlacedMA.Frac _
        | PlacedMA.Function _ | PlacedMA.Bracketed _ | PlacedMA.RootN _ | PlacedMA.Sqrt _
        | PlacedMA.BigOp _ | PlacedMA.Accented _ | PlacedMA.Overline _ | PlacedMA.Underline _
        | PlacedMA.Stack _ | PlacedMA.Table _ -> ValueNone

    /// The MA this was laid out from, which a cursor position is expressed against.
    member t.ToMA: MA =
        match t with
        | PlacedMA.Row children -> MA.Row(children |> ImmArray.map (fun c -> c.Pma.ToMA))
        | PlacedMA.Char(c, _) -> MA.Char c
        | PlacedMA.BoldVar(c, _) -> MA.BoldVar c
        | PlacedMA.Blackboard(c, _) -> MA.Blackboard c
        | PlacedMA.Cdot _ -> MA.Cdot
        | PlacedMA.UprightD _ -> MA.UprightD
        | PlacedMA.ScriptSuper(main, super, sub) ->
            MA.ScriptSuper(main.Pma.ToMA, super.Pma.ToMA, sub |> ValueOption.map (fun s -> s.Pma.ToMA))
        | PlacedMA.ScriptSub(main, sub) -> MA.ScriptSub(main.Pma.ToMA, sub.Pma.ToMA)
        | PlacedMA.Frac(numerator, _, denominator) -> MA.Frac(numerator.Pma.ToMA, denominator.Pma.ToMA)
        | PlacedMA.Function(f, _) -> MA.Function f
        | PlacedMA.Operator(o, _) -> MA.Operator o
        | PlacedMA.Bracketed(brackets, _, inner, _, completion) ->
            MA.Bracketed(brackets, inner.Pma.ToMA, completion)
        | PlacedMA.RootN(degree, _, _, radicand) -> MA.RootN(degree.Pma.ToMA, radicand.Pma.ToMA)
        | PlacedMA.Sqrt(_, _, radicand) -> MA.Sqrt radicand.Pma.ToMA
        | PlacedMA.BigOp(op, _, lower, upper) ->
            MA.BigOp(
                op,
                lower |> ValueOption.map (fun l -> l.Pma.ToMA),
                upper |> ValueOption.map (fun u -> u.Pma.ToMA))
        | PlacedMA.Accented(accent, _, x) -> MA.Accented(accent, x.Pma.ToMA)
        | PlacedMA.Overline(_, x) -> MA.Overline x.Pma.ToMA
        | PlacedMA.Underline(x, _) -> MA.Underline x.Pma.ToMA
        | PlacedMA.Stack(top, bottom) -> MA.Stack(top.Pma.ToMA, bottom.Pma.ToMA)
        | PlacedMA.Table(cells, alignments) ->
            MA.Table(cells |> ImmA2D.map (fun cell -> cell.Pma.ToMA), alignments)

type Extent with
    /// Covering everything drawn, which is placed relative to the atom's own origin.
    static member OfParts
        (width: float32<px>, italicCorrection: float32<px>, parts: ImmutableArray<Part>) =
        let top(part: Part) =
            match part with
            | Part.Child c -> c.Y + c.Extent.Ascent
            | Part.Rule(r, _) -> r.Y + r.Thickness
            | Part.Glyph(g, _) -> g.Top
        let bottom(part: Part) =
            match part with
            | Part.Child c -> c.Y - c.Extent.Descent
            | Part.Rule(r, _) -> r.Y
            | Part.Glyph(g, _) -> g.Bottom
        let ascent = ImmArray.maxWithSafe(parts, 0f<px>, top)
        let descent = -ImmArray.minWithSafe(parts, 0f<px>, bottom)
        Extent(width, ascent, descent, italicCorrection)
