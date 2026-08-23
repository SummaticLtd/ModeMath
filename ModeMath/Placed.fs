namespace ModeMath

open System.Collections.Immutable
open System.Drawing
open FSUtils

/// The box an empty slot shows, which sizes the cursor as well as filling the slot.
module internal Slot =
    let box =
        match MathFont.OfChar '□' with
        | ValueSome glyph -> glyph
        | ValueNone -> failwith "the font cannot draw an empty slot"

/// Whether something is part of the formula or only offered, such as an unclosed bracket's partner.
type Ink =
    | Solid = 0
    | Tentative = 1

/// What a laid-out atom measures, in pixels from its own origin on the baseline, y upwards.
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

/// One glyph at the size it is set in, offset from the origin of the atom that drew it.
[<Struct>]
type PlacedGlyph(glyph: Glyph, size: float32<px>, x: float32<px>, y: float32<px>) =
    member _.Glyph = glyph
    member _.Size = size
    member _.X = x
    member _.Y = y
    member private _.Scale = size / MathConstants.UnitsPerEm
    member t.Top = y + glyph.Top * t.Scale
    member t.Bottom = y + glyph.Bottom * t.Scale

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
    | UprightD of PlacedGlyph
    | ScriptSuper of main: Placed * super: Placed * sub: Placed voption
    | ScriptSub of main: Placed * sub: Placed
    | Frac of numerator: Placed * rule: PlacedRule * denominator: Placed
    | Function of MathFunction * letters: PlacedGlyphs
    | Bracketed of Brackets * left: PlacedGlyphs * inner: Placed * right: PlacedGlyphs * BracketCompletion
    | RootN of degree: Placed * surd: PlacedGlyphs * bar: PlacedRule * radicand: Placed
    | Sqrt of surd: PlacedGlyphs * bar: PlacedRule * radicand: Placed
    | BigOp of BigOperator * operator: PlacedGlyphs * lower: Placed voption * upper: Placed voption
    | Accented of accent: Accent * mark: PlacedGlyph * x: Placed
    | Spanned of mark: Spanning * grown: PlacedGlyphs * x: Placed
    | Overline of rule: PlacedRule * x: Placed
    | Underline of x: Placed * rule: PlacedRule
    | Stack of top: Placed * bottom: Placed
    | Table of cells: ImmA2D<Placed> * alignments: ImmutableArray<Alignment>
    | Coloured of colour: Color * x: Placed
    | Text of string * letters: PlacedGlyphs
    /// A gap, which draws nothing at all.
    | Space of Space
    /// An empty slot, which shows the box a formula could be written in.
    | Placeholder of PlacedGlyph

/// A laid-out MA at an offset from its parent's origin, holding what it draws so that painting a
/// second time builds nothing.
and [<Struct>] Placed
    (
        pma: PlacedMA,
        parts: ImmutableArray<Part>,
        extent: Extent,
        emSize: float32<px>,
        x: float32<px>,
        y: float32<px>
    ) =
    member _.Pma = pma
    member _.Parts = parts
    member _.Extent = extent
    /// The pixels to the em this atom was set at, which is smaller inside a script or a fraction.
    member _.EmSize = emSize
    member _.X = x
    member _.Y = y
    member _.Width = extent.Width
    member _.Ascent = extent.Ascent
    member _.Descent = extent.Descent
    member _.ItalicCorrection = extent.ItalicCorrection
    member _.Height = extent.Height
    /// The same atom, moved to an offset its parent has chosen for it.
    member _.At(x: float32<px>, y: float32<px>) = Placed(pma, parts, extent, emSize, x, y)

/// One thing drawn, so that a painter need know nothing of the atom that drew it.
and [<RequireQualifiedAccess>] Part =
    | Glyph of glyph: PlacedGlyph * ink: Ink
    | Rule of rule: PlacedRule * ink: Ink
    | Child of Placed
    /// A child drawn in a colour of its own, which the one around it goes back to afterwards.
    | Painted of colour: Color * child: Placed

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
        | Row children -> for c in children do child c
        | Char(_, g) | BoldVar(_, g) | Blackboard(_, g)
        | UprightD g -> glyph g
        | ScriptSuper(main, super, sub) ->
            child main
            child super
            optional sub
        | ScriptSub(main, sub) ->
            child main
            child sub
        | Frac(numerator, bar, denominator) ->
            child numerator
            rule bar
            child denominator
        | Function(_, letters) -> marks(letters, Ink.Solid)
        | Bracketed(_, left, inner, right, completion) ->
            let ink(completed: bool) = if completed then Ink.Solid else Ink.Tentative
            marks(left, ink completion.LeftCompleted)
            child inner
            marks(right, ink completion.RightCompleted)
        | RootN(degree, surd, bar, radicand) ->
            child degree
            marks(surd, Ink.Solid)
            rule bar
            child radicand
        | Sqrt(surd, bar, radicand) ->
            marks(surd, Ink.Solid)
            rule bar
            child radicand
        | BigOp(_, operator, lower, upper) ->
            marks(operator, Ink.Solid)
            optional lower
            optional upper
        | Accented(_, mark, x) ->
            child x
            glyph mark
        | Spanned(_, grown, x) ->
            child x
            marks(grown, Ink.Solid)
        | Overline(bar, x) ->
            child x
            rule bar
        | Underline(x, bar) ->
            child x
            rule bar
        | Stack(top, bottom) ->
            child top
            child bottom
        | Table(cells, _) -> for cell in cells.Elements do child cell
        | Coloured(colour, x) -> b.Add(Part.Painted(colour, x))
        | Text(_, letters) -> marks(letters, Ink.Solid)
        | Space _ -> ()
        | Placeholder box -> glyph box
        b.ToImmutable()

    /// The glyph this atom draws, where it draws exactly one and nothing besides.
    member t.SingleGlyph: PlacedGlyph voption =
        match t with
        | Char(_, g) | BoldVar(_, g) | Blackboard(_, g)
        | UprightD g -> ValueSome g
        | Row _ | ScriptSuper _ | ScriptSub _ | Frac _
        | Function _ | Bracketed _ | RootN _ | Sqrt _
        | BigOp _ | Accented _ | Spanned _ | Overline _
        | Underline _ | Stack _ | Table _ | Text _
        | Space _ | Coloured _ | Placeholder _ -> ValueNone

    /// The MA this was laid out from, which a cursor position is expressed against.
    member t.ToMA: MA =
        match t with
        | Row children -> MA.Row(children |> ImmArray.map (fun c -> c.Pma.ToMA))
        | Char(c, _) -> MA.Char c
        | BoldVar(c, _) -> MA.BoldVar c
        | Blackboard(c, _) -> MA.Blackboard c
        | UprightD _ -> MA.UprightD
        | ScriptSuper(main, super, sub) ->
            MA.ScriptSuper(main.Pma.ToMA, super.Pma.ToMA, sub |> ValueOption.map (fun s -> s.Pma.ToMA))
        | ScriptSub(main, sub) -> MA.ScriptSub(main.Pma.ToMA, sub.Pma.ToMA)
        | Frac(numerator, _, denominator) -> MA.Frac(numerator.Pma.ToMA, denominator.Pma.ToMA)
        | Function(f, _) -> MA.Function f
        | Bracketed(brackets, _, inner, _, completion) ->
            MA.Bracketed(brackets, inner.Pma.ToMA, completion)
        | RootN(degree, _, _, radicand) -> MA.RootN(degree.Pma.ToMA, radicand.Pma.ToMA)
        | Sqrt(_, _, radicand) -> MA.Sqrt radicand.Pma.ToMA
        | BigOp(op, _, lower, upper) ->
            MA.BigOp(
                op,
                lower |> ValueOption.map (fun l -> l.Pma.ToMA),
                upper |> ValueOption.map (fun u -> u.Pma.ToMA))
        | Accented(accent, _, x) -> MA.Accented(accent, x.Pma.ToMA)
        | Spanned(mark, _, x) -> MA.Spanned(mark, x.Pma.ToMA)
        | Overline(_, x) -> MA.Overline x.Pma.ToMA
        | Underline(x, _) -> MA.Underline x.Pma.ToMA
        | Stack(top, bottom) -> MA.Stack(top.Pma.ToMA, bottom.Pma.ToMA)
        | Table(cells, alignments) ->
            MA.Table(cells |> ImmA2D.map (fun cell -> cell.Pma.ToMA), alignments)
        | Coloured(colour, x) -> MA.Coloured(colour, x.Pma.ToMA)
        | Text(text, _) -> MA.Text text
        | Space space -> MA.Space space
        | Placeholder _ -> MA.Empty

type Extent with
    /// Covering everything drawn, which is placed relative to the atom's own origin.
    static member OfParts
        (width: float32<px>, italicCorrection: float32<px>, parts: ImmutableArray<Part>) =
        let top(part: Part) =
            match part with
            | Part.Child c | Part.Painted(_, c) -> c.Y + c.Extent.Ascent
            | Part.Rule(r, _) -> r.Y + r.Thickness
            | Part.Glyph(g, _) -> g.Top
        let bottom(part: Part) =
            match part with
            | Part.Child c | Part.Painted(_, c) -> c.Y - c.Extent.Descent
            | Part.Rule(r, _) -> r.Y
            | Part.Glyph(g, _) -> g.Bottom
        let ascent = ImmArray.maxWithSafe(parts, 0f<px>, top)
        let descent = -ImmArray.minWithSafe(parts, 0f<px>, bottom)
        Extent(width, ascent, descent, italicCorrection)

/// A cursor in a laid-out formula, which mirrors MACurs case for case.
[<RequireQualifiedAccess>]
type PlacedMACurs =
    /// Filling the box an empty slot shows.
    | Fills of caret: PlacedRule
    /// Standing between the atoms of a row, which is where a caret is drawn as a bar.
    | Between of before: ImmutableArray<Placed> * caret: PlacedRule * after: ImmutableArray<Placed>
    /// Inside one atom of a row.
    | Within of before: ImmutableArray<Placed> * PlacedCurs * after: ImmutableArray<Placed>
    | ScriptMainSuper of main: PlacedCurs * super: Placed * sub: Placed voption
    | ScriptMainSub of main: PlacedCurs * sub: Placed
    | ScriptSuper of main: Placed * super: PlacedCurs * sub: Placed voption
    | ScriptSub of main: Placed * super: Placed voption * sub: PlacedCurs
    | FracNum of n: PlacedCurs * d: Placed
    | FracDen of n: Placed * d: PlacedCurs
    | Bracketed of Brackets * PlacedCurs * completion: BracketCompletion
    | RootNDegree of n: PlacedCurs * x: Placed
    | RootNMain of n: Placed * x: PlacedCurs
    | Sqrt of x: PlacedCurs

/// The atom a cursor stands in, laid out, with the way down to the cursor inside it.
and [<Struct>] PlacedCurs(placed: Placed, curs: PlacedMACurs) =
    /// The bar a cursor is drawn as where the pen stood, as tall as the box an empty slot shows.
    static member internal Bar(pen: float32<px>, emSize: float32<px>) =
        let scale = emSize / MathConstants.UnitsPerEm
        let thickness = MathConstants.FractionRuleThickness * scale
        PlacedRule(
            thickness,
            (Slot.box.Top - Slot.box.Bottom) * scale,
            pen - thickness / 2f,
            Slot.box.Bottom * scale)

    /// How far a point is from a rectangle, which is nothing at all where it is inside it.
    static member internal Away
        (
            left: float32<px>,
            right: float32<px>,
            bottom: float32<px>,
            top: float32<px>,
            x: float32<px>,
            y: float32<px>
        ) =
        let outside(from: float32<px>, until: float32<px>, point: float32<px>) =
            max 0f<px> (max (from - point) (point - until))
        outside(left, right, x) + outside(bottom, top, y)

    /// The atom itself, which is laid out the same whatever the cursor in it is doing.
    member _.Placed = placed
    member _.Curs = curs
    /// What the formula and the cursor in it cover together, which is more than the formula alone
    /// since a caret stands as tall as the box an empty slot shows.
    member t.Bounds: PlacedRule =
        let caret = t.Caret
        let left = min 0f<px> caret.X
        let bottom = min -placed.Descent caret.Y
        PlacedRule(
            Measure.max32(placed.Width, (caret.X + caret.Width)) - left,
            Measure.max32(placed.Ascent, (caret.Y + caret.Thickness)) - bottom,
            left,
            bottom)

    /// Where the cursor is, in pixels from this atom's origin.
    member t.Caret: PlacedRule =
        let below(child: PlacedCurs) =
            let caret = child.Caret
            PlacedRule(caret.Width, caret.Thickness, child.Placed.X + caret.X, child.Placed.Y + caret.Y)
        match curs with
        | PlacedMACurs.Fills caret | PlacedMACurs.Between(_, caret, _) -> caret
        | PlacedMACurs.Within(_, child, _)
        | PlacedMACurs.ScriptMainSuper(child, _, _)
        | PlacedMACurs.ScriptMainSub(child, _)
        | PlacedMACurs.ScriptSuper(_, child, _)
        | PlacedMACurs.ScriptSub(_, _, child)
        | PlacedMACurs.FracNum(child, _)
        | PlacedMACurs.FracDen(_, child)
        | PlacedMACurs.Bracketed(_, child, _)
        | PlacedMACurs.RootNDegree(child, _)
        | PlacedMACurs.RootNMain(_, child)
        | PlacedMACurs.Sqrt child -> below child

type PlacedCurs with
    /// The cursor this was placed from, which an edit is made against.
    member internal t.ToMACurs: MACurs =
        let ma(placed: Placed) = placed.Pma.ToMA
        let each(placed: ImmutableArray<Placed>) = placed |> ImmArray.map ma
        match t.Curs with
        | PlacedMACurs.Fills _ -> MACurs.CursorOrEmpty
        | PlacedMACurs.Between(before, _, after) ->
            MACurs.MakeRow(each before, MACurs.CursorOrEmpty, each after)
        | PlacedMACurs.Within(before, inner, after) ->
            MACurs.MakeRow(each before, inner.ToMACurs, each after)
        | PlacedMACurs.ScriptMainSuper(main, super, sub) ->
            MACurs.ScriptMainSuper(main.ToMACurs, ma super, sub |> ValueOption.map ma)
        | PlacedMACurs.ScriptMainSub(main, sub) -> MACurs.ScriptMainSub(main.ToMACurs, ma sub)
        | PlacedMACurs.ScriptSuper(main, super, sub) ->
            MACurs.ScriptSuper(ma main, super.ToMACurs, sub |> ValueOption.map ma)
        | PlacedMACurs.ScriptSub(main, super, sub) ->
            MACurs.ScriptSub(ma main, super |> ValueOption.map ma, sub.ToMACurs)
        | PlacedMACurs.FracNum(n, d) -> MACurs.FracNum(n.ToMACurs, ma d)
        | PlacedMACurs.FracDen(n, d) -> MACurs.FracDen(ma n, d.ToMACurs)
        | PlacedMACurs.Bracketed(brackets, inner, completion) ->
            MACurs.Bracketed(brackets, inner.ToMACurs, completion)
        | PlacedMACurs.RootNDegree(n, x) -> MACurs.RootNDegree(n.ToMACurs, ma x)
        | PlacedMACurs.RootNMain(n, x) -> MACurs.RootNMain(ma n, x.ToMACurs)
        | PlacedMACurs.Sqrt x -> MACurs.Sqrt x.ToMACurs

    /// The atoms of a slot. A row of one is laid out as that one, which stands for the row itself.
    static member private Children(placed: Placed) =
        match placed.Pma with
        | PlacedMA.Row children -> children
        | _ -> ImmutableArray.Create(placed.At(0f<px>, 0f<px>))

    /// Where the pen stood after the first count of them, which is where a caret between them goes.
    static member private Pen(children: ImmutableArray<Placed>, count: int) =
        if count < children.Length then children.[count].X
        elif children.IsEmpty then 0f<px>
        else
            let last = children.[children.Length - 1]
            last.X + last.Width - last.ItalicCorrection

    /// The cursor put against a formula already laid out, which must be the formula it stands in.
    static member internal Of(curs: MACurs, placed: Placed): PlacedCurs =
        let children = PlacedCurs.Children placed
        let bar(count: int) = PlacedCurs.Bar(PlacedCurs.Pen(children, count), placed.EmSize)
        let first(count: int) = children.RemoveRange(count, children.Length - count)
        let rest(count: int) = children.RemoveRange(0, count)
        let wrong(kind: string) = failwith $"a cursor in a {kind} was placed over {placed.Pma.ToMA}"
        let scripts() =
            match placed.Pma with
            | PlacedMA.ScriptSuper(main, super, sub) -> struct(main, ValueSome super, sub)
            | PlacedMA.ScriptSub(main, sub) -> struct(main, ValueNone, ValueSome sub)
            | _ -> wrong "script"
        let script(part: Placed voption, kind: string) =
            match part with
            | ValueSome found -> found
            | ValueNone -> wrong kind
        match curs with
        | MACurs.CursorOrEmpty ->
            let caret =
                match placed.Pma with
                // An empty formula shows no box to fill, so the cursor stands in it as a bar.
                | PlacedMA.Row children when children.IsEmpty -> bar 0
                | _ -> PlacedRule(placed.Width, placed.Ascent + placed.Descent, 0f<px>, -placed.Descent)
            PlacedCurs(placed, PlacedMACurs.Fills caret)
        | MACurs.Row(before, inner, after) ->
            let count = before.Length
            if inner.IsCursorOrEmpty then
                PlacedCurs(
                    placed,
                    PlacedMACurs.Between(first count, bar count, rest count))
            else
                PlacedCurs(
                    placed,
                    PlacedMACurs.Within(
                        first count,
                        PlacedCurs.Of(inner, children.[count]),
                        rest (count + 1)))
        | MACurs.ScriptMainSuper(main, _, _) ->
            let struct(b, super, sub) = scripts()
            PlacedCurs(
                placed,
                PlacedMACurs.ScriptMainSuper(PlacedCurs.Of(main, b), script(super, "superscript"), sub))
        | MACurs.ScriptMainSub(main, _) ->
            let struct(b, _, sub) = scripts()
            PlacedCurs(
                placed,
                PlacedMACurs.ScriptMainSub(PlacedCurs.Of(main, b), script(sub, "subscript")))
        | MACurs.ScriptSuper(_, super, _) ->
            let struct(b, above, sub) = scripts()
            PlacedCurs(
                placed,
                PlacedMACurs.ScriptSuper(b, PlacedCurs.Of(super, script(above, "superscript")), sub))
        | MACurs.ScriptSub(_, _, sub) ->
            let struct(b, super, below) = scripts()
            PlacedCurs(
                placed,
                PlacedMACurs.ScriptSub(b, super, PlacedCurs.Of(sub, script(below, "subscript"))))
        | MACurs.FracNum(n, _) ->
            match placed.Pma with
            | PlacedMA.Frac(numerator, _, denominator) ->
                PlacedCurs(placed, PlacedMACurs.FracNum(PlacedCurs.Of(n, numerator), denominator))
            | _ -> wrong "fraction"
        | MACurs.FracDen(_, d) ->
            match placed.Pma with
            | PlacedMA.Frac(numerator, _, denominator) ->
                PlacedCurs(placed, PlacedMACurs.FracDen(numerator, PlacedCurs.Of(d, denominator)))
            | _ -> wrong "fraction"
        | MACurs.Bracketed(_, inner, _) ->
            match placed.Pma with
            | PlacedMA.Bracketed(brackets, _, held, _, completion) ->
                PlacedCurs(
                    placed,
                    PlacedMACurs.Bracketed(brackets, PlacedCurs.Of(inner, held), completion))
            | _ -> wrong "bracket"
        | MACurs.RootNDegree(n, _) ->
            match placed.Pma with
            | PlacedMA.RootN(degree, _, _, radicand) ->
                PlacedCurs(placed, PlacedMACurs.RootNDegree(PlacedCurs.Of(n, degree), radicand))
            | _ -> wrong "root"
        | MACurs.RootNMain(_, x) ->
            match placed.Pma with
            | PlacedMA.RootN(degree, _, _, radicand) ->
                PlacedCurs(placed, PlacedMACurs.RootNMain(degree, PlacedCurs.Of(x, radicand)))
            | _ -> wrong "root"
        | MACurs.Sqrt x ->
            match placed.Pma with
            | PlacedMA.Sqrt(_, _, radicand) ->
                PlacedCurs(placed, PlacedMACurs.Sqrt(PlacedCurs.Of(x, radicand)))
            | _ -> wrong "square root"

type PlacedCurs with
    /// How far a point is from an atom where it was laid out.
    static member private AwayFrom(placed: Placed, x: float32<px>, y: float32<px>) =
        PlacedCurs.Away(
            placed.X,
            placed.X + placed.Width,
            placed.Y - placed.Descent,
            placed.Y + placed.Ascent,
            x,
            y)

    /// The cursor nearest a point inside one atom. ValueNone where a cursor cannot go inside it.
    static member private Inside(placed: Placed, x: float32<px>, y: float32<px>) =
        let nearest(slots: struct(Placed * (MACurs -> MACurs)) list) =
            let struct(slot, wrap) =
                slots |> List.minBy (fun struct(slot, _) -> PlacedCurs.AwayFrom(slot, x, y))
            wrap(PlacedCurs.NearestIn(slot, x - slot.X, y - slot.Y)) |> ValueSome
        let ma(placed: Placed) = placed.Pma.ToMA
        match placed.Pma with
        | PlacedMA.Frac(n, _, d) ->
            nearest [
                struct(n, fun inner -> MACurs.FracNum(inner, ma d))
                struct(d, fun inner -> MACurs.FracDen(ma n, inner))
            ]
        | PlacedMA.ScriptSuper(main, super, sub) ->
            let below = sub |> ValueOption.map ma
            [
                struct(main, fun inner -> MACurs.ScriptMainSuper(inner, ma super, below))
                struct(super, fun inner -> MACurs.ScriptSuper(ma main, inner, below))
                match sub with
                | ValueSome sub ->
                    struct(sub, fun inner -> MACurs.ScriptSub(ma main, ValueSome(ma super), inner))
                | ValueNone -> ()
            ]
            |> nearest
        | PlacedMA.ScriptSub(main, sub) ->
            nearest [
                struct(main, fun inner -> MACurs.ScriptMainSub(inner, ma sub))
                struct(sub, fun inner -> MACurs.ScriptSub(ma main, ValueNone, inner))
            ]
        | PlacedMA.Bracketed(brackets, _, held, _, completion) ->
            nearest [ struct(held, fun inner -> MACurs.Bracketed(brackets, inner, completion)) ]
        | PlacedMA.RootN(degree, _, _, radicand) ->
            nearest [
                struct(degree, fun inner -> MACurs.RootNDegree(inner, ma radicand))
                struct(radicand, fun inner -> MACurs.RootNMain(ma degree, inner))
            ]
        | PlacedMA.Sqrt(_, _, radicand) ->
            nearest [ struct(radicand, MACurs.Sqrt) ]
        | PlacedMA.Row _ | PlacedMA.Char _ | PlacedMA.BoldVar _ | PlacedMA.Blackboard _
        | PlacedMA.UprightD _ | PlacedMA.Function _ | PlacedMA.BigOp _ | PlacedMA.Accented _ | PlacedMA.Spanned _ | PlacedMA.Overline _
        | PlacedMA.Underline _ | PlacedMA.Stack _ | PlacedMA.Table _ | PlacedMA.Coloured _
        | PlacedMA.Text _ | PlacedMA.Space _ | PlacedMA.Placeholder _ -> ValueNone

    /// The cursor nearest a point in a slot, which every formula is and so always holds one.
    static member private NearestIn(placed: Placed, x: float32<px>, y: float32<px>): MACurs =
        if placed.Pma.IsPlaceholder then MACurs.CursorOrEmpty else

        let children = PlacedCurs.Children placed
        let ma(placed: Placed) = placed.Pma.ToMA
        let first(count: int) = children.RemoveRange(count, children.Length - count) |> ImmArray.map ma
        let rest(count: int) = children.RemoveRange(0, count) |> ImmArray.map ma
        // The start of the slot, so that a point which is no number at all still finds a cursor
        // standing in this formula rather than one standing in an empty one.
        let mutable best = MACurs.MakeRow(first 0, MACurs.CursorOrEmpty, rest 0)
        let mutable closest = System.Single.MaxValue * 1f<px>
        let consider(distance: float32<px>, curs: MACurs) =
            if distance < closest then
                closest <- distance
                best <- curs
        for count in 0 .. children.Length do
            // A gap offers the bar it would draw, so a click high in a numerator is not drawn down
            // to the row the fraction sits in.
            let bar = PlacedCurs.Bar(PlacedCurs.Pen(children, count), placed.EmSize)
            let away =
                PlacedCurs.Away(bar.X, bar.X + bar.Width, bar.Y, bar.Y + bar.Thickness, x, y)
            consider(away, MACurs.MakeRow(first count, MACurs.CursorOrEmpty, rest count))
        for i in 0 .. children.Length - 1 do
            let child = children.[i]
            match PlacedCurs.Inside(child, x - child.X, y - child.Y) with
            | ValueSome inner ->
                consider(PlacedCurs.AwayFrom(child, x, y), MACurs.MakeRow(first i, inner, rest (i + 1)))
            | ValueNone -> ()
        best

    /// The cursor nearest a point, in pixels from the formula's origin with y upwards.
    static member Nearest(placed: Placed, x: float32<px>, y: float32<px>) =
        PlacedCurs.Of(PlacedCurs.NearestIn(placed, x, y), placed)
