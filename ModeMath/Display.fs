namespace ModeMath

open System.Collections.Immutable

/// Whether something is part of the formula or only offered, such as an unclosed bracket's partner.
type Ink =
    | Solid = 0
    | Tentative = 1

[<RequireQualifiedAccess>]
type Content =
    /// One glyph at the origin, drawn at the given size in points.
    | Glyph of glyph: Glyph * size: float32 * ink: Ink
    /// A rectangle filling the display's own extent.
    | Rule of ink: Ink
    | Children of ImmutableArray<Placed>

/// A laid-out MA, measured in points from its origin on the baseline, y upwards.
and Display(width: float32, ascent: float32, descent: float32, italicCorrection: float32, content: Content) =
    member _.Width = width
    member _.Ascent = ascent
    /// Depth below the baseline, positive downwards.
    member _.Descent = descent
    /// How far the last glyph leans past its advance.
    member _.ItalicCorrection = italicCorrection
    member _.Content = content
    member _.Height = ascent + descent

/// A display at an offset from its parent's origin.
and [<Struct>] Placed(display: Display, x: float32, y: float32) =
    member _.Display = display
    member _.X = x
    member _.Y = y

type Display with
    static member Empty = Display(0f, 0f, 0f, 0f, Content.Children ImmutableArray.Empty)

    static member OfRule(width: float32, thickness: float32, ink: Ink) =
        Display(width, thickness, 0f, 0f, Content.Rule ink)

    /// Bounds taken from the children, which are positioned relative to the new display's origin.
    static member OfChildren(width: float32, italicCorrection: float32, children: ImmutableArray<Placed>) =
        let mutable ascent = 0f
        let mutable descent = 0f
        for child in children do
            ascent <- max ascent (child.Y + child.Display.Ascent)
            descent <- max descent (child.Display.Descent - child.Y)
        Display(width, ascent, descent, italicCorrection, Content.Children children)
