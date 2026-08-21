namespace ModeMath

/// A formula being edited: laid out with the cursor in it, and what to lay it out again with.
[<Sealed>]
type Editor(layout: Layout, cursor: PlacedCurs) =
    let over(curs: MACurs) = Editor(layout, layout.Of curs)

    /// A formula opened for editing with the cursor at its left-hand end.
    new(layout: Layout, formula: MA) = Editor(layout, layout.Of(MACurs.AtStart formula))

    /// The same, with the cursor at its right-hand end.
    static member AtEnd(layout: Layout, formula: MA) = Editor(layout, layout.Of(MACurs.AtEnd formula))

    /// Everything drawn, which is the same whatever the cursor is doing.
    member _.Placed = cursor.Placed

    /// Where the cursor is, in pixels from the formula's origin.
    member _.Caret = cursor.Caret

    /// What the formula and the cursor cover together, which is taller than the formula alone.
    member _.Bounds = cursor.Bounds

    /// The cursor, which a Painter draws and a click is measured against.
    member _.Cursor = cursor

    /// The formula as it stands, with no cursor in it.
    member _.Formula = cursor.Placed.Pma.ToMA

    /// The cursor put at a point, from the formula's origin with y upwards. Lays nothing out.
    member _.Click(x: float32<px>, y: float32<px>) =
        Editor(layout, PlacedCurs.Nearest(cursor.Placed, x, y))

    /// ValueNone at that end of the formula, so a caller can pass the key on. Lays nothing out.
    member _.Move(direction: Direction) =
        cursor.ToMACurs.Move direction
        |> ValueOption.map (fun moved -> Editor(layout, PlacedCurs.Of(moved, cursor.Placed)))

    /// A character typed at the cursor, which completes a function name where one is spelled out.
    member _.Type(character: char) = over(cursor.ToMACurs.AddAlphanumeric character)

    /// A formula put in at the cursor, which the cursor then stands after.
    member _.Insert(addition: MA) = over(cursor.ToMACurs.AddMACurs(MACurs.AtEnd addition))

    /// A fraction put in at the cursor, which then stands in its numerator.
    member _.InsertFraction = over(cursor.ToMACurs.AddMACurs(MACurs.FracNum(MACurs.CursorOrEmpty, MA.Empty)))

    /// A square root put in at the cursor, which then stands inside it.
    member _.InsertSqrt = over(cursor.ToMACurs.AddMACurs(MACurs.Sqrt MACurs.CursorOrEmpty))

    /// A root put in at the cursor, which then stands in its degree.
    member _.InsertRoot = over(cursor.ToMACurs.AddMACurs(MACurs.RootNDegree(MACurs.CursorOrEmpty, MA.Empty)))

    /// A bracketed group put in at the cursor, which then stands inside it.
    member _.InsertBracket(brackets: Brackets) =
        over(cursor.ToMACurs.AddMACurs(MACurs.Bracketed(brackets, MACurs.CursorOrEmpty, BracketCompletion.Completed)))

    /// A superscript on the atom before the cursor, which then stands in it. An atom already
    /// carrying one keeps it and is entered rather than being set under a second.
    member _.InsertSuperscript =
        let superscripted(main: MA) =
            match main with
            | MA.ScriptSuper(main, super, sub) -> MACurs.ScriptSuper(main, MACurs.AtEnd super, sub)
            | MA.ScriptSub(main, sub) -> MACurs.ScriptSuper(main, MACurs.CursorOrEmpty, ValueSome sub)
            | main -> MACurs.ScriptSuper(main, MACurs.CursorOrEmpty, ValueNone)
        over(cursor.ToMACurs.ReplaceBefore superscripted)

    /// A subscript on the atom before the cursor, which then stands in it. An atom already carrying
    /// one keeps it and is entered rather than being set over a second.
    member _.InsertSubscript =
        let subscripted(main: MA) =
            match main with
            | MA.ScriptSuper(main, super, ValueSome sub) ->
                MACurs.ScriptSub(main, ValueSome super, MACurs.AtEnd sub)
            | MA.ScriptSuper(main, super, ValueNone) ->
                MACurs.ScriptSub(main, ValueSome super, MACurs.CursorOrEmpty)
            | MA.ScriptSub(main, sub) -> MACurs.ScriptSub(main, ValueNone, MACurs.AtEnd sub)
            | main -> MACurs.ScriptSub(main, ValueNone, MACurs.CursorOrEmpty)
        over(cursor.ToMACurs.ReplaceBefore subscripted)

    /// ValueNone where there is nothing to the left to delete, so that a caller can pass the key on.
    member _.BackSpace =
        match cursor.ToMACurs.BackSpace with
        | Choice1Of2 curs -> over curs |> ValueSome
        | Choice2Of2 _ -> ValueNone

    /// ValueNone where there is nothing to the right to delete.
    member _.Delete =
        match cursor.ToMACurs.Delete with
        | Choice1Of2 curs -> over curs |> ValueSome
        | Choice2Of2 _ -> ValueNone
