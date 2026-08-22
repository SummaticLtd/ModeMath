namespace ModeMath

open System.Collections.Immutable

/// A formula being edited: laid out with the cursor in it, and what to lay it out again with.
[<Sealed>]
type Editor(layout: Layout, cursor: PlacedCurs) =
    let over(curs: MACurs) = Editor(layout, layout.Of curs)

    /// Whether an atom carries a term on rather than breaking it, asked of the side facing the cursor.
    let carriesOn(ma: MA) =
        match Conventions.atomClasses ma with
        | ValueSome(struct (_, right)) ->
            match right with
            | AtomClass.Ordinary | AtomClass.Close | AtomClass.Inner -> true
            | _ -> false
        // A gap is no atom, and a term does not reach across one.
        | ValueNone -> false

    /// The one atom a script goes on.
    let one(before: ImmutableArray<MA>) = min 1 before.Length

    /// Everything a closing bracket with no opening one takes in.
    let all(before: ImmutableArray<MA>) = before.Length

    /// The term a fraction takes up, which reaches back to whatever last broke one.
    let term(before: ImmutableArray<MA>) =
        let mutable count = 0
        while count < before.Length && carriesOn before.[before.Length - 1 - count] do
            count <- count + 1
        count

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

    /// A fraction over the term before the cursor, which then stands in the denominator. Where no
    /// term stands there the fraction is empty and the cursor goes in the numerator instead.
    member _.InsertFraction =
        let divided(numerator: MA) =
            if numerator.IsEmpty then MACurs.FracNum(MACurs.CursorOrEmpty, MA.Empty)
            else MACurs.FracDen(numerator, MACurs.CursorOrEmpty)
        over(cursor.ToMACurs.ReplaceBefore(term, divided))

    /// A square root put in at the cursor, which then stands inside it.
    member _.InsertSqrt = over(cursor.ToMACurs.AddMACurs(MACurs.Sqrt MACurs.CursorOrEmpty))

    /// A root put in at the cursor, which then stands in its degree.
    member _.InsertRoot = over(cursor.ToMACurs.AddMACurs(MACurs.RootNDegree(MACurs.CursorOrEmpty, MA.Empty)))

    /// An opening bracket typed at the cursor. Where the atom it stands in is waiting for one, that
    /// is where it goes and what was before it comes out. Otherwise what is after it is taken into
    /// a new atom whose closing bracket is drawn tentative, and the cursor stands at its start.
    member _.InsertBracket(brackets: Brackets) =
        let curs = cursor.ToMACurs
        match curs.OpenBracket brackets.Left with
        | ValueSome opened -> over opened
        | ValueNone ->
            let enclosing(inner: MA) =
                MACurs.Bracketed(brackets, MACurs.AtStart inner, BracketCompletion.Left)
            over(curs.ReplaceAfter enclosing)

    /// A closing bracket typed at the cursor, which closes the bracketed atom it stands in and then
    /// stands after it. Where it stands in none, what is before it is taken into a new one whose
    /// opening bracket is drawn as tentative.
    member _.CloseBracket(bracket: Bracket) =
        let curs = cursor.ToMACurs
        match curs.CloseBracket bracket with
        | ValueSome closed -> over closed
        | ValueNone ->
            let enclosed(inner: MA) =
                MACurs.AtEnd(MA.Bracketed(Brackets.Matching bracket, inner, BracketCompletion.Right))
            over(curs.ReplaceBefore(all, enclosed))

    /// A bracket that opens and closes alike, as a vertical bar does: it closes a bracketed atom of
    /// its own shape that is waiting for a closing bracket, and opens one otherwise.
    member t.InsertBar(bracket: Bracket) =
        match cursor.ToMACurs.Unclosed with
        | ValueSome brackets when brackets.Left = bracket -> t.CloseBracket bracket
        | ValueSome _ | ValueNone -> t.InsertBracket(Brackets.Matching bracket)

    /// A superscript on the atom before the cursor, which then stands in it. An atom already
    /// carrying one keeps it and is entered rather than being set under a second.
    member _.InsertSuperscript =
        let superscripted(atom: MA) =
            match atom with
            | MA.ScriptSuper(main, super, sub) -> MACurs.ScriptSuper(main, MACurs.AtEnd super, sub)
            | MA.ScriptSub(main, sub) -> MACurs.ScriptSuper(main, MACurs.CursorOrEmpty, ValueSome sub)
            | _ -> MACurs.ScriptSuper(atom, MACurs.CursorOrEmpty, ValueNone)
        over(cursor.ToMACurs.ReplaceBefore(one, superscripted))

    /// A subscript on the atom before the cursor, which then stands in it. An atom already carrying
    /// one keeps it and is entered rather than being set over a second.
    member _.InsertSubscript =
        let subscripted(atom: MA) =
            match atom with
            | MA.ScriptSuper(main, super, ValueSome sub) ->
                MACurs.ScriptSub(main, ValueSome super, MACurs.AtEnd sub)
            | MA.ScriptSuper(main, super, ValueNone) ->
                MACurs.ScriptSub(main, ValueSome super, MACurs.CursorOrEmpty)
            | MA.ScriptSub(main, sub) -> MACurs.ScriptSub(main, ValueNone, MACurs.AtEnd sub)
            | _ -> MACurs.ScriptSub(atom, ValueNone, MACurs.CursorOrEmpty)
        over(cursor.ToMACurs.ReplaceBefore(one, subscripted))

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
