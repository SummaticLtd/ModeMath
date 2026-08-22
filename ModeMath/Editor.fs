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

    /// The atom before the cursor given the closing bracket it waits for, where it waits for one.
    /// Without a bracket it takes the tentative one it was already drawing.
    let closingLast(bracket: Bracket voption) (before: ImmutableArray<MA>) =
        if before.IsEmpty then before
        else
            let last = before.Length - 1
            match before.[last] with
            | MA.Bracketed(b, x, BracketCompletion.Left) ->
                let right = bracket |> ValueOption.defaultValue b.Right
                before.SetItem(last, MA.Bracketed(Brackets(b.Left, right), x, BracketCompletion.Completed))
            | _ -> before

    /// The same for the atom after the cursor and the opening bracket it waits for.
    let openingFirst(bracket: Bracket voption) (after: ImmutableArray<MA>) =
        if after.IsEmpty then after
        else
            match after.[0] with
            | MA.Bracketed(b, x, BracketCompletion.Right) ->
                let left = bracket |> ValueOption.defaultValue b.Left
                after.SetItem(0, MA.Bracketed(Brackets(left, b.Right), x, BracketCompletion.Completed))
            | _ -> after

    /// Both brackets the cursor stands beside settled, as putting anything past one does.
    let settling(before: ImmutableArray<MA>, after: ImmutableArray<MA>) =
        struct (closingLast ValueNone before, openingFirst ValueNone after)

    /// The cursor with the tentative brackets it has moved out past made good.
    let settled() = cursor.ToMACurs.Rewrite settling

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
    member _.Type(character: char) = over((settled()).AddAlphanumeric character)

    /// A formula put in at the cursor, which the cursor then stands after.
    member _.Insert(addition: MA) = over((settled()).AddMACurs(MACurs.AtEnd addition))

    /// A fraction over the term before the cursor, which then stands in the denominator. Where no
    /// term stands there the fraction is empty and the cursor goes in the numerator instead.
    member _.InsertFraction =
        let divided(numerator: MA) =
            if numerator.IsEmpty then MACurs.FracNum(MACurs.CursorOrEmpty, MA.Empty)
            else MACurs.FracDen(numerator, MACurs.CursorOrEmpty)
        over((settled()).ReplaceBefore(term, divided))

    /// A square root put in at the cursor, which then stands inside it.
    member _.InsertSqrt = over((settled()).AddMACurs(MACurs.Sqrt MACurs.CursorOrEmpty))

    /// A root put in at the cursor, which then stands in its degree.
    member _.InsertRoot = over((settled()).AddMACurs(MACurs.RootNDegree(MACurs.CursorOrEmpty, MA.Empty)))

    /// An opening bracket typed at the cursor. A bracketed atom waiting for one takes it, whether
    /// the cursor stands in that atom or just before it, and what was before the cursor comes out of
    /// it. Otherwise what is after the cursor is taken into a new atom the cursor then starts, whose
    /// closing bracket is drawn tentative until one is typed.
    member _.InsertBracket(brackets: Brackets) =
        let standing = cursor.ToMACurs
        let given(before: ImmutableArray<MA>, after: ImmutableArray<MA>) =
            struct (before, openingFirst (ValueSome brackets.Left) after)
        let taken = standing.Rewrite given
        if taken <> standing then over taken
        else
            let curs = settled()
            match curs.OpenBracket brackets.Left with
            | ValueSome opened -> over opened
            | ValueNone ->
                let enclosing(inner: MA) =
                    MACurs.Bracketed(brackets, MACurs.AtStart inner, BracketCompletion.Left)
                over(curs.ReplaceAfter enclosing)

    /// A closing bracket typed at the cursor. A bracketed atom waiting for one takes it, whether
    /// the cursor stands in that atom or just after it, and what was after the cursor comes out of
    /// it. Otherwise what is before the cursor is taken into a new atom the cursor then stands after,
    /// whose opening bracket is drawn tentative.
    member _.CloseBracket(bracket: Bracket) =
        let standing = cursor.ToMACurs
        let given(before: ImmutableArray<MA>, after: ImmutableArray<MA>) =
            struct (closingLast (ValueSome bracket) before, after)
        let taken = standing.Rewrite given
        if taken <> standing then over taken
        else
            let curs = settled()
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
        over((settled()).ReplaceBefore(one, superscripted))

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
        over((settled()).ReplaceBefore(one, subscripted))

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
