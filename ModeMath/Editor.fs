namespace ModeMath

open System.Collections.Generic
open System.Collections.Immutable

/// A bracket a key types, which is a shape whose opening and closing forms are keys of their own.
type BracketKey =
    | Round = 0
    | Square = 1
    | Curly = 2

module private BracketKeys =
    /// The shape it types.
    let shape(key: BracketKey) =
        match key with
        | BracketKey.Round -> Bracket.Normal
        | BracketKey.Square -> Bracket.Square
        | BracketKey.Curly -> Bracket.Curly

/// A key an editor answers, which is every way a keyboard reaches a formula.
[<RequireQualifiedAccess>]
type MathKey =
    /// A character itself, which is any the font can draw rather than a list of the ones it knows.
    | Character of char
    | Move of Direction
    | Backspace
    | Delete
    | Fraction
    | Sqrt
    /// A root with a degree, which the cursor starts in.
    | Root
    | Superscript
    | Subscript
    /// An opening bracket, whose closing one is drawn faint in the same shape until one is typed.
    | Open of BracketKey
    /// A closing bracket, which need not be the shape the group was opened with.
    | Close of BracketKey
    /// The bar, which opens and closes alike and so is neither on its own.
    | Bar

/// A formula being edited: laid out with the cursor in it, and what to lay it out again with.
[<Sealed>]
type EditorState(layout: Layout, cursor: PlacedCurs) =
    let over(curs: MACurs) = EditorState(layout, layout.Of curs)

    /// Whether an atom carries a term on rather than breaking it, asked of the side facing the cursor.
    let carriesOn(ma: MA) =
        match Conventions.atomClasses ma with
        | ValueSome(struct(_, right)) ->
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
        struct(closingLast ValueNone before, openingFirst ValueNone after)

    /// The cursor with the tentative brackets it has moved out past made good.
    let settled() = cursor.ToMACurs.Rewrite settling

    /// The term a fraction takes up, which reaches back to whatever last broke one.
    let term(before: ImmutableArray<MA>) =
        let mutable count = 0
        while count < before.Length && carriesOn before.[before.Length - 1 - count] do
            count <- count + 1
        count

    /// A formula opened for editing with the cursor at its left-hand end.
    new(layout: Layout, formula: MA) = EditorState(layout, layout.Of(MACurs.AtStart formula))

    /// The same, with the cursor at its right-hand end.
    static member AtEnd(layout: Layout, formula: MA) = EditorState(layout, layout.Of(MACurs.AtEnd formula))

    /// Everything drawn, which is the same whatever the cursor is doing.
    member _.Placed = cursor.Placed

    /// Where the cursor is, in pixels from the formula's origin.
    member _.Caret = cursor.Caret

    /// What the formula and the cursor cover together, which does not change as the cursor moves.
    member _.Bounds = cursor.Bounds

    /// The cursor, which a Painter draws and a click is measured against.
    member _.Cursor = cursor

    /// The formula as it stands, with no cursor in it.
    member _.Formula = cursor.Placed.Pma.ToMA

    /// What this was laid out with, which another editor at the same size is built over.
    member _.Layout = layout

    /// The cursor put at a point, from the formula's origin with y upwards. Lays nothing out.
    member _.Click(x: float32<px>, y: float32<px>) =
        EditorState(layout, PlacedCurs.Nearest(cursor.Placed, x, y))

    /// ValueNone at that end of the formula, so a caller can pass the key on. Lays nothing out.
    member private _.Move(direction: Direction) =
        cursor.ToMACurs.Move direction
        |> ValueOption.map (fun moved -> EditorState(layout, PlacedCurs.Of(moved, cursor.Placed)))

    /// A character typed at the cursor, which completes a function name where one is spelled out.
    /// ValueNone where the font cannot draw it, so that a caller can pass the key on.
    member private _.Type(character: char) =
        if (Glyphs.variable character).IsNone then ValueNone
        else over((settled()).AddAlphanumeric character) |> ValueSome

    /// A formula put in at the cursor, which the cursor then stands after.
    member _.Insert(addition: MA) = over((settled()).AddMACurs(MACurs.AtEnd addition))

    /// A fraction over the term before the cursor, which then stands in the denominator. Where no
    /// term stands there the fraction is empty and the cursor goes in the numerator instead.
    member private _.InsertFraction =
        let divided(numerator: MA) =
            if numerator.IsEmpty then MACurs.FracNum(MACurs.CursorOrEmpty, MA.Empty)
            else MACurs.FracDen(numerator, MACurs.CursorOrEmpty)
        over((settled()).ReplaceBefore(term, divided))

    /// A square root put in at the cursor, which then stands inside it.
    member private _.InsertSqrt = over((settled()).AddMACurs(MACurs.Sqrt MACurs.CursorOrEmpty))

    /// A root put in at the cursor, which then stands in its degree.
    member private _.InsertRoot = over((settled()).AddMACurs(MACurs.RootNDegree(MACurs.CursorOrEmpty, MA.Empty)))

    /// An opening bracket typed at the cursor. A bracketed atom waiting for one takes it, whether
    /// the cursor stands in that atom or just before it, and what was before the cursor comes out of
    /// it. Otherwise what is after the cursor is taken into a new atom the cursor then starts, whose
    /// closing bracket is drawn tentative until one is typed.
    member private _.InsertBracket(bracket: Bracket) =
        let standing = cursor.ToMACurs
        let given(before: ImmutableArray<MA>, after: ImmutableArray<MA>) =
            struct(before, openingFirst (ValueSome bracket) after)
        let taken = standing.Rewrite given
        if taken <> standing then over taken
        else
            let curs = settled()
            match curs.OpenBracket bracket with
            | ValueSome opened -> over opened
            | ValueNone ->
                let enclosing(inner: MA) =
                    MACurs.Bracketed(
                        Brackets.Matching bracket,
                        MACurs.AtStart inner,
                        BracketCompletion.Left)
                over(curs.ReplaceAfter enclosing)

    /// A closing bracket typed at the cursor. A bracketed atom waiting for one takes it, whether
    /// the cursor stands in that atom or just after it, and what was after the cursor comes out of
    /// it. Otherwise what is before the cursor is taken into a new atom the cursor then stands after,
    /// whose opening bracket is drawn tentative.
    member private _.CloseBracket(bracket: Bracket) =
        let standing = cursor.ToMACurs
        let given(before: ImmutableArray<MA>, after: ImmutableArray<MA>) =
            struct(closingLast (ValueSome bracket) before, after)
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

    /// The bar, which opens and closes alike: it closes a bar group waiting for its closing bar,
    /// and opens one otherwise.
    member private t.InsertBar =
        match cursor.ToMACurs.Unclosed with
        | ValueSome brackets when brackets.Left = Bracket.Line -> t.CloseBracket Bracket.Line
        | ValueSome _ | ValueNone -> t.InsertBracket Bracket.Line

    /// A superscript on the atom before the cursor, which then stands in it. An atom already
    /// carrying one keeps it and is entered rather than being set under a second.
    member private _.InsertSuperscript =
        let superscripted(atom: MA) =
            match atom with
            | MA.ScriptSuper(main, super, sub) -> MACurs.ScriptSuper(main, MACurs.AtEnd super, sub)
            | MA.ScriptSub(main, sub) -> MACurs.ScriptSuper(main, MACurs.CursorOrEmpty, ValueSome sub)
            | _ -> MACurs.ScriptSuper(atom, MACurs.CursorOrEmpty, ValueNone)
        over((settled()).ReplaceBefore(one, superscripted))

    /// A subscript on the atom before the cursor, which then stands in it. An atom already carrying
    /// one keeps it and is entered rather than being set over a second.
    member private _.InsertSubscript =
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
    member private _.BackSpace =
        match cursor.ToMACurs.BackSpace with
        | Choice1Of2 curs -> over curs |> ValueSome
        | Choice2Of2 _ -> ValueNone

    /// ValueNone where there is nothing to the right to delete.
    member private _.Delete =
        match cursor.ToMACurs.Delete with
        | Choice1Of2 curs -> over curs |> ValueSome
        | Choice2Of2 _ -> ValueNone

    /// The formula a key leaves behind. ValueNone where the key had nothing to do here, so that a
    /// caller can pass it on to whatever else answers keys.
    member t.Press(key: MathKey) : EditorState voption =
        match key with
        | MathKey.Character character -> t.Type character
        | MathKey.Move direction -> t.Move direction
        | MathKey.Backspace -> t.BackSpace
        | MathKey.Delete -> t.Delete
        | MathKey.Fraction -> ValueSome t.InsertFraction
        | MathKey.Sqrt -> ValueSome t.InsertSqrt
        | MathKey.Root -> ValueSome t.InsertRoot
        | MathKey.Superscript -> ValueSome t.InsertSuperscript
        | MathKey.Subscript -> ValueSome t.InsertSubscript
        | MathKey.Open key -> ValueSome(t.InsertBracket(BracketKeys.shape key))
        | MathKey.Close key -> ValueSome(t.CloseBracket(BracketKeys.shape key))
        | MathKey.Bar -> ValueSome t.InsertBar

/// A formula being edited, which keeps the state it stands at now and the states it stood at before.
[<Sealed>]
type Editor(state: EditorState) =
    let past = Stack<MACurs>()
    let future = Stack<MACurs>()
    let mutable state = state
    /// Whether the last key typed a character, which is what carries a run of them into one undo.
    let mutable typing = false

    let at(layout: Layout, curs: MACurs) = EditorState(layout, layout.Of curs)

    /// The state given, with the one it replaces put by to undo to unless a run carries on.
    let edited(next: EditorState, carriesOn: bool) =
        if not (carriesOn && typing) then past.Push state.Cursor.ToMACurs
        future.Clear()
        state <- next
        typing <- carriesOn

    /// The cursor moved rather than the formula edited, which no undo goes back to.
    let moved(next: EditorState) =
        state <- next
        typing <- false

    /// A formula opened for editing, which the cursor stands after as putting one in does.
    new(layout: Layout, formula: MA) = Editor(EditorState.AtEnd(layout, formula))

    /// The formula with its cursor as they stand, which a Painter draws.
    member _.State = state

    /// What it is laid out at. Setting it lays the formula out again where the cursor stands.
    member _.Layout
        with get () = state.Layout
        and set (value: Layout) = state <- at(value, state.Cursor.ToMACurs)

    /// False where the key had nothing to do here, so that a caller can pass it on.
    member _.Press(key: MathKey) =
        match state.Press key with
        | ValueSome pressed ->
            match key with
            | MathKey.Move _ -> moved pressed
            | MathKey.Character _ -> edited(pressed, true)
            | _ -> edited(pressed, false)
            true
        | ValueNone -> false

    /// The cursor put at a point, from the formula's origin with y upwards.
    member _.Click(x: float32<px>, y: float32<px>) = moved(state.Click(x, y))

    member _.Insert(addition: MA) = edited(state.Insert addition, false)

    /// The formula as it stands. Setting it stands the cursor at the end of what is put in.
    member _.Formula
        with get () = state.Formula
        and set (formula: MA) = edited(EditorState.AtEnd(state.Layout, formula), false)

    /// A formula opened afresh, with nothing behind it to undo to, as a new one to edit rather than an edit.
    member _.Open(formula: MA) =
        past.Clear()
        future.Clear()
        state <- EditorState.AtEnd(state.Layout, formula)
        typing <- false

    /// False where nothing has been edited yet.
    member _.Undo() =
        if past.Count = 0 then false
        else
            future.Push state.Cursor.ToMACurs
            state <- at(state.Layout, past.Pop())
            typing <- false
            true

    /// False where nothing has been undone, which anything edited since undoing has emptied.
    member _.Redo() =
        if future.Count = 0 then false
        else
            past.Push state.Cursor.ToMACurs
            state <- at(state.Layout, future.Pop())
            typing <- false
            true
