namespace ModeMath

open System.IO

module private Alphabet =
    /// An unassigned slot keeps its alphabet indexable but is no letter.
    let at(alphabet: Glyph array, index: int) =
        let glyph = alphabet.[index]
        if glyph.Id = 0 then ValueNone else ValueSome glyph

/// The alphabets a variable is set in. ValueNone means not a letter, never a gap in the font.
module Letters =
    /// Math italic, in which variables are set. Capital Greek is upright, so it is not here.
    let italic(c: char) =
        if c >= 'a' && c <= 'z' then Alphabet.at(Alphabets.italicSmall, int c - int 'a')
        elif c >= 'A' && c <= 'Z' then Alphabet.at(Alphabets.italicCapital, int c - int 'A')
        elif c >= 'α' && c <= 'ω' then Alphabet.at(Alphabets.italicGreek, int c - 0x03B1)
        else
            let mutable found = ValueNone
            for shape in Alphabets.italicShapes do
                if shape.Codepoint = int c then found <- ValueSome shape.Glyph
            found

    /// Math bold italic, in which bold variables are set.
    let bold(c: char) =
        if c >= 'a' && c <= 'z' then Alphabet.at(Alphabets.boldSmall, int c - int 'a')
        elif c >= 'A' && c <= 'Z' then Alphabet.at(Alphabets.boldCapital, int c - int 'A')
        else ValueNone

    /// Upright, in which function names and capital Greek are set.
    let upright(c: char) =
        if c >= 'a' && c <= 'z' then Alphabet.at(Alphabets.uprightSmall, int c - int 'a')
        elif c >= 'A' && c <= 'Z' then Alphabet.at(Alphabets.uprightCapital, int c - int 'A')
        elif c >= 'Α' && c <= 'Ω' then Alphabet.at(Alphabets.uprightGreekCapital, int c - 0x0391)
        else ValueNone

module Digits =
    /// The upright figure a digit is set in. ValueNone where c is not a digit.
    let glyph(c: char) =
        if c >= '0' && c <= '9' then ValueSome Alphabets.digits.[int c - int '0'] else ValueNone

[<AbstractClass; Sealed>]
type MathFont =
    /// Punctuation and symbols. ValueNone where the character is outside the repertoire.
    static member OfChar(c: char) =
        let all = Repertoire.all
        let codepoint = int c
        let mutable low = 0
        let mutable high = all.Length - 1
        let mutable found = ValueNone
        while found.IsNone && low <= high do
            let middle = low + (high - low) / 2
            let candidate = all.[middle].Codepoint
            if candidate = codepoint then found <- ValueSome all.[middle].Glyph
            elif candidate < codepoint then low <- middle + 1
            else high <- middle - 1
        found

    /// The font file the metrics were generated from, whose glyph ids Glyph.Id refers to.
    static member OpenFontFile(): Stream =
        typeof<Glyph>.Assembly.GetManifestResourceStream "ModeMath.latinmodern-math.otf"
