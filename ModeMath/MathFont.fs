namespace ModeMath

open System.IO

/// The alphabets a variable is set in. ValueNone means not a letter, never a gap in the font.
module Letters =
    /// Math italic, in which variables are set. Capital Greek is absent, being set upright.
    let italic(c: char) =
        if c >= 'a' && c <= 'z' then ValueSome Alphabets.italicSmall.[int c - int 'a']
        elif c >= 'A' && c <= 'Z' then ValueSome Alphabets.italicCapital.[int c - int 'A']
        elif c >= 'α' && c <= 'ω' then ValueSome Alphabets.italicGreek.[int c - 0x03B1]
        else
            let mutable found = ValueNone
            for shape in Alphabets.italicShapes do
                if shape.Codepoint = int c then found <- ValueSome shape.Glyph
            found

    /// Math bold italic, in which bold variables are set.
    let bold(c: char) =
        if c >= 'a' && c <= 'z' then ValueSome Alphabets.boldSmall.[int c - int 'a']
        elif c >= 'A' && c <= 'Z' then ValueSome Alphabets.boldCapital.[int c - int 'A']
        else ValueNone

[<AbstractClass; Sealed>]
type MathFont =
    /// ValueNone where the font has no glyph for the codepoint.
    static member OfCodepoint(codepoint: int) =
        let all = Codepoints.all
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

    static member OfChar(c: char) = MathFont.OfCodepoint(int c)

    /// The font file the metrics were generated from, whose glyph ids Glyph.Id refers to.
    static member OpenFontFile(): Stream =
        typeof<Glyph>.Assembly.GetManifestResourceStream "ModeMath.latinmodern-math.otf"
