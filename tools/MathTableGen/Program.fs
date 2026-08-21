module MathTableGen.Program

open System
open System.IO
open System.Security.Cryptography
open SkiaSharp

let private mathTag =
    uint32 (int 'M' <<< 24 ||| (int 'A' <<< 16) ||| (int 'T' <<< 8) ||| int 'H')

/// The blackboard face holds .notdef and then A to Z, its glyph ids being read in that order.
let private blackboardGlyphCount = 27

/// The letters the blackboard face maps, which check the order the rest of its glyphs are taken in.
let private blackboardMapped =
    [ 0x2102, 'C'; 0x210D, 'H'; 0x2115, 'N'; 0x2119, 'P'; 0x211A, 'Q'; 0x211D, 'R'; 0x2124, 'Z' ]

/// A face at its design size, hinted as the library draws it.
let private designFont(typeface: SKTypeface) =
    let font = new SKFont(typeface, float32 typeface.UnitsPerEm)
    font.Hinting <- SKFontHinting.None
    font.Subpixel <- true
    font

/// The advance and ink bounds of every glyph of a face, in its own design units.
let private measure(font: SKFont, glyphCount: int) =
    let glyphIds = Array.init glyphCount uint16
    let widths = Array.zeroCreate<float32> glyphCount
    let bounds = Array.zeroCreate<SKRect> glyphCount
    font.GetGlyphWidths(ReadOnlySpan glyphIds, Span widths, Span bounds)
    let advances =
        widths
        |> Array.map (fun width ->
            if Math.Abs(width - MathF.Round width) > 0.01f then
                failwith $"Non-integral advance {width} at font size {font.Size}"
            int (MathF.Round width))
    // Curve extrema land off the design grid, so bounds are rounded outwards.
    let tops = bounds |> Array.map (fun b -> int (ceil -b.Top))
    let bottoms = bounds |> Array.map (fun b -> int (floor -b.Bottom))
    advances, tops, bottoms

/// Codepoints outside the surrogate range, which no font maps.
let private allCodepoints =
    [|
        for c in 0x20 .. 0x10FFFF do
            if c < 0xD800 || c > 0xDFFF then yield c
    |]

let private writeConstants(w: Writer, table: MathTable, unitsPerEm: int) =
    w.Line "/// Positioning values from the MATH table, in font design units."
    w.Line "module MathConstants ="
    w.Line "    [<Literal>]"
    w.Line $"    let UnitsPerEm = {unitsPerEm}f<du>"
    for i in 0 .. MathConstantNames.table.Length - 1 do
        let name = MathConstantNames.table.[i]
        let unit = if MathConstantNames.percentages.Contains name then "f" else "f<du>"
        w.Line "    [<Literal>]"
        w.Line $"    let {name} = {table.Constants.[i]}{unit}"
    w.Line "    [<Literal>]"
    w.Line $"    let MinConnectorOverlap = {table.MinConnectorOverlap}f<du>"

[<EntryPoint>]
let main(args: string array): int =
    let fontPath = args.[0]
    let blackboardPath = args.[1]
    let outputPath = args.[2]
    let fontBytes = File.ReadAllBytes fontPath
    let blackboardBytes = File.ReadAllBytes blackboardPath
    use typeface = SKTypeface.FromFile fontPath
    use blackboardTypeface = SKTypeface.FromFile blackboardPath
    let table = MathTable(typeface.GetTableData mathTag)
    if table.KernedGlyphCount > 0 then
        failwith $"{table.KernedGlyphCount} glyphs carry MathKernInfo, which is not generated"
    let unitsPerEm = typeface.UnitsPerEm
    if blackboardTypeface.UnitsPerEm <> unitsPerEm then
        failwith
            $"the blackboard face is drawn on {blackboardTypeface.UnitsPerEm} units to the em, \
                not {unitsPerEm}"

    use font = designFont typeface
    use blackboardFont = designFont blackboardTypeface
    let advances, tops, bottoms = measure(font, typeface.GlyphCount)
    let italics = dict table.ItalicsCorrections
    let attachments = dict table.TopAccentAttachments

    /// One glyph of the math face as the literal that reconstructs it.
    let glyph(id: int) =
        let italic = match italics.TryGetValue id with | true, value -> value | false, _ -> 0
        let attachment =
            match attachments.TryGetValue id with
            | true, value -> value
            | false, _ -> advances.[id] / 2
        $"Glyph({id}, {advances.[id]}f<du>, {tops.[id]}f<du>, {bottoms.[id]}f<du>, {italic}f<du>, \
            {attachment}f<du>)"

    if blackboardTypeface.GlyphCount <> blackboardGlyphCount then
        failwith
            $"the blackboard face has {blackboardTypeface.GlyphCount} glyphs, not the \
                {blackboardGlyphCount} its letters are read from"
    let blackboardIds =
        blackboardFont.GetGlyphs(ReadOnlySpan(blackboardMapped |> List.map fst |> List.toArray))
    for i in 0 .. blackboardMapped.Length - 1 do
        let codepoint, letter = blackboardMapped.[i]
        let expected = int letter - int 'A' + 1
        if int blackboardIds.[i] <> expected then
            failwith
                $"the blackboard face draws U+{codepoint:X4} with glyph {blackboardIds.[i]}, \
                    not {expected}"
    let blackboardAdvances, blackboardTops, blackboardBottoms =
        measure(blackboardFont, blackboardTypeface.GlyphCount)

    /// The blackboard face carries no MATH table, so it leans nowhere and attaches at its midpoint.
    let blackboardGlyph(id: int) =
        let advance = blackboardAdvances.[id]
        $"Glyph(Face.Blackboard, {id}, {advance}f<du>, {blackboardTops.[id]}f<du>, \
            {blackboardBottoms.[id]}f<du>, 0f<du>, {advance / 2}f<du>)"

    let mapped = font.GetGlyphs(ReadOnlySpan allCodepoints)
    let byCodepoint =
        [| for i in 0 .. allCodepoints.Length - 1 do
            if mapped.[i] <> 0us then yield allCodepoints.[i], int mapped.[i] |]
    let glyphOf = dict byCodepoint

    let resolve(codepoint: int) =
        match glyphOf.TryGetValue codepoint with
        | true, id -> id
        | false, _ -> failwith $"the font has no glyph for U+{codepoint:X4}"

    let holes = Set.ofList Named.alphabetHoles
    /// An unassigned slot keeps the alphabet indexable and is reported as no letter at all.
    let letter(codepoint: int) =
        if holes.Contains codepoint then "Glyph(0, 0f<du>, 0f<du>, 0f<du>, 0f<du>, 0f<du>)"
        else glyph (resolve codepoint)

    let exceptions = dict Named.alphabetExceptions
    let substituted(codepoint: int) =
        match exceptions.TryGetValue codepoint with
        | true, replacement -> replacement
        | false, _ -> codepoint

    let w = Writer()
    w.Line "namespace ModeMath"
    w.Blank()
    w.Line
        $"// Generated by tools/MathTableGen from {Path.GetFileName fontPath} and \
            {Path.GetFileName blackboardPath}. Do not edit."
    w.Blank()
    writeConstants(w, table, unitsPerEm)
    w.Blank()

    w.Line "/// The alphabets a variable is set in, indexed from the first letter of each."
    w.Line "module internal Alphabets ="
    for name, first, count, summary in Named.alphabets do
        w.Line $"    /// {summary}"
        w.Array(
            name,
            "Glyph",
            seq { for i in 0 .. count - 1 do yield letter(substituted(first + i)) })
    w.Line "    /// The italic shapes Unicode keeps outside the alphabets."
    w.Array(
        "italicShapes",
        "CG",
        Named.italicShapes |> Seq.map (fun (c, italic) -> $"CG({c}, {glyph (resolve italic)})"))
    w.Line "    /// Blackboard bold A to Z, which the second face is carried for."
    w.Array("blackboardCapital", "Glyph", seq { for id in 1 .. 26 -> blackboardGlyph id })
    w.Blank()

    w.Line "/// Every character a formula may hold that no alphabet covers, ascending by codepoint."
    w.Line "module internal Repertoire ="
    let repertoire = Named.repertoire |> Seq.map int |> Seq.distinct |> Seq.sort |> Seq.toArray
    w.Array("all", "CG", repertoire |> Seq.map (fun c -> $"CG({c}, {glyph (resolve c)})"))
    w.Blank()

    /// A delimiter with its sizes and assembly, taken from the vertical constructions.
    let stretchy(codepoint: int) =
        let id = resolve codepoint
        match table.VerticalConstructions |> Array.tryFind (fun c -> c.Glyph = id) with
        | None -> failwith $"U+{codepoint:X4} does not stretch vertically"
        | Some construction ->
            let sizes =
                construction.Variants
                |> Array.map (fun v -> $"StretchSize({glyph v.Glyph}, {v.Advance}f<du>)")
            let parts =
                match construction.Assembly with
                | ValueNone -> Array.empty
                | ValueSome assembly ->
                    assembly.Parts
                    |> Array.map (fun p ->
                        let extender = if p.IsExtender then "true" else "false"
                        $"AssemblyPart({glyph p.Glyph}, {p.StartConnector}f<du>, \
                            {p.EndConnector}f<du>, {p.FullAdvance}f<du>, {extender})")
            id, sizes, parts

    let writeStretchy(moduleName: string, summary: string, entries: (string * int * string) list) =
        w.Line $"/// {summary}"
        w.Line $"module {moduleName} ="
        for name, codepoint, doc in entries do
            let id, sizes, parts = stretchy codepoint
            w.Line $"    /// {doc}"
            w.Line $"    let {name} ="
            w.Line "        StretchyGlyph("
            w.Line $"            {glyph id},"
            w.Nested(sizes, ",")
            w.Nested(parts, ")")
        w.Blank()

    let writeNamed(moduleName: string, summary: string, entries: (string * int * string) list) =
        w.Line $"/// {summary}"
        w.Line $"module {moduleName} ="
        for name, codepoint, doc in entries do
            w.Line $"    /// {doc}"
            w.Line $"    let {name} = {glyph (resolve codepoint)}"
        w.Blank()

    writeNamed("Symbols", "Single glyphs the library refers to that no alphabet covers.", Named.symbols)
    writeNamed(
        "Operators",
        "Operators whose codepoints are easy to mistake for the keys that resemble them.",
        Named.operators)
    writeNamed("Accents", "Accents, which are placed by the point they attach over.", Named.accents)
    writeStretchy("Delimiters", "Delimiters, which grow to the height of what they hold.", Named.delimiters)
    writeStretchy(
        "BigOperators",
        "Large operators, which take a taller glyph in display style and may carry limits.",
        Named.bigOperators)
    writeStretchy("Radicals", "Roots, whose surd grows to cover the radicand.", Named.radicals)

    w.Line "module internal FontFile ="
    w.Line "    /// The math face, which every glyph but a blackboard bold capital comes from."
    w.Line $"    let byteLength = {fontBytes.Length}"
    w.Line $"    let sha256 = \"{Convert.ToHexStringLower(SHA256.HashData fontBytes)}\""
    w.Line "    /// The blackboard face."
    w.Line $"    let blackboardByteLength = {blackboardBytes.Length}"
    w.Line
        $"    let blackboardSha256 = \"{Convert.ToHexStringLower(SHA256.HashData blackboardBytes)}\""

    File.WriteAllText(outputPath, w.Text, Text.UTF8Encoding false)
    printfn $"{outputPath}: {repertoire.Length} characters in the repertoire, {w.Text.Length} bytes"
    0
