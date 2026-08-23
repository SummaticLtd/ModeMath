/// The glyphs the library reaches for by name, and the documentation each is generated with.
module MathTableGen.Named

/// Single glyphs the library refers to that no alphabet covers.
let symbols =
    [ "uprightD", int 'd', "The upright d of a derivative, which is not a variable." ]

/// Operators whose codepoints are easy to mistake for the keys that resemble them.
let operators =
    [
        "plus", int '+', "The plus sign."
        "minus", 0x2212, "The minus sign, which is longer than the hyphen."
        "times", 0x00D7, "The multiplication cross."
        "divide", 0x00F7, "The division sign."
        "equals", int '=', "The equals sign."
        "cdot", 0x22C5, "The centred dot of a product."
    ]

/// Delimiters, which grow to the height of what they hold.
let delimiters =
    [
        "roundLeft", int '(', "The opening round bracket."
        "roundRight", int ')', "The closing round bracket."
        "squareLeft", int '[', "The opening square bracket."
        "squareRight", int ']', "The closing square bracket."
        "curlyLeft", int '{', "The opening curly bracket."
        "curlyRight", int '}', "The closing curly bracket."
        "angleLeft", 0x27E8, "The opening angle bracket, as a bra opens."
        "angleRight", 0x27E9, "The closing angle bracket, as a ket closes."
        "floorLeft", 0x230A, "The opening floor bracket."
        "floorRight", 0x230B, "The closing floor bracket."
        "ceilingLeft", 0x2308, "The opening ceiling bracket."
        "ceilingRight", 0x2309, "The closing ceiling bracket."
        "bar", int '|', "The vertical bar of an absolute value, used on both sides."
        "slash", int '/', "The slash of a division set on one line, used on both sides."
    ]

/// Large operators, which take a taller glyph in display style and may carry limits.
let bigOperators =
    [
        "sum", 0x2211, "The summation sign."
        "product", 0x220F, "The product sign."
        "coproduct", 0x2210, "The coproduct sign."
        "integral", 0x222B, "The integral sign."
        "contourIntegral", 0x222E, "The contour integral sign."
        "union", 0x22C3, "The n-ary union sign."
        "intersection", 0x22C2, "The n-ary intersection sign."
    ]

/// Accents, which are combining marks: no advance, and ink drawn to the left of the origin.
let accents =
    [
        "hat", 0x0302, "The circumflex of \\hat."
        "tilde", 0x0303, "The tilde of \\tilde."
        "bar", 0x0304, "The macron of \\bar."
        "vec", 0x20D7, "The right arrow of \\vec."
        "dot", 0x0307, "The single dot of \\dot, a first derivative in time."
        "doubleDot", 0x0308, "The double dot of \\ddot, a second derivative in time."
        "check", 0x030C, "The caron of \\check."
        "acute", 0x0301, "The acute of \\acute."
        "grave", 0x0300, "The grave of \\grave."
        "breve", 0x0306, "The breve of \\breve."
    ]

/// Marks that grow along the line to span what they are set over or under.
let horizontalMarks =
    [
        "wideHat", 0x0302, "The circumflex of \\widehat, which grows to cover its base."
        "wideTilde", 0x0303, "The tilde of \\widetilde, which grows to cover its base."
        "overbrace", 0x23DE, "The brace of \\overbrace."
        "underbrace", 0x23DF, "The brace of \\underbrace."
        "rightArrow", 0x20D7, "The arrow of \\overrightarrow."
    ]

let radicals =
    [ "surd", 0x221A, "The tick and bar of a root, which grows to cover the radicand." ]

/// The alphabets, as a name, a first codepoint and a count.
let alphabets =
    [
        "italicSmall", 0x1D44E, 26, "Math italic, a to z, in which variables are set."
        "italicCapital", 0x1D434, 26, "Math italic, A to Z."
        "italicGreek", 0x1D6FC, 25, "Math italic, alpha to omega."
        "boldSmall", 0x1D482, 26, "Math bold italic, a to z."
        "boldCapital", 0x1D468, 26, "Math bold italic, A to Z."
        "uprightSmall", int 'a', 26, "Upright a to z, in which function names are set."
        "uprightCapital", int 'A', 26, "Upright A to Z."
        "uprightGreekCapital", 0x0391, 25, "Upright Alpha to Omega, as capital Greek is set."
        "uprightGreekSmall", 0x03B1, 25, "Upright alpha to omega, as \\mathrm sets small Greek."
        "digits", int '0', 10, "Upright 0 to 9."
    ]

/// Italic h is unassigned in its block and lives among the letterlike symbols instead.
let alphabetExceptions = [ 0x1D455, 0x210E ]

/// Codepoints inside an alphabet's range that Unicode leaves unassigned, standing between Rho and Sigma.
let alphabetHoles = [ 0x03A2 ]

/// The italic shapes Unicode keeps outside the alphabets, which follow omega in the italic block.
let italicShapes =
    [
        0x2202, 0x1D715; 0x03F5, 0x1D716; 0x03D1, 0x1D717; 0x03F0, 0x1D718
        0x03D5, 0x1D719; 0x03F1, 0x1D71A; 0x03D6, 0x1D71B
    ]

/// Every other character a formula may hold. Anything absent here cannot be drawn.
let repertoire =
    String.concat "" [
        " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        "−×÷±∓⋅∗∘•∩∪∧∨∖⊕⊗†‡"
        "≤≥≠≈≡∼≅∝≪≫⊥∥∣"
        "∈∉∋⊂⊃⊆⊇∅"
        "¬∀∃"
        "→←↔⇒⇐⇔⟺↦↑↓⟶⟵↻"
        "∞∂∇∠∆□△"
        "∴∵…⋯⋮⋱′″‰°"
        "ℕℝℤℚℂℍℙℓ"
        "⟨⟩⌊⌋⌈⌉⌀"
        // What a formula borrows from text: a price, a unit prefix, a dash between numbers.
        "£µ–"
    ]
