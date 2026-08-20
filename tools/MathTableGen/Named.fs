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
        "bar", int '|', "The vertical bar of an absolute value, used on both sides."
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
        "digits", int '0', 10, "Upright 0 to 9."
    ]

/// Italic h is unassigned in its block and lives among the letterlike symbols instead.
let alphabetExceptions = [ 0x1D455, 0x210E ]

/// Codepoints inside an alphabet's range that Unicode leaves unassigned, standing between Rho and Sigma.
let alphabetHoles = [ 0x03A2 ]

/// The italic shapes Unicode keeps outside the alphabets, which follow omega in the italic block.
let italicShapes =
    [ 0x2202, 0x1D715; 0x03F5, 0x1D716; 0x03D1, 0x1D717; 0x03F0, 0x1D718
      0x03D5, 0x1D719; 0x03F1, 0x1D71A; 0x03D6, 0x1D71B ]

/// Every other character a formula may hold. Anything absent here cannot be drawn.
let repertoire =
    String.concat "" [
        " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
        "−×÷±∓⋅∗∩∪∧∨∖⊕⊗†‡"
        "≤≥≠≈≡∼≅∝≪≫⊥∥"
        "∈∉∋⊂⊃⊆⊇∅"
        "¬∀∃"
        "→←↔⇒⇐⇔⟺↦"
        "∞∂∇∠∆"
        "∴∵…⋯⋮⋱′″‰°"
        "ℕℝℤℚℂℍℙℓ"
        "⟨⟩"
    ]
