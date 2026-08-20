module ModeMath.Tests.Program

open SimpleTests

[<EntryPoint>]
let main (args: string array) : int =
    Runner.Run(args, [ CursorTests.tests; MathFontTests.tests; LayoutTests.tests ])
