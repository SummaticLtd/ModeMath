namespace ModeMath.Tests

open System

type Assert =
    [<Diagnostics.DebuggerHidden>]
    static member Equal<'a when 'a: equality and 'a: not null>(expected: 'a, actual: 'a, ?errorMsg: string) =
        if expected <> actual then
            let suffix = match errorMsg with None -> "" | Some msg -> Environment.NewLine + msg
            failwith(
                "Expected: " + expected.ToString() + Environment.NewLine
                + "But was: " + actual.ToString() + suffix
            )
    [<Diagnostics.DebuggerHidden>]
    static member True(condition: bool, ?errorMsg: string) =
        match errorMsg with
        | None -> if not condition then failwith "Expected condition to be true but was false."
        | Some em -> if not condition then failwith("Expected condition to be true but was false." + Environment.NewLine + em)
    [<Diagnostics.DebuggerHidden>]
    static member Fail(errorMsg: string) = failwith errorMsg
