namespace MathTableGen

open System.Text

/// Accumulates the generated F# source.
type Writer() =
    let text = StringBuilder()

    member _.Line(line: string) = text.Append(line).Append("\r\n") |> ignore

    member _.Blank() = text.Append("\r\n") |> ignore

    /// A flat array of ints, wrapped to a readable width.
    member t.Array(name: string, values: int seq) =
        t.Line $"    let {name}: int array ="
        t.Line "        [|"
        let line = StringBuilder()
        for value in values do
            if line.Length > 100 then
                t.Line("            " + line.ToString())
                line.Clear() |> ignore
            if line.Length > 0 then line.Append("; ") |> ignore
            line.Append value |> ignore
        if line.Length > 0 then t.Line("            " + line.ToString())
        t.Line "        |]"

    member _.Text = text.ToString()
