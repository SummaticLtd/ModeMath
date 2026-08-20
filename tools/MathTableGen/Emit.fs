namespace MathTableGen

open System.Text

/// Accumulates the generated F# source.
type Writer() =
    let text = StringBuilder()

    member _.Line(line: string) = text.Append(line).Append("\r\n") |> ignore

    member _.Blank() = text.Append("\r\n") |> ignore

    /// An array literal wrapped to a readable width.
    member t.Array(name: string, elementType: string, elements: string seq) =
        t.Line $"    let {name}: {elementType} array ="
        t.Line "        [|"
        let line = StringBuilder()
        for element in elements do
            if line.Length > 100 then
                t.Line("            " + line.ToString())
                line.Clear() |> ignore
            if line.Length > 0 then line.Append("; ") |> ignore
            line.Append element |> ignore
        if line.Length > 0 then t.Line("            " + line.ToString())
        t.Line "        |]"

    member t.Ints(name: string, values: int seq) =
        t.Array(name, "int", values |> Seq.map string)

    member _.Text = text.ToString()
