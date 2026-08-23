namespace MathTableGen

open System.Text

/// Accumulates the generated F# source.
type Writer() =
    let text = StringBuilder()

    let wrapped(indent: string, elements: string seq) =
        let lines = ResizeArray<string>()
        let line = StringBuilder()
        for element in elements do
            if line.Length > 100 then
                lines.Add(indent + line.ToString())
                line.Clear() |> ignore
            if line.Length > 0 then line.Append("; ") |> ignore
            line.Append element |> ignore
        if line.Length > 0 then lines.Add(indent + line.ToString())
        lines

    member _.Line(line: string) = text.Append(line).Append("\r\n") |> ignore

    member _.Blank() = text.Append("\r\n") |> ignore

    /// An array literal bound to a name inside a module, wrapped to a readable width.
    member t.Array(name: string, elementType: string, elements: string seq) =
        t.Line $"    let {name}: {elementType} array ="
        t.Line "        [|"
        for line in wrapped("            ", elements) do
            t.Line line
        t.Line "        |]"

    /// An array literal nested inside a constructor call, followed by a comma or a closing bracket.
    member t.Nested(elements: string seq, suffix: string) =
        let lines = wrapped("                ", elements)
        if lines.Count = 0 then
            t.Line $"            [||]{suffix}"
        else
            t.Line "            [|"
            for line in lines do
                t.Line line
            t.Line $"            |]{suffix}"

    member _.Text = text.ToString()
