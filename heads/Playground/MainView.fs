namespace Playground

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open ModeMath

/// The formula, the keys that build what typing cannot, and a box to read LaTeX into it.
type MainView() as t =
    inherit UserControl()

    let formula = FormulaView()
    let latex = TextBox(PlaceholderText = @"\frac{1}{2} — press Enter to read", FontFamily = FontFamily "monospace")
    let complaint = TextBlock(Foreground = Brushes.Crimson, Margin = Thickness(4.0, 0.0, 0.0, 0.0))

    let read() =
        match Latex.Read(if isNull latex.Text then "" else latex.Text) with
        | Ok ma ->
            complaint.Text <- ""
            formula.Editor <- Editor.AtEnd(Layout formula.FontSize, ma)
        | Error error -> complaint.Text <- string error

    let button(caption: string, act: unit -> unit) =
        let button = Button(Content = caption, Margin = Thickness(0.0, 0.0, 4.0, 0.0))
        button.Click.Add(fun _ ->
            act()
            formula.Focus() |> ignore)
        button

    let inserted(build: Editor -> Editor) =
        fun () -> formula.Editor <- build formula.Editor

    let keys =
        let panel = StackPanel(Orientation = Orientation.Horizontal, Margin = Thickness 8.0)
        let add(caption, act) = panel.Children.Add(button(caption, act))
        add("a/b", inserted(fun e -> e.InsertFraction))
        add("√", inserted(fun e -> e.InsertSqrt))
        add("ⁿ√", inserted(fun e -> e.InsertRoot))
        add("xʸ", inserted(fun e -> e.InsertSuperscript))
        add("xᵧ", inserted(fun e -> e.InsertSubscript))
        add("( )", inserted(fun e -> e.InsertBracket(Brackets.Matching Bracket.Normal)))
        add("| |", inserted(fun e -> e.InsertBracket(Brackets.Matching Bracket.Line)))
        add("clear", inserted(fun e -> Editor(Layout formula.FontSize, MA.Empty)))
        panel

    let entry =
        let panel = DockPanel(Margin = Thickness(8.0, 0.0, 8.0, 8.0))
        let load = button("read", read)
        DockPanel.SetDock(load, Dock.Right)
        panel.Children.Add load
        panel.Children.Add latex
        panel

    do
        latex.KeyDown.Add(fun e ->
            if e.Key = Avalonia.Input.Key.Enter then
                read()
                e.Handled <- true)
        let layout = DockPanel()
        DockPanel.SetDock(keys, Dock.Top)
        DockPanel.SetDock(entry, Dock.Top)
        DockPanel.SetDock(complaint, Dock.Bottom)
        layout.Children.Add keys
        layout.Children.Add entry
        layout.Children.Add complaint
        layout.Children.Add formula
        t.Content <- layout
        t.AttachedToVisualTree.Add(fun _ -> formula.Focus() |> ignore)
