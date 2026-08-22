namespace Playground

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Markup.Xaml.Styling
open Avalonia.Themes.Fluent

type App() =
    inherit Application()

    override t.Initialize() = t.Styles.Add(FluentTheme())

    override t.OnFrameworkInitializationCompleted() =
        match t.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- Window(Title = "ModeMath", Content = MainView(), Width = 900.0, Height = 600.0)
        | :? ISingleViewApplicationLifetime as single -> single.MainView <- MainView()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()
