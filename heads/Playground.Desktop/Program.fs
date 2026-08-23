module Playground.Desktop.Program

open Avalonia
open Playground

[<EntryPoint; System.STAThread>]
let main (args: string array) : int =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .StartWithClassicDesktopLifetime args
