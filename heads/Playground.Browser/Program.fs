module Playground.Browser.Program

open System.Threading.Tasks
open Avalonia
open Avalonia.Browser
open Playground

[<EntryPoint>]
let main (_: string array) : int =
    AppBuilder.Configure<App>().StartBrowserAppAsync "out" |> ignore
    0
