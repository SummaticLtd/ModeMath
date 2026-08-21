module Playground.Tests.HeadTests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media.Imaging
open Avalonia.Threading
open Avalonia.VisualTree
open SimpleTests
open ModeMath
open Playground

/// Headless Avalonia over the real Skia, which is what makes a captured frame worth counting.
let private started =
    lazy
        (AppBuilder
            .Configure<App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))
            .SetupWithoutStarting()
         |> ignore)

let private size = 48f<px>

/// The head open on one window, and the view inside it that a formula goes into.
let private opened() =
    started.Force()
    let window = Window(Content = MainView(), Width = 900.0, Height = 400.0)
    window.Show()
    Dispatcher.UIThread.RunJobs()
    let view =
        window.GetVisualDescendants()
        |> Seq.tryPick (fun visual ->
            match visual with
            | :? FormulaView as view -> Some view
            | _ -> None)
    match view with
    | None -> failwith "the head holds no FormulaView"
    | Some view -> struct (window, view)

let private showing(view: FormulaView, formula: MA) =
    view.Editor <- Editor.AtEnd(Layout size, formula)
    Dispatcher.UIThread.RunJobs()

let private captured(window: Window) =
    match window.CaptureRenderedFrame() with
    | null -> failwith "the head rendered no frame"
    | frame -> frame

/// The dark pixels below the entry row, which is where the formula and nothing else is drawn.
let private ink(frame: WriteableBitmap) =
    let width = frame.PixelSize.Width
    let height = frame.PixelSize.Height
    let pixels = Array.zeroCreate<byte> (width * height * 4)
    use buffer = frame.Lock()
    System.Runtime.InteropServices.Marshal.Copy(buffer.Address, pixels, 0, pixels.Length)
    let mutable dark = 0
    for y in height / 4 .. height - 1 do
        for x in 0 .. width - 1 do
            let at = (y * width + x) * 4
            if int pixels.[at] + int pixels.[at + 1] + int pixels.[at + 2] < 200 then dark <- dark + 1
    dark

let private expect(condition: bool, complaint: string) =
    if not condition then failwith complaint

let private drawing =
    TestList(
        "Drawing",
        [   Test.Sync(
                "aFormulaShownInTheHeadIsDrawnOnTheCanvasAvaloniaLends",
                fun () ->
                    let struct (window, view) = opened()
                    showing(view, MA.Frac(MA.String "-b", MA.String "2a"))
                    use frame = captured window
                    let drawn = ink frame
                    expect(drawn > 300, $"only {drawn} dark pixels, so the formula was not drawn")
            )
            Test.Sync(
                "aClickPutsTheCursorWhereTheFormulaWasDrawn",
                fun () ->
                    let struct (window, view) = opened()
                    showing(view, MA.String "ab")
                    view.Focus() |> ignore
                    // Left of the leftmost atom, which is where the cursor goes before all of them.
                    window.MouseDown(Point(2.0, 200.0), MouseButton.Left)
                    window.MouseUp(Point(2.0, 200.0), MouseButton.Left)
                    window.KeyTextInput "z"
                    Dispatcher.UIThread.RunJobs()
                    expect(
                        view.Formula = MA.String "zab",
                        $"a click at the left end then z gave {view.Formula}")
            )
        ]
    )

let tests = TestFolder("Head", [ drawing ])
