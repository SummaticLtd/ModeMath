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
        AppBuilder
            .Configure<App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))
            .SetupWithoutStarting()
        |> ignore

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
    | NonNull frame -> frame
    | Null -> failwith "the head rendered no frame"

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

/// One key pressed, named the way a headless window wants it.
let private pressing(window: Window, key: Key, physical: PhysicalKey) =
    window.KeyPress(key, RawInputModifiers.None, physical, "")

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
                "theKeysThatBuildAnAtomAreTakenBeforeTheFormulaSeesThem",
                fun () ->
                    let struct (window, view) = opened()
                    showing(view, MA.Empty)
                    view.Focus() |> ignore
                    window.KeyTextInput "a"
                    window.KeyTextInput "/"
                    window.KeyTextInput "b"
                    Dispatcher.UIThread.RunJobs()
                    let expected = MA.Row2(MA.Char 'a', MA.Frac(MA.Char 'b', MA.Empty))
                    expect(view.Formula = expected, $"typing a/b gave {view.Formula}")
            )
            Test.Sync(
                "theArrowsAndTheDeletingKeysReachTheCursorThroughAvalonia",
                fun () ->
                    let struct (window, view) = opened()
                    showing(view, MA.String "ab")
                    view.Focus() |> ignore
                    pressing(window, Key.Left, PhysicalKey.ArrowLeft)
                    window.KeyTextInput "z"
                    Dispatcher.UIThread.RunJobs()
                    expect(view.Formula = MA.String "azb", $"left then z gave {view.Formula}")
                    pressing(window, Key.Back, PhysicalKey.Backspace)
                    pressing(window, Key.Delete, PhysicalKey.Delete)
                    Dispatcher.UIThread.RunJobs()
                    expect(view.Formula = MA.Char 'a', $"backspace then delete gave {view.Formula}")
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

/// The head open on one window, with the page that holds the LaTeX box.
let private page() =
    started.Force()
    let view = MainView()
    let window = Window(Content = view, Width = 900.0, Height = 400.0)
    window.Show()
    Dispatcher.UIThread.RunJobs()
    struct (window, view)

let private entered(window: Window, view: MainView, latex: string) =
    view.Latex <- latex
    let box =
        window.GetVisualDescendants()
        |> Seq.tryPick (fun visual ->
            match visual with
            | :? TextBox as box -> Some box
            | _ -> None)
    match box with
    | None -> failwith "the page holds no box to write LaTeX in"
    | Some box ->
        box.Focus() |> ignore
        pressing(window, Key.Enter, PhysicalKey.Enter)
        Dispatcher.UIThread.RunJobs()

let private reading =
    TestList(
        "Reading",
        [   Test.Sync(
                "latexEnteredInTheBoxBecomesTheFormula",
                fun () ->
                    let struct (window, view) = page()
                    entered(window, view, @"\frac{a}{b}")
                    expect(view.Complaint = "", $"a formula that reads complained: {view.Complaint}")
                    expect(
                        view.Formula = MA.Frac(MA.Char 'a', MA.Char 'b'),
                        $"the box gave {view.Formula}")
            )
            Test.Sync(
                "latexTheReaderTurnsDownIsComplainedAboutAndNothingIsShown",
                fun () ->
                    let struct (window, view) = page()
                    entered(window, view, @"\frac{a}{b}")
                    entered(window, view, @"\foo")
                    expect(view.Complaint <> "", "a formula that does not read went uncomplained about")
                    expect(
                        view.Formula = MA.Frac(MA.Char 'a', MA.Char 'b'),
                        $"a formula that does not read replaced the one shown with {view.Formula}")
            )
        ]
    )

let tests = TestFolder("Head", [ drawing; reading ])
