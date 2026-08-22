# Heads

`Playground` is the editor as a page: the formula, keys for what typing cannot reach, and a box to
read LaTeX into it. Type to enter atoms, click to put the cursor, arrows to move it.

| Head | Run |
|---|---|
| Desktop | `dotnet run --project heads/Playground.Desktop` |
| Tests | `dotnet run --project heads/Playground.Tests` |

`Playground` holds the page whole, so a browser head over `Avalonia.Browser` would be a project
around it rather than a rewrite of it.

The tests run headless Avalonia over the real Skia, so a captured frame holds the pixels a screen
would.

## Why the versions are pinned together

Avalonia draws with SkiaSharp and ModeMath draws with SkiaSharp, and one process holds one
`SkiaSharp`. So ModeMath takes the version Avalonia ships with, and the head draws straight onto the
canvas Avalonia lends rather than copying pixels between two of them.
