# Heads

`Playground` is the editor as a page: the formula, keys for what typing cannot reach, and a box to
read LaTeX into it. Type to enter atoms, click to put the cursor, arrows to move it.

Run it with `dotnet run --project heads/Playground.Desktop`.

It is a demo rather than part of the library, and nothing here is tested. `Playground` holds the
page whole, so a browser head over `Avalonia.Browser` would be a project around it rather than a
rewrite of it.

## Why the versions are pinned together

Avalonia draws with SkiaSharp and ModeMath draws with SkiaSharp, and one process holds one
`SkiaSharp`. So ModeMath takes the version Avalonia ships with, and the head draws straight onto the
canvas Avalonia lends rather than copying pixels between two of them.
