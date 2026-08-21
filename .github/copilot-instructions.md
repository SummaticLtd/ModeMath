# Copilot Instructions

## Project Overview

ModeMath displays and edits mathematical formulas, rendered with SkiaSharp.

`MA` is the formula tree. `MACurs` mirrors it with a cursor at one position.

## Technology Stack
- Primary Language: F#
- Target Framework: .NET 10
- Tests use SimpleTests.

## Path-Specific Instructions

Additional scoped instructions are defined under `.github/instructions/`:
- `fsharp.instructions.md` for `**/*.fs`

## Comments and Documentation

- Do not add a comment that restates the code. Assume the reader can read F#.
- Prose in README.md and comments explaining a decision require explicit human approval before being added.

## Formatting

- Do not add whitespace to vertically align code or text, unless required by indentation rules.

## Code Review Guidelines

Do NOT comment on potential compilation failures.
