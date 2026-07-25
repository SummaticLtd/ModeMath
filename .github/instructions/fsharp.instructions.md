---
applyTo: "**/*.fs"
---

# F# Style Guide

## Function/Method Definitions
- Tupled functions (`let f(x: int, y: int) =`) are preferred to curried functions (`let f (x: int) (y: int)`) unless intended for partial application, since these are simpler.
- Every input of every function or method should be given a type annotation, e.g. `let f(x: int) =` instead of `let f(x) =`.
- If a function or class is generic, use an explicit type parameter where possible, e.g. `let f<'T>(x: 'T) =` instead of `let f(x: 'T) =`. Generally this is possible for top-level functions and methods.

## Type Design
- Classes are preferred over records, since construction then involves the class name, making the type clearer to readers.
- If equality is needed, use `[<Struct>]` or implement IEquatable<'T>. Same for comparison.

## String Formatting
- Do not use `sprintf` or similar functions since these are AOT-unfriendly. Instead use interpolated strings `$"..."` or `String.Format`.

## Avoid Reflection
- Reflection is not AOT-friendly and breaks the guards of the type system so should be avoided where possible.

## Scoping and Encapsulation
- Give elements as much as possible small scopes, either by defining `let` bindings in smaller scopes, using private access modifiers, or using classes to hide implementation.

## Collections and Options
- We generally use modules for collections and options inside FSUtils (e.g. ImmArray.map) in addition to the built-in F# collection modules (e.g. ValueOption.map).
- By default use ValueOption instead of Option since this avoids allocating.
- The default linear collection type is ImmutableArray and the module ImmArray inside FSUtils works with these.
  Use alternative collection types if a linked list is specifically needed (list), if mutability is needed (array), if resizing is needed (ResizeArray), etc.
- The default 2D collection type is ImmArr2D and the module ImmArr2D inside FSUtils works with these.

## Null Handling
- For interop with dotnet Nullable Reference Types (NRTs), match using the syntax `match x with | Null | NonNull value`. Do not use `match x with | null -> ... | value -> ...` since this is illogical.

## Miscellaneous
- Do not use implicit yields