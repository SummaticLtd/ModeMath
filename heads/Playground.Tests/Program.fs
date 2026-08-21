module Playground.Tests.Program

open SimpleTests

[<EntryPoint; System.STAThread>]
let main (args: string array) : int = Runner.Run(args, [ HeadTests.tests ])
