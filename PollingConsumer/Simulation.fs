module Samples.Simulation

open System
open Samples.ColorPrint

let pollForMessage
    (r : Random) () =

    printfn "Polling"

    r.Next(100, 1000)
    |> Async.Sleep
    |> Async.RunSynchronously

    if r.Next(0, 100) < 50
    then Some ()
    else None

let handle
    (r : Random) () =

    cprintfn ConsoleColor.Green "Handling"

    r.Next(100, 1000)
    |> Async.Sleep
    |> Async.RunSynchronously
