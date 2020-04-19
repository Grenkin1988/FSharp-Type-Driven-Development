module Samples.ColorPrint

open Microsoft.FSharp.Core.Printf

let cprintf color format =
    let print (s : string) = 
        let old = System.Console.ForegroundColor 
        try 
            System.Console.ForegroundColor <- color;
            System.Console.Write s
        finally
            System.Console.ForegroundColor <- old
    kprintf print format

let cprintfn color format = 
    cprintf color format
    printfn ""
