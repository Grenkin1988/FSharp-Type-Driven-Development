module Samples.PollingConsumerProperties

open Swensen.Unquote
open PollingConsumer
open NUnit.Framework
open FsCheck

[<Test>]
let ``transitionFromNoMessage returns correct result when it should idle`` () =
    let test (nm : NoMessageData) (idleRes : Timed<unit>) =        
        let shouldIdle _ = true
        let idle _ = idleRes

        let actual : State =
            transitionFromNoMessage shouldIdle idle nm

        let expected =
            idleRes |> Untimed.withResult nm.Result |> ReadyState
        expected =! actual

    Check.QuickThrowOnFailure test

