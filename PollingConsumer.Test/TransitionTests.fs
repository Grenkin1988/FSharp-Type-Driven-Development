namespace Samples.PollingConsumerProperties

open Swensen.Unquote
open Samples
open Samples.PollingConsumer
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TransitionTests =
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

    [<Test>]
    let ``transitionFromNoMessage returns correct result when it should not idle`` () =
        let test (nm : NoMessageData) (idleRes : Timed<unit>) =        
            let shouldIdle _ = false
            let idle _ = idleRes

            let actual : State =
                transitionFromNoMessage shouldIdle idle nm

            let expected = StoppedState nm.Result
            expected =! actual

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``transitionFromReady returns correct result when it should not poll`` () =
        let test (rd : ReadyData) (mh : Timed<MessageHandler option>) =        
            let shouldPoll _ = false
            let poll _ = mh

            let actual : State =
                transitionFromReady shouldPoll poll rd

            let expected = StoppedState rd.Result
            expected =! actual

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``transitionFromReady returns correct result when polling no message`` () =
        let test (rd : ReadyData) (mh : Timed<unit>) =        
            let shouldPoll _ = true
            let poll _ = 
                mh |> Untimed.withResult None

            let actual : State =
                transitionFromReady shouldPoll poll rd

            let expected =
                mh |> Untimed.withResult rd.Result |> NoMessageState
            expected =! actual

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``transitionFromReady returns correct result when polling message`` () =
        let test (rd : ReadyData) (mh : Timed<MessageHandler>) =        
            let shouldPoll _ = true
            let poll _ = 
                mh |> Untimed.withResult (Some mh.Result)

            let actual : State =
                transitionFromReady shouldPoll poll rd

            let expected =
                mh |> Untimed.withResult (rd.Result, mh.Result)
                |> ReceivedMessageState
            expected =! actual

        Check.QuickThrowOnFailure test    

