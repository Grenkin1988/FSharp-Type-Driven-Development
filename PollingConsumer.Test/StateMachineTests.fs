namespace Samples.PollingConsumerProperties

open Swensen.Unquote
open Samples
open Samples.PollingConsumer
open NUnit.Framework
open FsCheck
open System

[<TestFixture>]
module StateMachineTests =
    [<Test>]
    let ``run runs until stopped`` () =
        let test (states: State list) (start: State) =        
            states
            |> List.exists ((=) StoppedState) ==> lazy
            let q = System.Collections.Generic.Queue<State> states
            let transition _ = q.Dequeue()
            let actual : State = run transition start
            StoppedState =! actual

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``unfurl returns correct seq with constant transitions`` () =
        let test 
            (initial: string)
            (constantValue: string)
            (count: byte) = 
            let getNext _ = constantValue
            let actual : string seq = unfurl getNext initial
            test <@
                    actual
                    |> Seq.skip 1
                    |> Seq.truncate (int count)
                    |> Seq.forall ((=) constantValue) @>

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``unfurl returns as many values as requested`` () =
        let test 
            (initial: TimeSpan)
            (count: byte) = 
            let count = int count
            let actual = unfurl id initial
            let actual = actual |> Seq.truncate count |> Seq.length
            count =! actual

        Check.QuickThrowOnFailure test

    [<Test>]
    let ``unfurl returns correct values`` () =
        let test 
            (initial: byte)
            (count: byte) = 
            let count = int count
            let initial = int initial
            let actual = 
                unfurl ((+) 1) initial
                |> Seq.truncate (count + 1) 
                |> Seq.toList
            let expected = 
                [initial .. initial + count]
            expected =! actual

        Check.QuickThrowOnFailure test




