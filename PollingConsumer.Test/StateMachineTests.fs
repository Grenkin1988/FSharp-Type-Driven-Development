namespace Samples.PollingConsumerProperties

open Swensen.Unquote
open Samples
open Samples.PollingConsumer
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open System

[<TestFixture>]
module StateMachineTests =
    [<Property>]
    let ``unfurl returns correct seq with constant transitions``
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

    [<Property>]
    let ``unfurl returns as many values as requested``
        (initial: TimeSpan)
        (count: byte) = 
        let count = int count
        let actual = unfurl id initial
        let actual = actual |> Seq.truncate count |> Seq.length
        count =! actual

    [<Property>]
    let ``unfurl returns correct values``
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

    type WithoutStoppedStste =
        static member State() =
            Arb.Default.Derive()
            |> Arb.filter (not << isStopped)

    [<Property(Arbitrary = [| typeof<WithoutStoppedStste> |])>]
    let ``run returns last element of seq without stops``
        (states: State list) =
        not states.IsEmpty ==> lazy
        
        let actual : State = run states
        let expected = states |> Seq.last
        expected =! actual

    [<Property>]
    let ``run returns StoppedState when it occures``
        (states : State list) =
        states |> List.exists isStopped ==> lazy
        
        let actual = run states
        let expected = states |> List.find isStopped
        expected =! actual
