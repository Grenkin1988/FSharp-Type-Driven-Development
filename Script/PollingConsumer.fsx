open System

type Timed<'a> =
    {
        Started : DateTimeOffset
        Stopped : DateTimeOffset
        Result : 'a
    }
    member this.Duration = this.Stopped - this.Started

module Untimed =
    let map f x =
        { Started = x.Started; Stopped = x.Stopped; Result = f x.Result }

    let withResult newResult x = map (fun _ -> newResult) x

module Timed =
    let captured clock x =
        let now = clock()
        { Started = now; Stopped = now; Result = x }

    let map clock f x =
        let result = f x.Result
        let stopped = clock ()
        { Started = x.Started; Stopped = stopped; Result = result }
    
    let timeOn clock f x = x |> captured clock |> map clock f

module Clocks =
    let machineClock () = DateTimeOffset.Now

    let acclock (start:DateTimeOffset) rate () =
        let now = DateTimeOffset.Now
        let ellapsed = now - start
        start.AddTicks (ellapsed.Ticks * rate)

    open System.Collections.Generic

    let qlock (q : Queue<DateTimeOffset>) = q.Dequeue    

    let seqlock (l : DateTimeOffset seq) = Queue<DateTimeOffset> l |> qlock

// Tempopary placeholder
type Todo = unit
let todo () = ()

// State date
type ReadyData = Timed<Todo>

type ReceivedMessageData = Timed<Todo>

type NoMessageData = Timed<Todo>

// State

type PollingConsumer =
    | ReadyState of ReadyData
    | ReceivedMessageState of ReceivedMessageData
    | NoMessageState of NoMessageData
    | StoppedState

// Transitions

let transitionFromStopped = StoppedState

let transitionFromNoMessage shouldIdle idle (nm : NoMessageData) =
    if shouldIdle nm
    then idle() |> ReadyState
    else StoppedState

let transitionFromReady shouldPoll poll (r : ReadyData) : PollingConsumer =
    if shouldPoll r
    then
        let msg = poll ()
        match msg.Result with
        | Some _ -> msg |> Untimed.withResult () |> ReceivedMessageState
        | None -> msg |> Untimed.withResult () |> NoMessageState
    else StoppedState


