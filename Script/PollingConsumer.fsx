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

// Auxiliary types
type MessageHandler = unit -> Timed<unit>

// State date
type ReadyData = Timed<TimeSpan list>

type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>

type NoMessageData = Timed<TimeSpan list>

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
    then idle() |> Untimed.withResult nm.Result |> ReadyState
    else StoppedState

let transitionFromReady shouldPoll poll (r : ReadyData) =
    if shouldPoll r
    then
        let msg = poll ()
        match msg.Result with
        | Some h -> msg |> Untimed.withResult (r.Result, h) |> ReceivedMessageState
        | None -> msg |> Untimed.withResult r.Result |> NoMessageState
    else StoppedState

let transitionFromReceived (rm : ReceivedMessageData) = 
    let durations, handleMessage = rm.Result
    let t = handleMessage ()
    let pollDuration = rm.Duration
    let handleDuration = t.Duration
    let totalDuration = pollDuration + handleDuration
    t |> Untimed.withResult (totalDuration :: durations) |> ReadyState

// State machine
let rec run trans state =
    let nextState = trans state
    match nextState with
    | StoppedState -> StoppedState
    | _ -> run trans nextState

let transition shouldPoll poll shouldIdle idle  state =
    match state with
    | ReadyState r -> transitionFromReady shouldPoll poll r
    | ReceivedMessageState rm -> transitionFromReceived rm
    | NoMessageState nm -> transitionFromNoMessage shouldIdle idle nm
    | StoppedState -> transitionFromStopped

let shouldIdle idleDuration stopBefore (nm : NoMessageData) : bool =
    nm.Stopped + idleDuration < stopBefore

let idle (idleDuration : TimeSpan) () =
    let s () =
        idleDuration.TotalMilliseconds
        |> int 
        |> Async.Sleep
        |> Async.RunSynchronously 
    printfn "Sleeping"
    Timed.timeOn Clocks.machineClock s ()

let shouldPoll stoppedBefore calculateExpectedDuration (r : ReadyData) : bool =
    let durations = r.Result
    let expectedHandleDuration = calculateExpectedDuration durations
    r.Stopped + expectedHandleDuration < stoppedBefore

let poll pollForMessage clock handle () =
    let p () =
        match pollForMessage () with
        | Some msg ->
            let h () = Timed.timeOn clock (handle >> ignore) msg
            Some (h : MessageHandler)
        | None -> None
    Timed.timeOn clock p ()    



