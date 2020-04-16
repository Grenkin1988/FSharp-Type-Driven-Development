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
    let capture clock x =
        let now = clock()
        { Started = now; Stopped = now; Result = x }

    let map clock f x =
        let result = f x.Result
        let stopped = clock ()
        { Started = x.Started; Stopped = stopped; Result = result }
    
    let timeOn clock f x = x |> capture clock |> map clock f

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

let shouldPoll stopBefore calculateExpectedDuration (r : ReadyData) : bool =
    let durations = r.Result
    let expectedHandleDuration = calculateExpectedDuration durations
    r.Stopped + expectedHandleDuration < stopBefore

let poll pollForMessage clock handle () =
    let p () =
        match pollForMessage () with
        | Some msg ->
            let h () = Timed.timeOn clock (handle >> ignore) msg
            Some (h : MessageHandler)
        | None -> None
    Timed.timeOn clock p ()    

let calculateAverage (durations : TimeSpan list) =
    if durations.IsEmpty
    then None
    else
        durations
        |> List.averageBy (fun x -> float x.Ticks)
        |> int64
        |> TimeSpan.FromTicks
        |> Some

let calculateAverageAndStandardDeviation durations =
    let stdDev (avg : TimeSpan) =
        durations
        |> List.averageBy (fun x -> ((x - avg).Ticks |> float) ** 2.)
        |> sqrt
        |> int64
        |> TimeSpan.FromTicks
    durations |> calculateAverage |> Option.map (fun avg -> avg, stdDev avg)

// The expected duration is the average duration, plus three standard
// deviations. Assuming a normal distribution of durations, this should include
// 99.7 % of all durations. If the list of durations is empty, this function
// falls back on an estimated duration that the caller must supply as a wild
// guess.
let calculateExpectedDuration estimatedDuration durations =
    match calculateAverageAndStandardDeviation durations with
    | None -> estimatedDuration
    | Some (avg, stdDev) -> avg + stdDev + stdDev + stdDev // avg + stdDev * 3

// Simulation functions

let simulatedPollForMessage
    (r : Random) () =

    printfn "Polling"

    r.Next(100, 1000)
    |> Async.Sleep
    |> Async.RunSynchronously

    if r.Next(0, 100) < 50
    then Some ()
    else None

let simulatedHandle
    (r : Random) () =

    printfn "Handling"

    r.Next(100, 1000)
    |> Async.Sleep
    |> Async.RunSynchronously

// Configuration
let clock = Clocks.machineClock
let now' = DateTimeOffset.Now
let stopBefore' = now' + TimeSpan.FromSeconds 20.
let estimatedDuration' = TimeSpan.FromSeconds 2.
let idleDuration' = TimeSpan.FromSeconds 5.

// Compose functions
let shouldPoll' =
    shouldPoll stopBefore' (calculateExpectedDuration estimatedDuration') 

let r' = Random()
let handle' = simulatedHandle r'
let pollForMessage' = simulatedPollForMessage r'
let poll' = poll pollForMessage' clock handle'

let shouldIdle' = shouldIdle idleDuration' stopBefore'

let idle' = idle idleDuration'

let transition' = transition shouldPoll' poll' shouldIdle' idle'
let run' = run transition'

let result' = [] |> Timed.capture clock |> ReadyState |> run'
