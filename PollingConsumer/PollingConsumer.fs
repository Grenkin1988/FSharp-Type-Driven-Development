module Samples.PollingConsumer

open System

// Auxiliary types
[<CustomEquality; NoComparison>]
type MessageHandler = 
    { 
        Handle : unit -> Timed<unit> 
    }
    override this.Equals obj =
        match obj with
        | :? MessageHandler as other ->
            Object.Equals(this.Handle, other.Handle)
        | _ -> false
    override this.GetHashCode() = (box this.Handle).GetHashCode()        

// State date
type ReadyData = Timed<TimeSpan list>

type ReceivedMessageData = Timed<TimeSpan list * MessageHandler>

type NoMessageData = Timed<TimeSpan list>

// State
type State =
    | ReadyState of ReadyData
    | ReceivedMessageState of ReceivedMessageData
    | NoMessageState of NoMessageData
    | StoppedState

// Transitions
let transitionFromStopped = StoppedState

let transitionFromNoMessage shouldIdle idle nm =
    if shouldIdle ()
    then idle () |> Untimed.withResult nm.Result |> ReadyState
    else StoppedState

let transitionFromReady shouldPoll poll (r : ReadyData) =
    if shouldPoll ()
    then
        let msg = poll()
        match msg.Result with
        | None -> msg |> Untimed.withResult r.Result |> NoMessageState
        | Some mh ->
            msg 
            |> Untimed.withResult (r.Result, mh) 
            |> ReceivedMessageState
    else StoppedState

let transitionFromReceived (rm : ReceivedMessageData) = 
    let durations, handleMessage = rm.Result
    let t = handleMessage.Handle ()
    let pollDuration = rm.Duration
    let handleDuration = t.Duration
    let totalDuration = pollDuration + handleDuration
    t |> Untimed.withResult (totalDuration :: durations) |> ReadyState

// State machine

let rec unfurl getNext state =
    seq {
        yield state
        let next = getNext state
        yield! unfurl getNext next
    }

let isStopped = function StoppedState -> true | _ -> false

let run states =
    states |> Seq.last


