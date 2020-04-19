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

type StoppedData = TimeSpan list

// State
type State =
    | ReadyState of ReadyData
    | ReceivedMessageState of ReceivedMessageData
    | NoMessageState of NoMessageData
    | StoppedState of StoppedData

// Transitions
let transitionFromNoMessage shouldIdle idle nm =
    if shouldIdle nm
    then idle () |> Untimed.withResult nm.Result |> ReadyState
    else StoppedState nm.Result

let transitionFromReady shouldPoll poll (r : ReadyData) =
    if shouldPoll r
    then
        let msg = poll()
        match msg.Result with
        | None -> msg |> Untimed.withResult r.Result |> NoMessageState
        | Some mh ->
            msg 
            |> Untimed.withResult (r.Result, mh) 
            |> ReceivedMessageState
    else StoppedState r.Result

let transitionFromReceived (rm : ReceivedMessageData) = 
    let durations, handleMessage = rm.Result
    let t = handleMessage.Handle ()
    let pollDuration = rm.Duration
    let handleDuration = t.Duration
    let totalDuration = pollDuration + handleDuration
    t |> Untimed.withResult (totalDuration :: durations) |> ReadyState

let transitionFromStopped s = StoppedState s

// State machine

let rec unfurl getNext state =
    seq {
        yield state
        let next = getNext state
        yield! unfurl getNext next
    }

let isStopped = function StoppedState _ -> true | _ -> false

let run states =
    let takeUntil predicate (s : seq<'a>) =
        let rec loop (en : Collections.Generic.IEnumerator<_>) = seq {
            if en.MoveNext() then
                yield en.Current
                if not (predicate en.Current) then
                    yield! loop en }
        seq {
            use en = s.GetEnumerator()
            yield! loop en
        }                
    states |> takeUntil isStopped |> Seq.last

let transition shouldPoll poll shouldIdle idle state =
    match state with
    | ReadyState r -> transitionFromReady shouldPoll poll r
    | ReceivedMessageState rm -> transitionFromReceived rm
    | NoMessageState nm -> transitionFromNoMessage shouldIdle idle nm
    | StoppedState s -> transitionFromStopped s

let startOn clock = [] |> Timed.capture clock |> ReadyState

let durations = function
    | ReadyState r -> r.Result
    | ReceivedMessageState rm -> fst rm.Result
    | NoMessageState nm -> nm.Result
    | StoppedState s -> s
