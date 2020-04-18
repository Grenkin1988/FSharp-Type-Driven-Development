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





