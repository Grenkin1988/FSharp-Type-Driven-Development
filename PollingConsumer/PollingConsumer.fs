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
    StoppedState

