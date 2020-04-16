module Samples.PollingConsumer

open System

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