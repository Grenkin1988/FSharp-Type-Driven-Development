open System
open Samples

[<EntryPoint>]
let main args =
    let clock = Clocks.machineClock
    let timeAtEntry = clock()
    let stopBefore = timeAtEntry + TimeSpan.FromMinutes 1.
    let estimatedDuration = TimeSpan.FromSeconds 2.
    let idleDuration = TimeSpan.FromSeconds 5.

    let calculateExpectedDuration =
        Statistics.calculateExpectedDuration estimatedDuration
    let shouldPoll = Imp.shouldPoll calculateExpectedDuration stopBefore

    let r = Random ()
    let pollForMessage = Simulation.pollForMessage r
    let handle = Simulation.handle r
    let poll = Imp.poll pollForMessage handle clock 
    
    let shouldIdle = Imp.shouldIdle idleDuration stopBefore
    
    let idle = Imp.idle idleDuration
    
    let transition =
        PollingConsumer.transition shouldPoll poll shouldIdle idle 

    printf "Program starting"
    PollingConsumer.startOn clock
    |> PollingConsumer.unfurl transition
    |> PollingConsumer.run
    |> ignore
    0