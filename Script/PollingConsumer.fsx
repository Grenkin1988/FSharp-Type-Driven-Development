open System

type Time<'a> =
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