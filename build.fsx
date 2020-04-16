#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "**/bin"
    ++ "**/obj"
    |> Shell.cleanDirs 
)

Target.create "Build" (fun _ ->
    !! "FSharp-Type-Driven-Development.sln"
    |> Seq.iter (DotNet.build id)
)

Target.create "Test" (fun _ ->
    !! "**/*Tests.dll"
    |> Seq.iter (DotNet.test (fun p -> DotNet.TestOptions.Create()))
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
