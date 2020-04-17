#r "paket:
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
nuget Fake.Core.Target
nuget Fake.DotNet.Testing.NUnit //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.DotNet.Testing.NUnit3

let buildDir = "build/"

Target.initEnvironment ()

Target.create "Clean" (fun _ ->
    !! "**/bin"
    ++ "**/obj"
    ++ buildDir
    |> Shell.cleanDirs
)

let setBuildParams (p:DotNet.BuildOptions) : DotNet.BuildOptions =
    {
        p with
            OutputPath = Some buildDir
    }

Target.create "Build" (fun _ ->
    DotNet.build setBuildParams "FSharp-Type-Driven-Development.sln"
)

Target.create "Test" (fun _ ->
    !! (buildDir + "/*.Test.dll")
    |> Testing.NUnit3.run  (fun p ->
        { p with
            TraceLevel = NUnit3TraceLevel.Info
            TeamCity = false
            ShadowCopy = false 
            OutputDir = buildDir + "/TestResult.txt" })
)

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "All"

Target.runOrDefault "All"
