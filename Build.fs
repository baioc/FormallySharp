open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders
open Farmer.WebApp

open Helpers

initializeContext()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    [ "Clean Fable", dotnet "fable clean --yes" "." ]
    |> runParallel
)

Target.create "InstallClient" (fun _ -> run npm "install" ".")

Target.create "Bundle" (fun _ ->
    [ "server", dotnet $"publish -c Release -o \"{deployPath}\"" serverPath
      "client", dotnet "fable --run webpack -p" clientPath ]
    |> runParallel
)

Target.create "Azure" (fun _ ->
    let plan = servicePlan {
        name "FormallySharp-farm"
        // sku Sku.Free
        sku Sku.D1
    }

    let web = webApp {
        name "FormallySharp"
        zip_deploy "deploy"
        link_to_service_plan plan
    }

    let deployment = arm {
        add_resource plan
        add_resource web
        location Location.BrazilSouth
    }

    deployment
    |> Deploy.execute "FormallySharp" Deploy.NoParameters
    |> ignore
)

Target.create "Run" (fun _ ->
    run dotnet "build" sharedPath
    [ "server", dotnet "watch run" serverPath
      "client", dotnet "fable watch --run webpack-dev-server" clientPath ]
    |> runParallel
)

Target.create "Tests" (fun _ ->
    run dotnet "build" sharedTestsPath
    [ "server", dotnet "watch run" serverTestsPath
      "client", dotnet "fable watch --run webpack-dev-server --config ../../webpack.tests.config.js" clientTestsPath ]
    |> runParallel
)

Target.create "Format" (fun _ ->
    run dotnet "fantomas . -r" "src"
)

open Fake.Core.TargetOperators

let dependencies = [
    "InstallClient"
        ==> "Bundle"
        ==> "Azure"

    "InstallClient"
        ==> "Run"

    "InstallClient"
        ==> "Tests"
]

[<EntryPoint>]
let main args = runOrDefault args
