module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

type Storage() =
    let simulatorOutputList = ResizeArray<_>()

    member __.GetSimulatorOutputList() = List.ofSeq simulatorOutputList

    member __.AddSimulatorOutput(simulatorOutput: SimulatorOutput) =
        simulatorOutputList.Add simulatorOutput
        Ok()


let storage = Storage()

storage.AddSimulatorOutput(SimulatorOutput.create ("a", "a", 1))
|> ignore

storage.AddSimulatorOutput(SimulatorOutput.create ("b", "b", 2))
|> ignore

storage.AddSimulatorOutput(SimulatorOutput.create ("c", "c", 3))
|> ignore

let simulatorOutputApi =
    { getSimulatorOutputList = fun () -> async { return storage.GetSimulatorOutputList() }
      addSimulatorOutput =
          fun simulatorOutput ->
              async {
                  match storage.AddSimulatorOutput simulatorOutput with
                  | Ok () -> return simulatorOutput
                  | Error e -> return failwith e
              } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue simulatorOutputApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
