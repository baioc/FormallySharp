module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Giraffe

open Shared


type Storage() =
    let outputs = ResizeArray<_>()

    let mutable input = Input.create("","","")

    member __.GetOutputs() = 
        List.ofSeq outputs

    member __.AddOutput(output: Output) =
        outputs.Add output
        Ok()

    member __.SetInput(input1: Input) = 
        input <- input1
        Ok()


let storage = Storage()

storage.AddOutput(Output.create ("a", "a", 1))
|> ignore

storage.AddOutput(Output.create ("b", "b", 2))
|> ignore

storage.AddOutput(Output.create ("c", "c", 3))
|> ignore

let api =
    { getOutputs = 
            fun () -> 
                async { 
                    return storage.GetOutputs() 
                }

      addOutput =
            fun output ->
                async {
                    match storage.AddOutput output with
                    | Ok () -> return output
                    | Error e -> return failwith e
                } 

      setInput = 
            fun input ->
                async {
                    storage.SetInput(input)
                    return storage.GetOutputs() 
                }
    }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_static "public"
        use_router webApp
        use_gzip
        memory_cache
    }

run app
