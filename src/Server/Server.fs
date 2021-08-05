module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Giraffe

open Shared
open Formally.Regular
open Formally.Converter

type Storage() =
    let outputs = ResizeArray<_>()

    let mutable input = Input.create("","","")

    let mutable regularDefinitionsMap = Map.empty

    let mutable debugHelper = ""

    member __.GetOutputs() = 
        List.ofSeq outputs

    member __.AddOutput(output: Output) =
        outputs.Add output
        Ok()

    member __.GetInput() = 
        input

    member __.SetInput(input1: Input) = 
        input <- input1
        Ok()

    member __.GetRegularDefinitionsMap() =
        regularDefinitionsMap

    member __.PutRegularDefinition(regularDefinition: string, regex: Regexp) =
        regularDefinitionsMap <- Map.add regularDefinition regex regularDefinitionsMap

    member __.SetDebugHelper(value: string) = 
        debugHelper <- value


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
                    storage.SetInput(input) |> ignore
                    let regularDefinitions = List.ofArray(System.String.Concat(input.RegularDefinition.Split(' ')).Split('\n'))
                    for regularDefinition in regularDefinitions do 
                        let key, value = Converter.convertRegularDefinitionTextToRegexp(regularDefinition)
                        storage.PutRegularDefinition(key, value)
                        storage.SetDebugHelper(value.ToString())
                    return storage.GetOutputs() 
                }

      getRegularDefinitionsMap = 
            fun () ->
                async {
                    return storage.GetRegularDefinitionsMap()
                }

      putRegularDefinition = 
            fun (regularDefinition, regex) ->
                async {
                    return storage.PutRegularDefinition(regularDefinition, regex)
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
