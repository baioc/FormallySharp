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

    let mutable input = Input.create("","","","","")

    let mutable regularDefinitionsMap = Map.empty

    let mutable tokensMap = Map.empty

    let simulationArray = ResizeArray<_>()

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

    member __.PutRegularDefinition(regularDefinition: string, regex: string) =
        regularDefinitionsMap <- Map.add regularDefinition regex regularDefinitionsMap

    member __.GetTokensMap() =
        tokensMap

    member __.PutToken(token: string, regex: Regexp) =
        tokensMap <- Map.add token regex tokensMap

    member __.ContainsToken(key: string) = 
        Map.containsKey key tokensMap

    member __.GetToken(key: string) = 
        Map.find key tokensMap

    member __.AddSimulationArray(word: string) =
        simulationArray.Add word
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
                    storage.SetInput(input) |> ignore
                    let regularDefinitions = List.ofArray(System.String.Concat(input.RegularDefinition.Split(' ')).Split('\n'))
                    for regularDefinition in regularDefinitions do 
                        let text = List.ofArray(regularDefinition.Split(':'))
                        storage.PutRegularDefinition(text.Head, text.Item(1))
                    let tokens = List.ofArray(System.String.Concat(input.Token.Split(' ')).Split('\n'))
                    for token in tokens do
                        let key, value = Converter.convertTokenToRegexp(token, storage.GetRegularDefinitionsMap())
                        storage.PutToken(key, value)
                    let keyWords = List.ofArray(System.String.Concat(input.TokenKeyWord.Split(' ')).Split('\n'))
                    for keyWord in keyWords do
                        let key, value = Converter.convertRegularDefinitionTextToRegexp(keyWord)
                        if(storage.ContainsToken(key)) then
                            let mutable newValue = Regexp.empty
                            newValue <- (storage.GetToken(key) + value)
                            storage.PutToken(key, newValue)
                        else
                            storage.PutToken(key, value)
                    let skipWords = List.ofArray(System.String.Concat(input.TokenIgnore.Split(' ')).Split('\n'))
                    for skipWord in skipWords do
                        let __, value = Converter.convertTokenToRegexp(skipWord, storage.GetRegularDefinitionsMap())
                        let mutable key = "remove"
                        storage.PutToken(key, value)
                    let simulation = List.ofArray(System.String.Concat(input.Simulation.Split(' ')).Split('\n'))
                    for word in simulation do
                        storage.AddSimulationArray(word)
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
