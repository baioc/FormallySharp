module Server

open Giraffe
open Saturn
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions

open Shared
open Formally.Regular
open Formally.Converter

/// Server-side storage. Use with parsimony.
type Storage() =
    let database =
        let mapper = FSharpBsonMapper()
        let connStr = "Filename=FormallySharp.db;mode=Exclusive"
        new LiteDatabase(connStr, mapper)

    let mutable input = Input.create("","","","","")

    let mutable regularDefinitionsMap = Map.empty

    let mutable tokensMap = Map.empty

    let simulationArray = ResizeArray<_>()

    let projects = database.GetCollection<Project> "projects"

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

    /// Retrieves a project by its identifier.
    member __.GetProject(id) =
        projects.findOne <@ fun project -> project.Id = id @>

    /// Saves a project to the database. Always overwrites.
    member __.SaveProject(project: Project) =
        projects.Insert(project)


let storage = Storage()


let api =
    // DEIXA AQUI POR ENQUANTO PLEASE:
    // { getOutputs = 
    //         fun () -> 
    //             async { 
    //                 return storage.GetOutputs() 
    //             }

    //   addOutput =
    //         fun output ->
    //             async {
    //                 match storage.AddOutput output with
    //                 | Ok () -> return output
    //                 | Error e -> return failwith e
    //             } 

    //   setInput = 
    //         fun input ->
    //             async {
    //                 storage.SetInput(input) |> ignore
    //                 let regularDefinitions = List.ofArray(System.String.Concat(input.RegularDefinition.Split(' ')).Split('\n'))
    //                 for regularDefinition in regularDefinitions do 
    //                     let text = List.ofArray(regularDefinition.Split(':'))
    //                     storage.PutRegularDefinition(text.Head, text.Item(1))
    //                 let tokens = List.ofArray(System.String.Concat(input.Token.Split(' ')).Split('\n'))
    //                 for token in tokens do
    //                     let key, value = Converter.convertTokenToRegexp(token, storage.GetRegularDefinitionsMap())
    //                     storage.PutToken(key, value)
    //                 let keyWords = List.ofArray(System.String.Concat(input.TokenKeyWord.Split(' ')).Split('\n'))
    //                 for keyWord in keyWords do
    //                     let key, value = Converter.convertRegularDefinitionTextToRegexp(keyWord)
    //                     if(storage.ContainsToken(key)) then
    //                         let mutable newValue = Regexp.empty
    //                         newValue <- (storage.GetToken(key) + value)
    //                         storage.PutToken(key, newValue)
    //                     else
    //                         storage.PutToken(key, value)
    //                 let skipWords = List.ofArray(System.String.Concat(input.TokenIgnore.Split(' ')).Split('\n'))
    //                 for skipWord in skipWords do
    //                     let __, value = Converter.convertTokenToRegexp(skipWord, storage.GetRegularDefinitionsMap())
    //                     let mutable key = "remove"
    //                     storage.PutToken(key, value)
    //                 let simulation = List.ofArray(System.String.Concat(input.Simulation.Split(' ')).Split('\n'))
    //                 for word in simulation do
    //                     storage.AddSimulationArray(word) |> ignore
    //                 return storage.GetOutputs() 
    //             }
    // }

    { generateLexer = fun spec -> async { return Lexer.make spec }
      saveProject = fun project -> async { return storage.SaveProject(project) |> ignore }
      loadProject = fun id -> async { return storage.GetProject(id) } 
      }


let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler


let routes = choose [ webApp ]

let app =
    application {
        url "http://0.0.0.0:8085"
        use_static "public"
        use_router routes
        use_gzip
        memory_cache
    }

run app
