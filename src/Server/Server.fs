module Server

open Giraffe
open Saturn
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions

open Formally.ContextFree
open Shared


// XXX: we need the alternative types for projects that can be stored in LiteDB,
// see the issue with sets: https://github.com/Zaid-Ajaj/LiteDB.FSharp/issues/65
type Grammar' =
    { Initial: Identifier
      Rules: List<ContextFreeProduction<Identifier, Identifier>> }

type Project' =
    { Id: Identifier
      Lexicon: LexicalSpecification
      Syntax: Grammar' }

let private store (p: Project) : Project' =
    { Id = p.Id
      Lexicon = p.Lexicon
      Syntax = { Initial = p.Syntax.Initial; Rules = Set.toList p.Syntax.Rules } }

let private restore (p: Project') : Project =
    { Id = p.Id
      Lexicon = p.Lexicon
      Syntax = { Initial = p.Syntax.Initial; Rules = Set.ofList p.Syntax.Rules } }

/// Server-side storage. Use with parsimony.
type Storage() =
    // creates a DB if it doesn't exist, otherwise uses what's already on disk
    let database =
        let mapper = FSharpBsonMapper()
        let connStr = "Filename=FormallySharp.db;mode=Exclusive"
        new LiteDatabase(connStr, mapper)

    let projects = database.GetCollection<Project'> "projects"

    /// Retrieves a project by its identifier, raising an error when not found.
    member __.GetProject(id) =
        projects.findOne <@ fun project -> project.Id = id @>
        |> restore

    /// Saves a project to the database. NOTE: Always overwrites.
    member __.SaveProject(project: Project) =
        let project = store project
        match projects.tryFindOne <@ fun p -> p.Id = project.Id @> with
        | None -> projects.Insert(project) |> ignore
        | Some _ -> projects.Update(project) |> ignore

let storage = Storage()


let api =
    { generateLexer = fun spec -> async { return Lexer.make spec }
      generateParser = fun grammar -> async { return Parser.make grammar }
      saveProject = fun project -> async { return storage.SaveProject(project) }
      loadProject = fun id -> async { return storage.GetProject(id) } }

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
