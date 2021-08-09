module Server

open Giraffe
open Saturn
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open LiteDB
open LiteDB.FSharp
open LiteDB.FSharp.Extensions

open Shared


/// Server-side storage. Use with parsimony.
type Storage() =
    let database =
        let mapper = FSharpBsonMapper()
        let connStr = "Filename=FormallySharp.db;mode=Exclusive"
        new LiteDatabase(connStr, mapper)

    let projects = database.GetCollection<Project> "projects"

    /// Retrieves a project by its identifier.
    member __.GetProject(id) =
        projects.findOne <@ fun project -> project.Id = id @>

    /// Saves a project to the database. Always overwrites.
    member __.SaveProject(project: Project) =
        projects.Insert(project)

let storage = Storage()


let api =
    { generateLexer = fun spec -> async { return Lexer.make spec }
      saveProject = fun project -> async { return storage.SaveProject(project) |> ignore }
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
