module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Giraffe

open Shared


type Storage() =
    let todos = ResizeArray<_>()

    member __.GetTodos() = List.ofSeq todos

    member __.AddTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

let storage = Storage()

storage.AddTodo(Todo.create "Create new SAFE project") |> ignore
storage.AddTodo(Todo.create "Write your app") |> ignore
storage.AddTodo(Todo.create "Ship it !!!") |> ignore

let todosProtocol =
    { getTodos = fun () -> async { return storage.GetTodos() }
      addTodo =
          fun todo ->
              async {
                  match storage.AddTodo todo with
                  | Ok () -> return todo
                  | Error e -> return failwith e
              } }

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosProtocol
    |> Remoting.buildHttpHandler


open Formal.Automata
open Shared.Automata

let inline tab m = m |> Map.toSeq |> Set.ofSeq

let automataProtocol =
    { determinize =
          fun transitions ->
              async { return transitions |> Map.ofSeq |> Automaton.determinize |> tab }
      ``epsilon-closure`` =
          fun args ->
              async { return Automaton.epsilonClosure (Map.ofSeq args.transitions) args.state }
      indeterminize =
          fun transitions ->
              async { return transitions |> Map.ofSeq |> Automaton.indeterminize |> tab } }

let automataApiDocs =
    let docs = Docs.createFor<automata> ()
    let abba = tab abba.Transitions
    let cyclic = tab cyclic.Transitions
    let even = tab even.Transitions

    Remoting.documentation
        "Automata API"
        [ docs.route <@ fun api -> api.determinize @>
          |> docs.alias "Determinization"
          |> docs.description "Converts a nondeterministic transition table to a deterministic one"
          |> docs.example <@ fun api -> api.determinize abba @>

          docs.route <@ fun api -> api.``epsilon-closure`` @>
          |> docs.alias "Epsilon closure"
          |> docs.description "Finds the set reachable by epsilon transitions from a given state"
          |> docs.example
              <@ fun api -> api.``epsilon-closure`` {| transitions = cyclic; state = "A" |} @>
          |> docs.example
              <@ fun api -> api.``epsilon-closure`` {| transitions = abba; state = "$" |} @>

          docs.route <@ fun api -> api.indeterminize @>
          |> docs.alias "Indeterminization"
          |> docs.description "Trivial mapping of deterministic to nondeterministic transitions"
          |> docs.example <@ fun api -> api.indeterminize even @> ]

let automataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue automataProtocol
    |> Remoting.withDocs "/api/automata/docs" automataApiDocs
    |> Remoting.buildHttpHandler


let api = choose [ automataApi; todosApi ]

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router api
        use_gzip
        memory_cache
    }

run app
