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
open Formal.Languages
open Shared.Examples

let automataProtocol =
    { Determinization =
          fun nfa ->
              async {
                  let determinized = Automaton.determinize nfa.Transitions

                  let accepting =
                      Map.toSeq determinized
                      |> Seq.map (fun ((q, a), q') -> set [ q; q' ])
                      |> Set.unionMany
                      |> Set.filter (fun s -> Set.intersect s nfa.Accepting |> (not << Set.isEmpty))

                  let initial =
                      nfa.Current
                      |> Seq.map (Automaton.epsilonClosure nfa.Transitions)
                      |> Set.unionMany

                  return
                      { Transitions = determinized
                        Accepting = accepting
                        Dead = set []
                        Current = initial }
              }
      Union = fun a b -> async { return Automaton.union a b }
      Hash = fun obj -> async { return Automaton.hash obj } }

let automataApiDocs =
    let docs = Docs.createFor<IAutomata> ()

    // the quoting mechanism doesn't like complex expressions: factor them out
    let abbaObj = (abba :> obj)

    // XXX: Fable autodoc examples are not implemented for methods with more than 1 param
    Remoting.documentation
        "Automata API"
        [ docs.route <@ fun api -> api.Determinization @>
          |> docs.alias "Determinization"
          |> docs.description "Converts a nondeterministic transition table to a deterministic one"
          |> docs.example <@ fun api -> api.Determinization abba @>

          docs.route <@ fun api (a, b) -> api.Union a b @>
          |> docs.alias "Union"
          |> docs.description "Union of two NFAs through epsilon transitions"
          |> docs.example <@ fun api -> api.Union abba cyclic @>

          docs.route <@ fun api -> api.Hash @>
          |> docs.alias "Automata hash"
          |> docs.description "Base64-encoded string hash of an automaton"
          |> docs.example <@ fun api -> api.Hash abbaObj @> ]

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
