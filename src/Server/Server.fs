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
open Shared.Automata

let inline toTable transitions = transitions |> Map.toSeq |> Set.ofSeq
let inline toTransitions table = table |> Map.ofSeq

let toAutomaton (nfa: NondeterministicAutomaton) =
    { Current = nfa.initial
      Accepting = nfa.accepting
      Transitions = toTransitions nfa.transitions }

let toNfa automaton =
    { initial = automaton.Current
      accepting = automaton.Accepting
      transitions = toTable automaton.Transitions }

let automataProtocol =
    { determinization =
          fun table -> async { return table |> toTransitions |> Automaton.determinize |> toTable }
      indeterminization =
          fun table -> async { return table |> toTransitions |> Automaton.indeterminize |> toTable }
      union =
          fun a b ->
              async {
                  let a = toAutomaton a
                  let b = toAutomaton b
                  return Automaton.union a b |> toNfa
              }
      hash = fun obj -> async { return Automaton.hash obj }
      ``epsilon-closure`` =
          fun args ->
              async {
                  return Automaton.epsilonClosure (toTransitions args.transitions) args.initial } }

let automataApiDocs =
    let docs = Docs.createFor<automata> ()

    // the quoting mechanism doesn't like complex expressions: factor them out
    let abba = toNfa abba
    let cyclic = toNfa cyclic
    let evenTransitions = toTable even.Transitions
    let abbaObj = (abba :> obj)

    let c1 =
        { transitions = cyclic.transitions
          initial = "A" }

    let c2 =
        { transitions = abba.transitions
          initial = "$" }

    Remoting.documentation
        "Automata API"
        [ docs.route <@ fun api -> api.determinization @>
          |> docs.alias "Determinization"
          |> docs.description "Converts a nondeterministic transition table to a deterministic one"
          |> docs.example <@ fun api -> api.determinization abba.transitions @>

          docs.route <@ fun api -> api.indeterminization @>
          |> docs.alias "Indeterminization"
          |> docs.description "Trivial mapping of deterministic to nondeterministic transitions"
          |> docs.example <@ fun api -> api.indeterminization evenTransitions @>

          //   // XXX: Fable.Remoting hasn't implemented autodocs for functions with more than 1 param
          //   docs.route <@ fun api -> api.union @>
          //   |> docs.alias "Union"
          //   |> docs.description "Union of two NFAs through epsilon transitions"
          //   |> docs.example <@ fun api -> api.union abba cyclic @>

          docs.route <@ fun api -> api.hash @>
          |> docs.alias "Automata hash"
          |> docs.description "Base64-encoded string hash of an automaton"
          |> docs.example <@ fun api -> api.hash abbaObj @>

          docs.route <@ fun api -> api.``epsilon-closure`` @>
          |> docs.alias "Epsilon closure"
          |> docs.description "Finds the set reachable by epsilon transitions from a given state"
          |> docs.example <@ fun api -> api.``epsilon-closure`` c1 @>
          |> docs.example <@ fun api -> api.``epsilon-closure`` c2 @> ]

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
