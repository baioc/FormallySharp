/// Miscellaneous stuff that is used by both Client and Server
namespace Shared


// defines all API endpoints
module Route =
    let builder typeName methodName = $"/api/{typeName}/{methodName}"


open System

type Todo = { Id: Guid; Description: string }

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }


open Formal.Languages

type private State = string

type IAutomata =
    { Determinization: Nfa<State> -> Async<Dfa<Set<State>>>
      Union: Nfa<State> -> Nfa<State> -> Async<Nfa<State>>
      Hash: Object -> Async<State> }


module Examples =
    // functional DSL style
    let inline map s = Map.ofSeq s

    /// DFA over {0,1} that accepts even binary numbers with at least one digit.
    let even =
        { Dead = "dead"
          Current = "empty"
          Accepting = set [ "even" ]
          Transitions =
              map [
                  ("empty", '0'), "even"
                  ("empty", '1'), "odd"
                  ("even", '0'), "even"
                  ("even", '1'), "odd"
                  ("odd", '0'), "even"
                  ("odd", '1'), "odd"
              ] }

    // NFA over {a,b} that accepts strings containing "abba" as a substring.
    let abba =
        { Current = set [ "$" ]
          Accepting = set [ "ABBA" ]
          Transitions =
              map [
                  ("$", Some 'a'), set [ "$"; "A" ]
                  ("$", Some 'b'), set [ "$" ]
                  ("A", Some 'b'), set [ "AB" ]
                  ("AB", Some 'b'), set [ "ABB" ]
                  ("ABB", Some 'a'), set [ "ABBA" ]
                  ("ABBA", Some 'a'), set [ "ABBA" ]
                  ("ABBA", Some 'b'), set [ "ABBA" ]
              ] }

    /// NFA with cyclic and reflexive epsilon transitions, rejects all input.
    let cyclic =
        { Current = set [ "A" ]
          Accepting = set []
          Transitions =
              map [
                  ("A", None), set [ "A"; "B"; "C" ]
                  ("B", None), set []
                  ("C", None), set [ "B"; "A" ]
              ] }
