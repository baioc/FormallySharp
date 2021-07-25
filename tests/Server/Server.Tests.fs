module Server.Tests

open Expecto

open Server
open Shared


let server = testList "Server" [
    Formal.Automata.Tests.Automaton.tests

    testCase "Adding valid Todo" <| fun _ ->
        let storage = Storage()
        let validTodo = Todo.create "TODO"
        let expectedResult = Ok ()

        let result = storage.AddTodo validTodo

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains (storage.GetTodos()) validTodo "Storage should contain new todo"
]


let all =  testList "All" [
    Shared.Tests.Shared.tests
    server
]

[<EntryPoint>]
let main _ = runTests { defaultConfig with verbosity = Logging.Debug } all
