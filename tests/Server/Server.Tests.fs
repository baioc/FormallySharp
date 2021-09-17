module Server.Tests

open Expecto

open Server
open Shared
open Shared.Tests


let server = testList "Server" [
    testCase "DB write followed by read" <| fun _ ->
        let storage = Storage()
        let emptyProject =
            { Id = ""
              Lexicon = Map.empty
              Syntax = { Initial = ""; Rules = Set.empty } }
        do storage.SaveProject(emptyProject)
        let project = storage.GetProject("")
        Expect.equal project emptyProject "DB write followed by read should be idempotent"
]


let all =  testList "All" [
    Shared.tests
    server
]

[<EntryPoint>]
let main _ = runTests { defaultConfig with verbosity = Logging.Debug } all
