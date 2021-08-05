module Server.Tests

open Expecto

open Server
open Shared
open Shared.Tests


let server = testList "Server" []


let all =  testList "All" [
    Shared.tests
    server
]

[<EntryPoint>]
let main _ = runTests { defaultConfig with verbosity = Logging.Debug } all
