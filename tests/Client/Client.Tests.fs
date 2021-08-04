module Client.Tests

open Fable.Mocha

open Index
open Shared
open Shared.Tests


let client = testList "Client" []


let all =
    testList "All" [
#if FABLE_COMPILER // this preprocessor directive makes editor happy
        Shared.tests
#endif
        client
    ]

[<EntryPoint>]
let main _ = Mocha.runTests all
