module Client.Tests

open Fable.Mocha


let client = testList "Client" []


let all =
    testList "All" [
#if FABLE_COMPILER // this preprocessor directive makes editor happy
        Shared.tests
#endif
        client
    ]

Mocha.runTests all |> ignore
