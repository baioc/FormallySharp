module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared


[<RequireQualifiedAccess>]
module Shared =
    open Formally.Automata.Tests
    open Formally.Regular.Tests

    let tests = testList "Shared" [
        // internal libraries
        Automaton.tests
        Regexp.tests
        Nfa.tests

        // shared business logic
        testList "Domain Modeling" [
            testCase "Empty string is not a valid description" <| fun _ ->
                let expected = false
                let actual = Todo.isValid ""
                Expect.equal actual expected "Should be false"
        ]
    ]


// TODO: this is only here to avoid changing names in client tests
let shared = Shared.tests
