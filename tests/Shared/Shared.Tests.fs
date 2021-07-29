module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared


[<RequireQualifiedAccess>]
module Shared =
    open Formally.Automata
    open Formally.Regular

    let tests = testList "Shared" [
        Tests.Automaton.tests
        Tests.Regexp.tests
        Tests.Nfa.tests

        testList "Misc" [
            testCase "Empty string is not a valid description" <| fun _ ->
                let expected = false
                let actual = Todo.isValid ""
                Expect.equal actual expected "Should be false"
        ]
    ]


// TODO: this is only here to avoid changing names in client tests
let shared = Shared.tests
