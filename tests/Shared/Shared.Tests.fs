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
    open Formally.Converter.Tests

    let tests = testList "Shared" [
        // internal libraries
        Automaton.tests
        Regexp.tests
        Converter.tests
        Nfa.tests

        // shared business logic
        testList "Domain Modeling" []
    ]
