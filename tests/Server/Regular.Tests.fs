namespace Formal.Languages.Tests

open Expecto

open Formal.Languages


[<RequireQualifiedAccess>]
module Automaton =
    open Shared.Automata

    let tests = testList "Regular" [
        testCase "Automata hash" <| fun _ ->
            Expect.equal (Automaton.hash even) (Automaton.hash even) "Should be equal"
            Expect.notEqual (Automaton.hash abba) (Automaton.hash cyclic) "Should not be equal"

        ptestCase "NFA epsilon-union" <| fun _ ->
            Expect.equal false true "TODO: test `Automaton.union`"
    ]
