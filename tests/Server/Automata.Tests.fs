namespace Formal.Automata.Tests

open Expecto

open Formal.Automata


[<RequireQualifiedAccess>]
module Automaton =
    open Shared.Examples

    let tests = testList "Automata" [
        ptestCase "NFA determinization" <| fun _ ->
            Expect.equal false true "TODO: test `Automaton.determinize`"
    ]
