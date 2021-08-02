namespace Formally.Automata.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.Automata


[<AutoOpen>]
module Extensions =
    module Automaton =
        // XXX: helps Fable get type info of generic parameter
#if FABLE_COMPILER
        let inline exec inputs automaton =
#else
        let exec inputs automaton =
#endif
            Automaton.trace inputs automaton
            |> Seq.last
            |> fun ((q, i), (q', o)) -> q'

    module Expect =
        /// Runs an automaton over inputs and checks if it ends in the expected state.
#if FABLE_COMPILER
        let inline trace automaton inputs expected =
#else
        let trace automaton inputs expected =
#endif
                Expect.equal (Automaton.exec inputs automaton) expected
                    "Should have reached the expected state"

module Automaton =
    let tests = testList "Automata" [
        ptestCase "Automaton.step" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.step`"

        ptestCase "Automaton.view" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.view`"

        ptestCase "Automaton.trace" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.trace`"

        ptestCase "Automaton.withViewAdapter" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.withViewAdapter`"

        ptestCase "Automaton.withInputAdapter" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.withInputAdapter`"

        ptestCase "Automaton.withOutputAdapter" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.withOutputAdapter`"

        ptestCase "Automaton.compose" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.compose`"

        ptestCase "Automaton.zip" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.zip`"

        ptestCase "Automaton.makeProbe" <| fun _ ->
            Expect.equal true false "TODO: test `Automaton.makeProbe`"
    ]
