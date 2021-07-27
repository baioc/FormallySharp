namespace Formal.Automata.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formal.Automata


[<RequireQualifiedAccess>]
module Automaton =
    open Shared.Automata // provides example machines
    open System

    let inline exec automaton inputs =
        Automaton.trace automaton inputs
        |> Seq.last
        |> fun ((q, i), (q', o)) -> q'

    module Expect =
        /// Runs an automaton over inputs and checks if it ends in the expected state.
        let inline trace automaton inputs expected =
                Expect.equal (exec automaton inputs) expected
                    "Should have reached the expected state"

    let tests = testList "Automata" [
        testCase "Polymorphic view" <| fun _ ->
            let message = "Should be able to view initial state"
            Expect.equal (Automaton.view even) even.Current message
            Expect.equal (Automaton.view abba) abba.Current message

        testCase "Polymorphic step" <| fun _ ->
            Expect.trace even "10" "even"
            Expect.trace abba (Seq.map Some "ab") (set [ "$"; "AB" ])
            Expect.trace cyclic [ Some '.' ] (set [])

        testCase "Epsilon closures" <| fun _ ->
            let abba = Automaton.epsilonClosure abba.Transitions
            let cyclic = Automaton.epsilonClosure cyclic.Transitions
            Expect.equal (abba "$") (set [ "$" ]) "Closure from a state should contain itself"
            Expect.equal (cyclic "A") (set [ "A"; "B"; "C" ]) "Should work with cyclic closures"

        testCase "Indeterminization" <| fun _ ->
            let nondetEven =
                map [ ("empty", Some '0'), set [ "even" ]
                      ("empty", Some '1'), set [ "odd" ]
                      ("even", Some '0'), set [ "even" ]
                      ("even", Some '1'), set [ "odd" ]
                      ("odd", Some '0'), set [ "even" ]
                      ("odd", Some '1'), set [ "odd" ] ]
            Expect.equal (Automaton.indeterminize even.Transitions) nondetEven
                "Should have been a trivial conversion"

        testCase "DFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let number = rand.Next()
                let binary = Convert.ToString(number, 2)
                let expected = if number % 2 = 0 then "even" else "odd"
                Expect.trace even binary expected

        testCase "NFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let bits = Convert.ToString(rand.Next(), 2)
                let input = String.map (fun c -> if c = '0' then 'a' else 'b') bits
                let expected = input.Contains "abba"
                let actual =
                    exec abba (Seq.map Some input)
                    |> Set.intersect abba.Accepting
                    |> Set.isEmpty
                    |> not
                Expect.equal actual expected $"Should have halted with {expected} on \"{input}\""

        testCase "Implicit dead transitions" <| fun _ ->
            let message = "Should have transitioned to dead state"
            let deadEven = { even with Current = even.Dead } :> IAutomaton<_, _, _>
            let deadAbba = { abba with Current = abba.Dead } :> IAutomaton<_, _, _>
            Expect.equal (Automaton.step even '?') (deadEven, ()) message
            Expect.equal (Automaton.step abba (Some '?')) (deadAbba, ()) message

        testCase "State set inference" <| fun _ ->
            let message = "Should have inferred state set"
            Expect.equal even.States (set [ "dead"; "empty"; "even"; "odd" ]) message
            Expect.equal abba.States (set [ "$"; "A"; "AB"; "ABB"; "ABBA" ]) message
            Expect.equal cyclic.States (set [ "A"; "B"; "C" ]) message

        testCase "Alphabet inference" <| fun _ ->
            let message = "Should have inferred input alphabet"
            Expect.equal even.Alphabet (set [ '0'; '1' ]) message
            Expect.equal abba.Alphabet (set [ 'a'; 'b' ]) message
            Expect.equal cyclic.Alphabet (set []) message
    ]
