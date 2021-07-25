namespace Formal.Automata.Tests

open Expecto

open Formal.Automata


[<RequireQualifiedAccess>]
module Automaton =
    open Formal.Languages
    open System

    /// DFA over {0,1} that accepts even binary numbers with at least one digit.
    let even =
        { Dead = "dead"
          Current = "empty"
          Accepting = set [ "even" ]
          Transitions =
              Map.ofList [ ("empty", '0'), "even"
                           ("empty", '1'), "odd"
                           ("even", '0'), "even"
                           ("even", '1'), "odd"
                           ("odd", '0'), "even"
                           ("odd", '1'), "odd" ] }

    // NFA over {a,b} that accepts strings containing "abba" as a substring.
    let abba =
        { Current = set [ "$" ]
          Accepting = set [ "ABBA" ]
          Transitions =
              Map.ofList [ ("$", Some 'a'), set [ "$"; "A" ]
                           ("$", Some 'b'), set [ "$" ]
                           ("A", Some 'b'), set [ "AB" ]
                           ("AB", Some 'b'), set [ "ABB" ]
                           ("ABB", Some 'a'), set [ "ABBA" ]
                           ("ABBA", Some 'a'), set [ "ABBA" ]
                           ("ABBA", Some 'b'), set [ "ABBA" ] ] }

    /// An NFA with cyclic and reflexive epsilon transitions, rejects all input.
    let cyclic =
        { Current = set [ "A" ]
          Accepting = set []
          Transitions =
              Map.ofList [ ("A", Automaton.epsilon), set [ "A"; "B"; "C" ]
                           ("B", Automaton.epsilon), set []
                           ("C", Automaton.epsilon), set [ "B"; "A" ] ] }

    let exec automaton inputs =
        Automaton.trace automaton inputs
        |> Seq.last
        |> fun ((q, i), (q', o)) -> q'

    module Expect =
        /// Runs an automaton over inputs and checks if it ends in the expected state.
        let trace automaton inputs expected =
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

        testCase "DFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let number = rand.Next()
                let string = Convert.ToString(number, 2)
                let expected = if number % 2 = 0 then "even" else "odd"
                Expect.trace even string expected

        testCase "NFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let bits = Convert.ToString(rand.Next(), 2)
                let string = String.map (fun c -> if c = '0' then 'a' else 'b') bits
                let expected = string.Contains "abba"
                let actual = exec abba (Seq.map Some string) |> Set.isSubset abba.Accepting
                Expect.equal actual expected $"Should have answered {expected} on \"{string}\""

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
            Expect.equal even.Alphabet (set "01") message
            Expect.equal abba.Alphabet (set "ab") message
            Expect.equal cyclic.Alphabet (set "") message
    ]
