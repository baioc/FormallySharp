namespace Formally.ContextFree.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.ContextFree


module Dpda =
    open Formally.Automata
    open Formally.Automata.Tests.Extensions

    // functional DSL style
    let map s = Map.ofSeq s
    let (=>) a b = a, b
    let (|->) a b = a => InputConsumingTransition (map b)
    let (?->) a b = a => EpsilonTransition b
    let [<Literal>] Bottom = '$'

    // the canonical DPDA example: a recognizer of { a^n b^n | n >= 0 }
    let pairs =
        let a, b = 'a', 'b'
        { Dead = "dead"
          Accepting = set [ "zero"; "match" ]
          Current = "zero", [ Bottom ]
          Transitions = map [
              ("zero", Bottom) |-> [
                  a => ("push", Push [ a ])
              ]
              ("push", a) |-> [
                  a => ("push", Push [ a ])
                  b => ("pop", PopN 1u)
              ]
              ("pop", a) |-> [
                  b => ("pop", PopN 1u)
              ]
              ("pop", Bottom) ?-> ("match", NoOp)
          ] }

    let tests = testList "Pushdown Automaton" [
        testCase "State set inference" <| fun _ ->
            Expect.equal pairs.States (set [ "dead"; "zero"; "push"; "pop"; "match" ])
                "Should have inferred state set"

        testCase "Input alphabet inference" <| fun _ ->
            Expect.equal pairs.InputAlphabet (set [ 'a'; 'b' ])
                "Should have inferred input alphabet"

        testCase "Stack alphabet inference" <| fun _ ->
            Expect.equal pairs.StackAlphabet (set [ Bottom; 'a' ])
                "Should have inferred stack alphabet"

        testCase "DPDA execution" <| fun _ ->
            [ ""         => true
              "b"        => false
              "ab"       => true
              "aabbb"    => false
              "aaabbb"   => true
              "aaabbabb" => false ]
            |> Seq.iter
                (fun (case, expected) ->
                    let actual =
                        Automaton.exec case pairs
                        ||> fun state stack -> Set.contains state pairs.Accepting
                    Expect.equal actual expected $"Wrong output for input string '{case}'")

        testCase "Implicit dead transitions" <| fun _ ->
            let message = "Should have transitioned to dead state"
            let state, stack = pairs.Current
            // input doesn't match state + stack
            let deadByInput = { pairs with Current = pairs.Dead, stack } :> IAutomaton<_, _, _>
            Expect.equal (Automaton.step 'b' pairs) (Ok NoOp, deadByInput) message
            // stack doesn't match state + input
            let pairsWrongStack = { pairs with Current = state, [ 'a' ] } :> IAutomaton<_, _, _>
            let deadByStack = { pairs with Current = pairs.Dead, [ 'a' ] } :> IAutomaton<_, _, _>
            Expect.equal (Automaton.step 'a' pairsWrongStack) (Ok NoOp, deadByStack) message
            // additional dead transition in case of underflow
            let pairsUnderflow = { pairs with Current = state, [] } :> IAutomaton<_, _, _>
            let deadByUnderflow = { pairs with Current = pairs.Dead, [] } :> IAutomaton<_, _, _>
            Expect.equal (Automaton.step 'a' pairsUnderflow) (Error (), deadByUnderflow) message
    ]
