namespace Formally.ContextFree.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.ContextFree


module Grammar =
    let (:=) head body = head, body

    // a grammar with hard-to-compute FIRST & FOLLOW sets
    let notLL1 =
        { Initial = "S"
          Rules = set [
              // S -> aAa | bBb | C
              "S" := [ Terminal 'a'; NonTerminal "A"; Terminal 'a' ]
              "S" := [ Terminal 'b'; NonTerminal "B"; Terminal 'b' ]
              "S" := [ NonTerminal "C" ]
              // A -> a | DCc
              "A" := [ Terminal 'a' ]
              "A" := [ NonTerminal "D"; NonTerminal "C"; Terminal 'c' ]
              // B -> b | DCc
              "B" := [ Terminal 'b' ]
              "B" := [ NonTerminal "D"; NonTerminal "C"; Terminal 'c' ]
              // C -> CD | &
              "C" := [ NonTerminal "C"; NonTerminal "D" ]
              "C" := []
              // D -> Aa | Bb | CD
              "D" := [ NonTerminal "A"; Terminal 'a' ]
              "D" := [ NonTerminal "B"; Terminal 'b' ]
              "D" := [ NonTerminal "C"; NonTerminal "D" ]
          ] }

    let grammarWithLeftRecursions =
        { Initial = "S"
          Rules = set [
              // S -> Aa | b
              "S" := [ NonTerminal "A"; Terminal 'a' ]
              "S" := [ Terminal 'b'; ]
              // A -> Ac | Sd | a
              "A" := [ NonTerminal "A"; Terminal 'c' ]
              "A" := [ NonTerminal "S"; Terminal 'd' ]
              "A" := [ Terminal 'a'; ]
          ] }

    let grammarWithoutLeftRecursions =
        { Initial = "S"
          Rules = set [
              // S -> Aa | b
              "S" := [ NonTerminal "A"; Terminal 'a' ]
              "S" := [ Terminal 'b'; ]
              // A -> bdA' | aA'
              "A" := [ Terminal 'b'; Terminal 'd'; NonTerminal "A'"]
              "A" := [ Terminal 'a'; NonTerminal "A'"]
              // A' -> cA' | adA' | &
              "A'" := [ Terminal 'c'; NonTerminal "A'"]
              "A'" := [ Terminal 'a'; Terminal 'd'; NonTerminal "A'"]
              "A'" := []
          ] }

    let grammarToLeftFactor =
        { Initial = "S"
          Rules = set [
              // S -> AC | BC
              "S" := [ NonTerminal "A"; NonTerminal "C" ]
              "S" := [ NonTerminal "B"; NonTerminal "C" ]
              // A -> aD | cC
              "A" := [ Terminal 'a'; NonTerminal "D" ]
              "A" := [ Terminal 'c'; NonTerminal "C" ]
              // B -> aB | dD
              "B" := [ Terminal 'a'; NonTerminal "B" ]
              "B" := [ Terminal 'd'; NonTerminal "D" ]
              // C -> eC | eA
              "C" := [ Terminal 'e'; NonTerminal "C" ]
              "C" := [ Terminal 'e'; NonTerminal "A" ]
              // D -> fD | CB
              "D" := [ Terminal 'f'; NonTerminal "D" ]
              "D" := [ NonTerminal "C"; NonTerminal "B" ]
          ] }

    let grammarLeftFactored =
        { Initial = "S"
          Rules = set [
              // S -> aS' | aCC | dDC
              "S" := [ Terminal 'a'; NonTerminal "S'" ]
              "S" := [ Terminal 'c'; NonTerminal "C"; NonTerminal "C" ]
              "S" := [ Terminal 'd'; NonTerminal "D"; NonTerminal "C" ]
              // S' -> DC | BC
              "S'" := [ NonTerminal "D"; NonTerminal "C" ]
              "S'" := [ NonTerminal "B"; NonTerminal "C" ]
              // A -> aD | cC
              "A" := [ Terminal 'a'; NonTerminal "D" ]
              "A" := [ Terminal 'c'; NonTerminal "C" ]
              // B -> aB | dD
              "B" := [ Terminal 'a'; NonTerminal "B" ]
              "B" := [ Terminal 'd'; NonTerminal "D" ]
              // C -> eC'
              "C" := [ Terminal 'e'; NonTerminal "C'" ]
              // C' -> C | A
              "C'" := [ NonTerminal "C"]
              "C'" := [ NonTerminal "A"]
              // D -> fD | CB
              "D" := [ Terminal 'f'; NonTerminal "D" ]
              "D" := [ NonTerminal "C"; NonTerminal "B" ]
          ] }

    let tests = testList "Grammars" [
        testCase "FIRST sets" <| fun _ ->
            let testFirst symbols expected =
                Expect.equal (Grammar.first symbols notLL1) expected $"FIRST({symbols})"
            testFirst [ Terminal 'a' ]    (set [ Some 'a' ])
            testFirst [ Terminal 'b' ]    (set [ Some 'b' ])
            testFirst [ Terminal 'c' ]    (set [ Some 'c' ])
            testFirst [ NonTerminal "S" ] (set [ Some 'a'; Some 'b'; None ])
            testFirst [ NonTerminal "A" ] (set [ Some 'a'; Some 'b' ])
            testFirst [ NonTerminal "B" ] (set [ Some 'b'; Some 'a' ])
            testFirst [ NonTerminal "C" ] (set [ None; Some 'a'; Some 'b' ])
            testFirst [ NonTerminal "D" ] (set [ Some 'a'; Some 'b' ])

        testCase "FOLLOW sets" <| fun _ ->
            let follows = Grammar.followSets notLL1 '$'
            let testFollow symbol expected =
                Expect.equal (Map.find symbol follows) expected $"FOLLOW({symbol})"
            testFollow "S" (set [ '$' ])
            testFollow "A" (set [ 'a' ])
            testFollow "B" (set [ 'b' ])
            testFollow "C" (set [ 'c'; 'a'; 'b'; '$' ])
            testFollow "D" (set [ 'a'; 'b'; 'c'; '$' ])

        ptestCase "Left recursion elimination" <| fun _ ->
            Expect.equal
                (Grammar.eliminateLeftRecursions grammarWithLeftRecursions)
                grammarWithoutLeftRecursions
                "Failed to eliminate left recursions"

        ptestCase "Left-factoring" <| fun _ ->
            Expect.equal
                (Grammar.leftFactor grammarToLeftFactor)
                grammarLeftFactored
                "Failed to left-factor the grammar"
    ]


module Dpda =
    open Formally.Automata
    open Formally.Automata.Tests.Extensions

    // functional DSL style
    let map s = Map.ofSeq s
    let (=>) a b = a, b
    let (|->) a b = a => InputConsumingTransitions (map b)
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
                  a => ("push", ReplaceTop [ a; Bottom ])
              ]
              ("push", a) |-> [
                  a => ("push", ReplaceTop [ a; a ])
                  b => ("pop", ReplaceTop [])
              ]
              ("pop", a) |-> [
                  b => ("pop", ReplaceTop [])
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
            let tests =
                [ ""         => true
                  "b"        => false
                  "ab"       => true
                  "aabbb"    => false
                  "aaabbb"   => true
                  "aaabbabb" => false ]
            for case, expected in tests do
                let actual =
                    Automaton.exec case pairs
                    ||> fun state stack -> Set.contains state pairs.Accepting
                Expect.equal actual expected $"Wrong output for input string '{case}'"

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
