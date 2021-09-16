namespace Formally.ContextFree.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.ContextFree


module ContextFree =
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
              "D" := [ NonTerminal "A"; Terminal 'a'; ]
              "D" := [ NonTerminal "B"; Terminal 'b'; ]
              "D" := [ NonTerminal "C"; NonTerminal "D" ]
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
            let testFollow symbol expected =
                Expect.equal (Grammar.follow symbol notLL1 '$' "S") expected $"FOLLOW({symbol})"
            testFollow "S" (set [ '$' ])
            testFollow "A" (set [ 'a' ])
            testFollow "B" (set [ 'b' ])
            testFollow "C" (set [ 'c'; 'a'; 'b'; '$' ])
            testFollow "D" (set [ 'a'; 'b'; 'c'; '$' ])
    ]
