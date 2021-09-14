namespace Formally.ContextFree.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.ContextFree


module ContextFree =
    let (:=) head body = head, body

    let grammar =
        { Initial = "S"
          Rules = set [
              // S -> aAa | bBb | C
              "S" := [ Terminal 'a'; NonTerminal "A"; Terminal 'a' ]
              "S" := [ Terminal 'b'; NonTerminal "B"; Terminal 'b' ]
              "S" := [ NonTerminal "C" ]
              // A -> Da | a
              "A" := [ NonTerminal "D"; Terminal 'a' ]
              "A" := [ Terminal 'a' ]
              // B -> Db | b
              "B" := [ NonTerminal "D"; Terminal 'b' ]
              "B" := [ Terminal 'b' ]
              // C -> CD | &
              "C" := [ NonTerminal "C"; NonTerminal "D" ]
              "C" := []
              // D -> Aa | Bb | CD
              "D" := [ NonTerminal "A"; Terminal 'a'; ]
              "D" := [ NonTerminal "B"; Terminal 'b'; ]
              "D" := [ NonTerminal "C"; NonTerminal "D" ]
          ] }

    let tests = testList "Grammars" [
        testCase "First sets" <| fun _ ->
            let testFirst symbols expected =
                Expect.equal (Grammar.first symbols grammar) expected $"FIRST({symbols})"
            testFirst [ Terminal 'a' ]    (set [ Some 'a' ])
            testFirst [ Terminal 'b' ]    (set [ Some 'b' ])
            testFirst [ NonTerminal "S" ] (set [ Some 'a'; Some 'b'; None ])
            testFirst [ NonTerminal "A" ] (set [ Some 'a'; Some 'b' ])
            testFirst [ NonTerminal "B" ] (set [ Some 'b'; Some 'a' ])
            testFirst [ NonTerminal "C" ] (set [ None; Some 'a'; Some 'b' ])
            testFirst [ NonTerminal "D" ] (set [ Some 'a'; Some 'b' ])
    ]
