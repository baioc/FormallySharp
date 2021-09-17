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
              "D" := [ NonTerminal "A"; Terminal 'a' ]
              "D" := [ NonTerminal "B"; Terminal 'b' ]
              "D" := [ NonTerminal "C"; NonTerminal "D" ]
          ] }

    // let GrammarToRefactor =
    //     { Initial = "S"
    //       Rules = set [
    //           // S -> AC | BC
    //           "S" := [ NonTerminal "A"; NonTerminal "C" ]
    //           "S" := [ NonTerminal "B"; NonTerminal "C" ]
    //           // A -> aD | cC
    //           "A" := [ Terminal 'a'; NonTerminal "D" ]
    //           "A" := [ Terminal 'c'; NonTerminal "C" ]
    //           // B -> aB | dD
    //           "B" := [ Terminal 'a'; NonTerminal "B" ]
    //           "B" := [ Terminal 'd'; NonTerminal "D" ]
    //           // C -> eC | eA
    //           "C" := [ Terminal 'e'; NonTerminal "C" ]
    //           "C" := [ Terminal 'e'; NonTerminal "A" ]
    //           // D -> fD | CB
    //           "D" := [ Terminal 'f'; NonTerminal "D" ]
    //           "D" := [ NonTerminal "C"; NonTerminal "B" ]
    //       ] }

    // let GrammarRefactored =
    //     { Initial = "S"
    //       Rules = set [
    //           // S -> aS' | aCC | dDC
    //           "S" := [ Terminal 'a'; NonTerminal "S'" ]
    //           "S" := [ Terminal 'c'; NonTerminal "C"; NonTerminal "C" ]
    //           "S" := [ Terminal 'd'; NonTerminal "D"; NonTerminal "C" ]
    //           // S' -> DC | BC
    //           "S'" := [ NonTerminal "D"; NonTerminal "C" ]
    //           "S'" := [ NonTerminal "B"; NonTerminal "C" ]
    //           // A -> aD | cC
    //           "A" := [ Terminal 'a'; NonTerminal "D" ]
    //           "A" := [ Terminal 'c'; NonTerminal "C" ]
    //           // B -> aB | dD
    //           "B" := [ Terminal 'a'; NonTerminal "B" ]
    //           "B" := [ Terminal 'd'; NonTerminal "D" ]
    //           // C -> eC'
    //           "C" := [ Terminal 'e'; NonTerminal "C'" ]
    //           // C' -> C | A
    //           "C'" := [ NonTerminal "C"]
    //           "C'" := [ NonTerminal "A"]
    //           // D -> fD | CB
    //           "D" := [ Terminal 'f'; NonTerminal "D" ]
    //           "D" := [ NonTerminal "C"; NonTerminal "B" ]
    //       ] }

    // let grammarWithLeftRecursions =
    //     { Initial = "S"
    //       Rules = set [
    //           // S -> Aa | b
    //           "S" := [ NonTerminal "A"; Terminal 'a' ]
    //           "S" := [ Terminal 'b'; ]
    //           // A -> Ac | Sd | a
    //           "A" := [ NonTerminal "A"; Terminal 'c' ]
    //           "A" := [ NonTerminal "S"; Terminal 'd' ]
    //           "A" := [ Terminal 'a'; ]
    //       ] }

    // let grammarWithoutLeftRecursions =
    //     { Initial = "S"
    //       Rules = set [
    //           // S -> Aa | b
    //           "S" := [ NonTerminal "A"; Terminal 'a' ]
    //           "S" := [ Terminal 'b'; ]
    //           // A -> bdA' | aA'
    //           "A" := [ Terminal 'b'; Terminal 'd'; NonTerminal "A'"]
    //           "A" := [ Terminal 'a'; NonTerminal "A'"]
    //           // A' -> cA' | adA' | &
    //           "A'" := [ Terminal 'c'; NonTerminal "A'"]
    //           "A'" := [ Terminal 'a'; Terminal 'd'; NonTerminal "A'"]
    //           "A'" := []
    //       ] }

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
            let follows = Grammar.followSets '$' notLL1
            let testFollow symbol expected =
                Expect.equal (Map.find symbol follows) expected $"FOLLOW({symbol})"
            testFollow "S" (set [ '$' ])
            testFollow "A" (set [ 'a' ])
            testFollow "B" (set [ 'b' ])
            testFollow "C" (set [ 'c'; 'a'; 'b'; '$' ])
            testFollow "D" (set [ 'a'; 'b'; 'c'; '$' ])

        // testCase "LEFT RECURSION sets" <| fun _ ->
        //     let testLeftRecursion grammar grammarExpected =
        //         Expect.equal (Grammar.eliminateLeftRecursions grammar) grammarExpected $"RECURSION({grammar})"
        //     testLeftRecursion grammarWithLeftRecursions grammarWithoutLeftRecursions

        // testCase "LEFT FACTOR sets" <| fun _ ->
        //     let testLeftFactor grammar grammarExpected =
        //         Expect.equal (Grammar.leftFactor grammar) grammarExpected $"FACTOR({grammar})"
        //     testLeftFactor GrammarToRefactor GrammarRefactored
    ]
