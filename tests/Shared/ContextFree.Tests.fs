namespace Formally.ContextFree.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.ContextFree


module ContextFree = 

    let tests = testList "First" [
        testCase "primeiro" <| fun _ ->
            let string = "Oi"
            let teste = Set.add string Set.empty
            Expect.equal (Grammar.first (Terminal "oi" ) { Initial = "inicial"; Rules = Set.empty } ) (teste) "deu erro"
    ]