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
            let string = "oi"
            let teste = Set.add(string)
            Expect.equal(Grammar.first()) (teste) "deu boa"
    ]