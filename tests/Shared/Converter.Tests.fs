namespace Formally.Converter.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open System

open Formally.Converter

open Formally.Regular

module Converter =

    let tests = testList "Converter" [
        testCase "primeiro" <| fun _ ->
            let string = "ab"
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (Regexp.ofSeq "ab") "should be equal"
    ]