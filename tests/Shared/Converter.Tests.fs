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
        testCase "string ab gera regex ab" <| fun _ ->
            let string = "ab"
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (Regexp.ofSeq "ab") "should be equal"

        testCase "a|b" <| fun _ ->
            let string = "a|b"
            let regexA = Regexp.singleton('a')
            let regexB = Regexp.singleton('b')
            let regex = regexA + regexB

            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (regex) "should be equal"

        testCase "[A-Z]" <| fun _ ->
            let string = "[A-Z]"
            let regex = Regexp.ofSet(['A' .. 'Z'])
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (regex) "should be equal"

        testCase "[A-Za-z]" <| fun _ ->
            let string = "[A-Za-z]"
            let regexA = Regexp.ofSet(['A' .. 'Z'])
            let regexB = Regexp.ofSet(['a' .. 'z'])
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (regexA + regexB) "should be equal"

        testCase "([0-9]|a)*" <| fun _ ->
            let string = "([0-9]|a)*"
            let regexA = Regexp.ofSet(['0' .. '9'])
            let regexB = Regexp.singleton('a')
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (!*(regexA + regexB)) "should be equal"

        testCase "([0-9]|a)*(abab|[0-9])" <| fun _ ->
            let string = "([0-9]|a)*(abab|[0-9])"
            let regex = !*(Regexp.ofSet(['0' .. '9']) + Regexp.singleton('a')) * ( (Regexp.ofSeq "abab") + Regexp.ofSet(['0' .. '9']) )
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (regex) "should be equal"

        testCase "&" <| fun _ ->
            let string = "&"
            let regex = Regexp.empty
            Expect.equal (Converter.convertRegularDefinitionTextToRegexp(string)) (regex) "should be equal"
    ]