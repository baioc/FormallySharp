module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.Regular
open Shared


[<RequireQualifiedAccess>]
module Shared =
    open Formally.Automata.Tests
    open Formally.Regular.Tests
    open Formally.Converter.Tests
    open Formally.ContextFree.Tests

    let tests = testList "Shared" [
        // internal libraries
        Automaton.tests
        Regexp.tests
        Nfa.tests
        Converter.tests
        ContextFree.tests

        // shared application-specific logic
        testList "Business Logic" [
            testCase "Identifier validation" <| fun _ ->
                let message = "Should have been able to (in)validate identifier"
                Expect.isTrue (Identifier.isValid "test") message
                Expect.isTrue (Identifier.isValid "TEST") message
                Expect.isTrue (Identifier.isValid "_123_TeST") message
                Expect.isFalse (Identifier.isValid "a<te\"st_'>#1323") message
                Expect.isFalse (Identifier.isValid "") message

            testCase "Lexer compilation and execution" <| fun _ ->
                let x, y = Regexp.singleton 'x', Regexp.singleton 'y'
                let xs = !* x
                let xxy = x * (x + y) * (!? y)
                let lexer =
                    (Lexer.make << Map.ofSeq) [
                        "WS", Separator <| UserRegexp(@"\s", Regexp.ofSet " \t\n")
                        "Xs", TokenClass(UserRegexp(@"x*", xs), 0)
                        "XXY", TokenClass(UserRegexp(@"x(x|y)y?", xxy), 1)
                    ]
                let tokenize input = Lexer.tokenize lexer input |> List.ofSeq
                Expect.equal (tokenize "") [] "Should not tokenize on empty input"
                Expect.equal
                    (tokenize "xxxxxx")
                    [ Ok { Token = "Xs"; Lexeme = "xxxxxx"; Position = 0u } ]
                    "Should have applied the maximal munch rule"
                Expect.equal
                    (tokenize "xx")
                    [ Ok { Token = "XXY"; Lexeme = "xx"; Position = 0u } ]
                    "Should have considered priority and alternative accept"
                Expect.equal
                    (tokenize "xyxy")
                    [ Ok { Token = "XXY"; Lexeme = "xy"; Position = 0u }
                      Ok { Token = "XXY"; Lexeme = "xy"; Position = 2u } ]
                    "Should work without separator when one isn't needed"
                Expect.equal
                    (tokenize "xx xxxxy")
                    [ Ok { Token = "XXY"; Lexeme = "xx"; Position = 0u }
                      Ok { Token = "Xs"; Lexeme = "xxxx"; Position = 3u }
                      Error { String = "y"; Position = 7u } ]
                    "Should respect separator and not backtrack on error"
        ]
    ]
