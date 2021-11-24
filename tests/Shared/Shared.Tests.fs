module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.Automata
open Formally.Regular
open Formally.ContextFree
open Shared


[<RequireQualifiedAccess>]
module Shared =
    open Formally.Automata.Tests
    open Formally.Regular.Tests
    open Formally.ContextFree.Tests

    let (:=) head body = head, body

    let tests = testList "Shared" [
        // internal libraries
        Automaton.tests
        Regexp.tests
        Nfa.tests
        Grammar.tests
        Dpda.tests

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
                let tokenize input = input |> Lexer.tokenize lexer |> List.ofSeq
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
                      Error { Irritant = "y"; Position = 7u } ]
                    "Should respect separator and not backtrack on error"

            testCase "LL(1) parser compilation and execution" <| fun _ ->
                let grammar =
                    { Initial = "E"
                      Rules = set [
                          // E -> T E'
                          "E"  := [ NonTerminal "T"; NonTerminal "E'" ]
                          // E' -> + T E' | &
                          "E'" := [ Terminal "+"; NonTerminal "T"; NonTerminal "E'" ]
                          "E'" := []
                          // T -> F T'
                          "T"  := [ NonTerminal "F"; NonTerminal "T'" ]
                          // T' -> * F T' | &
                          "T'" := [ Terminal "*"; NonTerminal "F"; NonTerminal "T'" ]
                          "T'" := []
                          // F -> ( E ) | id
                          "F"  := [ Terminal "("; NonTerminal "E"; Terminal ")" ]
                          "F"  := [ Terminal "id" ]
                      ] }

                let expectedFirsts =  Map.ofSeq [
                    "E"  := set [ Some "("; Some "id" ]
                    "E'" := set [ Some "+"; None ]
                    "T"  := set [ Some "("; Some "id" ]
                    "T'" := set [ Some "*"; None ]
                    "F"  := set [ Some "("; Some "id" ]
                ]

                let expectedFollows =  Map.ofSeq [
                    "E"  := set [ "$"; ")" ]
                    "E'" := set [ "$"; ")" ]
                    "T"  := set [ "+"; "$"; ")" ]
                    "T'" := set [ "+"; "$"; ")" ]
                    "F"  := set [ "*"; "+"; "$"; ")" ]
                ]

                let firsts =
                    grammar.NonTerminals
                    |> Seq.map (fun x -> x, Grammar.first [ NonTerminal x ] grammar)
                    |> Map.ofSeq

                Expect.equal firsts expectedFirsts "First sets"
                Expect.equal (Grammar.followSets grammar "$") expectedFollows "Follow sets"

                match Parser.make grammar with
                | Error table ->
                    failwithf "Should have compiled the LL(1) grammar %A" table
                | Ok parser ->
                    let tokens =
                        List.mapi (fun i x -> { Token = x; Lexeme = x; Position = uint i })
                    let tests = [
                        tokens [], false
                        tokens [ "id" ], true
                        tokens [ "("; ")" ], false
                        tokens [ "id"; "+"; "("; "id"; "*"; "id"; ")" ], true
                    ]
                    for case, expected in tests do
                        let actual = Parser.accepts parser case
                        Expect.equal actual expected $"Wrong output for input stream {case}"

            testCase "LL(1) grammar verification" <| fun _ ->
                let notLLK =
                    { Initial = "S"
                      Rules = set [
                          // S -> A | B
                          "S" := [ NonTerminal "A"]
                          "S" := [ NonTerminal "B" ]
                          // A -> a A b | ε
                          "A" := [ Terminal "a"; NonTerminal "A"; Terminal "b" ]
                          "A" := []
                          // B -> a B b b | ε
                          "B" := [ Terminal "a"; NonTerminal "B"; Terminal "b"; Terminal "b" ]
                          "B" := []
                      ] }
                let ll1 = match Parser.make notLLK with Error _ -> false | Ok _ -> true
                Expect.equal ll1 false "This grammar is not LL(k)"
        ]
    ]
