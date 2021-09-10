namespace Formally.Regular.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open System

open Formally.Regular


[<AutoOpen>]
module Extensions =
    type Random with
        /// Randomly generates an atomic regexp (symbol, zero or one).
        member this.NextRegexp() =
            let rand = this.Next(128)
            let c = char rand
            if not (Char.IsControl c) then Regexp.singleton c
            elif rand % 2 = 0 then Regexp.Zero
            else Regexp.One


module Regexp =
    let randomRegexps () =
        let sampleSize = 300
        let rand = Random()
        Seq.init sampleSize (fun _ -> rand.NextRegexp())

    let tests = testList "Regexps" [
        testCase "Associativity of (+)" <| fun _ ->
            let a = randomRegexps()
            let b = randomRegexps()
            let c = randomRegexps()
            Seq.zip3 a b c
            |> Seq.iter
                (fun (a, b, c) ->
                    Expect.equal ((a + b) + c) (a + (b + c))
                        "For all a, b and c, ((a + b) + c) should be equal to (a + (b + c))")

        testCase "Zero is the identity of (+)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (Regexp.Zero + r) r "For all r, (0 + r) should be equal to (r)"
                    Expect.equal (r + Regexp.Zero) r "For all r, (r + 0) should be equal to (r)")

        testCase "Commutativity of (+)" <| fun _ ->
            let a = randomRegexps()
            let b = randomRegexps()
            Seq.zip a b
            |> Seq.iter
                (fun (a, b) ->
                    Expect.equal (a + b) (b + a)
                        "For all a and b, (a + b) should be equal to (b + a)")

        testCase "Idempotency of (+)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (r + r) r "For all r, (r + r) should be equal to (r)")

        testCase "Associativity of (*)" <| fun _ ->
            let a = randomRegexps()
            let b = randomRegexps()
            let c = randomRegexps()
            Seq.zip3 a b c
            |> Seq.iter
                (fun (a, b, c) ->
                    Expect.equal ((a * b) * c) (a * (b * c))
                        "For all a, b and c, ((a * b) * c) should be equal to (a * (b * c))")

        testCase "One is the identity of (*)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (Regexp.One * r) r "For all r, (1 * r) should be equal to (r)"
                    Expect.equal (r * Regexp.One) r "For all r, (r * 1) should be equal to (r)")

        testCase "Zero annihilates by (*)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (Regexp.Zero * r) Regexp.Zero
                        "For all r, (0 * r) should be equal to (0)"
                    Expect.equal (r * Regexp.Zero) Regexp.Zero
                        "For all r, (r * 0) should be equal to (0)")

        testCase "The Kleene closure of Zero is One" <| fun _ ->
            Expect.equal (!* Regexp.Zero) Regexp.One "(!* 0) should be equal to (1)"

        testCase "The Kleene closure of One is One" <| fun _ ->
            Expect.equal (!* Regexp.One) Regexp.One "(!* 1) should be equal to (1)"

        testCase "Fixpoint property of the Kleene closure" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (!* (!* r)) (!*r) "For all r, (!* (!* r)) should be equal to (!*r)"
                    Expect.equal (!* (!? r)) (!*r) "For all r, (!* (!? r) should be equal to (!*r)")

        testCase "Build regexp from char range" <| fun _ ->
            let a = Regexp.singleton 'a'
            let b = Regexp.singleton 'b'
            let c = Regexp.singleton 'c'
            Expect.equal (Regexp.ofSeq "abc") (a * b * c) "\"abc\" should be equal to 'a'*'b'*'c'"
            let alpha = [ 'a' .. 'z' ]
            let alphaGroup = Seq.map Regexp.singleton alpha |> Seq.fold (+) Regexp.Zero
            Expect.equal (Regexp.ofSet alpha) alphaGroup "['a' .. 'z'] should be equal to /a-z/"

        testCase "Optional operator (!?)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (!?r) (Regexp.empty + r)
                        "For all r, (!?r) should be equal to (1 + r)")

        testCase "Positive closure operator (!+)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (!+r) (r * !*r) "For all r, (!+r) should be equal to (r * !*r)")

        testCase "Repetition operator (**)" <| fun _ ->
            randomRegexps()
            |> Seq.iter
                (fun r ->
                    Expect.equal (r ** 0) Regexp.One "For all r, (r**0) should be equal to (1)"
                    Expect.equal (r ** 1) r "For all r, (r**1) should be equal to (r)"
                    Expect.equal (r ** 2) (r * r) "For all r, (r**2) should be equal to (r * r)")

        testCase "Non-algebraic names" <| fun _ ->
            Expect.equal Regexp.none Regexp.Zero "none should be an alias of Zero"
            Expect.equal Regexp.empty Regexp.One "empty should be an alias of One"
            let a = randomRegexps()
            let b = randomRegexps()
            Seq.zip a b
            |> Seq.iter
                (fun (a, b) ->
                    Expect.equal (Regexp.union a b) (a + b) "union should be an alias of (+)"
                    Expect.equal (Regexp.append a b) (a * b) "append should be an alias of (*)"
                    Expect.equal (Regexp.star a) (!* a) "star should be an alias of (!*)"
                    Expect.equal (Regexp.maybe a) (!? a) "maybe should be an alias of (!?)"
                    Expect.equal (Regexp.many a) (!+ a) "many should be an alias of (!+)"
                    Expect.equal (Regexp.init 3 a) (a ** 3) "init should be an alterantive for (**)")
    ]


module Nfa =
    open Formally.Automata
    open Formally.Automata.Tests.Extensions

    // functional DSL style
    let map s = Map.ofSeq s
    let (=>) (previous, input) next = (previous, input), next

    // DFA over {0,1} that accepts even binary numbers with at least one digit
    let even =
        { Dead = -1
          Accepting = set [ 2 ]
          Current = 0
          Transitions =
              map [ (0, '0') => 2
                    (0, '1') => 1
                    (2, '0') => 2
                    (2, '1') => 1
                    (1, '0') => 2
                    (1, '1') => 1 ] }

    // NFA over {a,b} that accepts strings containing "abba" as a substring
    let abba =
        { Current = set [ "$" ]
          Accepting = set [ "ABBA" ]
          Transitions =
              map [ ("$",    Some 'a') => set [ "$"; "A" ]
                    ("$",    Some 'b') => set [ "$" ]
                    ("A",    Some 'b') => set [ "AB" ]
                    ("AB",   Some 'b') => set [ "ABB" ]
                    ("ABB",  Some 'a') => set [ "ABBA" ]
                    ("ABBA", Some 'a') => set [ "ABBA" ]
                    ("ABBA", Some 'b') => set [ "ABBA" ] ] }

    // NFA with cyclic and reflexive epsilon transitions. rejects all input
    let cyclic: Nfa<char> =
        { Current = set [ 'A' ]
          Accepting = set []
          Transitions =
              map [ ('A', None) => set [ 'A'; 'B'; 'C' ]
                    ('B', None) => set []
                    ('C', None) => set [ 'B'; 'A' ] ] }

    let tests = testList "Finite Automata" [
        testCase "State set inference" <| fun _ ->
            let message = "Should have inferred state set"
            Expect.equal even.States (set [ -1; 0; 2; 1 ]) message
            Expect.equal abba.States (set [ "$"; "A"; "AB"; "ABB"; "ABBA" ]) message
            Expect.equal cyclic.States (set [ 'A'; 'B'; 'C' ]) message

        testCase "Alphabet inference" <| fun _ ->
            let message = "Should have inferred input alphabet"
            Expect.equal even.Alphabet (set [ '0'; '1' ]) message
            Expect.equal abba.Alphabet (set [ 'a'; 'b' ]) message
            Expect.equal cyclic.Alphabet (set []) message

        testCase "DFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let number = rand.Next()
                let binary = Convert.ToString(number, 2)
                let expected = if number % 2 = 0 then 2 else 1
                Expect.trace even binary expected

        testCase "NFA execution" <| fun _ ->
            let rand = Random()
            for _ in 1 .. 100 do
                let bits = Convert.ToString(rand.Next(), 2)
                let input = String.map (fun c -> if c = '0' then 'a' else 'b') bits
                let expected = input.Contains "abba"
                let actual =
                    Automaton.exec (Seq.map Some input) abba
                    |> Set.intersect abba.Accepting
                    |> (not << Set.isEmpty)
                Expect.equal actual expected $"Should have halted with {expected} on \"{input}\""

        testCase "Implicit dead transitions" <| fun _ ->
            let message = "Should have transitioned to dead state"
            let deadEven = { even with Current = even.Dead } :> IAutomaton<_, _, _>
            let deadAbba = { abba with Current = abba.Dead } :> IAutomaton<_, _, _>
            Expect.equal (Automaton.step '?' even) ((), deadEven) message
            Expect.equal (Automaton.step (Some '?') abba) ((), deadAbba) message

        testCase "Epsilon transitions" <| fun _ ->
            let abbaEps = Automaton.step None abba |> snd |> Automaton.view
            let cyclicEps = Automaton.step None cyclic |> snd |> Automaton.view
            Expect.equal abbaEps (set [ "$" ]) "Closure from a state should contain itself"
            Expect.equal cyclicEps (set [ 'A'; 'B'; 'C' ]) "Should work with cyclic closures"

        testCase "Map and filter" <| fun _ ->
            let namedEven =
                even
                |> Dfa.map (function 0 -> "zero" | 1 -> "odd" | 2 -> "even" | _ -> "dead")
            Expect.equal namedEven.States (set [ "dead"; "zero"; "odd"; "even" ])
                "Map should transform all states"
            let unaryEven =
                even
                |> Dfa.filter (fun (_, symbol) _ -> symbol <> '0')
            Expect.equal unaryEven.Alphabet (set [ '1' ])
                "Filter should have removed all references to zero"

        testCase "DFA indeterminization" <| fun _ ->
            let nondetEven =
                { Current = set [ 0 ]
                  Accepting = set [ 2 ]
                  Transitions =
                      map [ (0, Some '0') => set [ 2 ]
                            (0, Some '1') => set [ 1 ]
                            (2, Some '0') => set [ 2 ]
                            (2, Some '1') => set [ 1 ]
                            (1, Some '0') => set [ 2 ]
                            (1, Some '1') => set [ 1 ] ] }
            Expect.equal (Nfa.ofDfa even) nondetEven "Should have been a trivial conversion"

        testCase "NFA union" <| fun _ ->
            let evenOrAbba =
                { Current = set [ "0"; "$" ]
                  Accepting = set [ "2"; "ABBA" ]
                  Transitions =
                      map [ ("0",    Some '0') => set [ "2" ]
                            ("0",    Some '1') => set [ "1" ]
                            ("2",    Some '0') => set [ "2" ]
                            ("2",    Some '1') => set [ "1" ]
                            ("1",    Some '0') => set [ "2" ]
                            ("1",    Some '1') => set [ "1" ]
                            ("$",    Some 'a') => set [ "$"; "A" ]
                            ("$",    Some 'b') => set [ "$" ]
                            ("A",    Some 'b') => set [ "AB" ]
                            ("AB",   Some 'b') => set [ "ABB" ]
                            ("ABB",  Some 'a') => set [ "ABBA" ]
                            ("ABBA", Some 'a') => set [ "ABBA" ]
                            ("ABBA", Some 'b') => set [ "ABBA" ] ] }
            let even = Nfa.ofDfa even
            let discriminated = Nfa.union even abba
            let unified =
                discriminated
                |> Nfa.map (function // no name conflicts -> this is safe
                            | Choice1Of2 i -> string i
                            | Choice2Of2 s -> s)
            Expect.equal unified evenOrAbba "Should have preserved individual structure"

        testCase "NFA determinization" <| fun _ ->
            let detAbba =
                { Dead = set []
                  Current = set [ "$" ]
                  Accepting =
                      set [ set [ "$"; "A"; "ABBA" ]
                            set [ "$"; "AB"; "ABBA" ]
                            set [ "$"; "ABB"; "ABBA" ]
                            set [ "$"; "ABBA" ] ]
                  Transitions =
                      map [ (set [ "$" ],                'a') => set [ "$"; "A" ]
                            (set [ "$" ],                'b') => set [ "$"; ]
                            (set [ "$"; "A" ],           'a') => set [ "$"; "A" ]
                            (set [ "$"; "A" ],           'b') => set [ "$"; "AB" ]
                            (set [ "$"; "AB" ],          'a') => set [ "$"; "A" ]
                            (set [ "$"; "AB" ],          'b') => set [ "$"; "ABB" ]
                            (set [ "$"; "ABB" ],         'a') => set [ "$"; "A"; "ABBA" ]
                            (set [ "$"; "ABB" ],         'b') => set [ "$" ]
                            (set [ "$"; "A"; "ABBA" ],   'a') => set [ "$"; "A"; "ABBA" ]
                            (set [ "$"; "A"; "ABBA" ],   'b') => set [ "$"; "AB"; "ABBA" ]
                            (set [ "$"; "AB"; "ABBA" ],  'a') => set [ "$"; "A"; "ABBA" ]
                            (set [ "$"; "AB"; "ABBA" ],  'b') => set [ "$"; "ABB"; "ABBA" ]
                            (set [ "$"; "ABB"; "ABBA" ], 'a') => set [ "$"; "A"; "ABBA" ]
                            (set [ "$"; "ABB"; "ABBA" ], 'b') => set [ "$"; "ABBA" ]
                            (set [ "$"; "ABBA" ],        'a') => set [ "$"; "A"; "ABBA" ]
                            (set [ "$"; "ABBA" ],        'b') => set [ "$"; "ABBA" ] ] }
            Expect.equal (Nfa.toDfa abba) detAbba "Should have been fully determinized"

        testCase "DFA from Regexp" <| fun _ ->
            let expected =
                { Dead = set []
                  Current = set [ 1; 2; 3 ]
                  Accepting = set [ set [ 1; 2; 3; 6 ] ]
                  Transitions =
                      map [ (set [ 1; 2; 3 ],    'a') => set [ 1; 2; 3; 4 ]
                            (set [ 1; 2; 3 ],    'b') => set [ 1; 2; 3 ]
                            (set [ 1; 2; 3; 4 ], 'a') => set [ 1; 2; 3; 4 ]
                            (set [ 1; 2; 3; 4 ], 'b') => set [ 1; 2; 3; 5 ]
                            (set [ 1; 2; 3; 5 ], 'a') => set [ 1; 2; 3; 4 ]
                            (set [ 1; 2; 3; 5 ], 'b') => set [ 1; 2; 3; 6 ]
                            (set [ 1; 2; 3; 6 ], 'a') => set [ 1; 2; 3; 4 ]
                            (set [ 1; 2; 3; 6 ], 'b') => set [ 1; 2; 3 ] ] }
            let a = Regexp.singleton 'a'
            let b = Regexp.singleton 'b'
            let regexp = (!* (a + b)) * a * b * b
            Expect.equal (Dfa.ofRegexp regexp) expected "Should have been correctly converted"
    ]
