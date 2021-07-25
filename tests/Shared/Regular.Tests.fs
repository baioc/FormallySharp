namespace Formal.Languages.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formal.Languages


[<RequireQualifiedAccess>]
module Regexp =
    open System

    type Random with
        member this.NextRegexp() =
            let rand = this.Next(128)
            let c = char rand
            if not (Char.IsControl c) then Regexp.ofChar c
            elif rand % 2 = 0 then Regexp.Zero
            else Regexp.One

    /// Randomly generates atomic regexps (symbols, zeros and ones).
    let randomRegexps() =
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
                    Expect.equal (!* !* r) (!*r) "For all r, (!* !* r) should be equal to (!*r)")

        testCase "Strings can be converted to regexps" <| fun _ ->
            let a = Regexp.ofChar 'a'
            let b = Regexp.ofChar 'b'
            let c = Regexp.ofChar 'c'
            Expect.equal (Regexp.ofSeq "abc") (a * b * c)
                "'abc' should be the equal to 'a' * 'b' * 'c'"

        testCase "Char ranges can be converted to regexps" <| fun _ ->
            let alpha = [ 'a' .. 'z' ]
            let alphaGroup = Seq.map Regexp.ofChar alpha |> Seq.fold (+) Regexp.Zero
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
                    Expect.equal (Regexp._Pow(r, 0)) Regexp.One "For all r, (r**0) should be equal to (1)"
                    Expect.equal (Regexp._Pow(r, 1)) r "For all r, (r**1) should be equal to (r)"
                    Expect.equal (Regexp._Pow(r, 2)) (r * r) "For all r, (r**2) should be equal to (r * r)")

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
                    Expect.equal (Regexp.init 3 a) (Regexp._Pow(a, 3)) "init should be an alterantive for (**)")
    ]
