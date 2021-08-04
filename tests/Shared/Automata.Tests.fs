namespace Formally.Automata.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Formally.Automata


[<AutoOpen>]
module Extensions =
    module Automaton =
        // XXX: helps Fable get type info of generic parameter
#if FABLE_COMPILER
        let inline exec inputs automaton =
#else
        let exec inputs automaton =
#endif
            Automaton.trace inputs automaton
            |> Seq.last
            |> fun ((q, i), (q', o)) -> q'

    module Expect =
        /// Runs an automaton over inputs and checks if it ends in the expected state.
#if FABLE_COMPILER
        let inline trace automaton inputs expected =
#else
        let trace automaton inputs expected =
#endif
                Expect.equal (Automaton.exec inputs automaton) expected
                    "Should have reached the expected state"


module Automaton =
    // mutable single-step Fibonacci automaton
    type Fibonacci(f0, f1) =
        let mutable current: bigint = f0
        let mutable previous: bigint = f1 - f0
        new() = Fibonacci(0I, 1I)
        interface IAutomaton<bigint, unit, bigint> with
            override __.View = current
            override this.Step _ =
                let next = previous + current
                previous <- current
                current <- next
                next, this :> IAutomaton<_, _, _>

    // functional counter automaton
    let inline sumFrom initial = Automaton.fold (+) initial

    let tests = testList "Automata" [
        testCase "Functional style" <| fun _ ->
            let fromZero = sumFrom 0I
            let _, fromOne = (1I, fromZero) ||> Automaton.step
            Expect.equal (Automaton.view fromZero) 0I "Should have maintained original state"
            Expect.equal (Automaton.view fromOne) 1I "Should have been incremented by 1"

        testCase "Mutable style" <| fun _ ->
            let fib0 = Fibonacci()
            Expect.equal (Automaton.view fib0) 0I "Should have been equal"
            let _, fib5 =
                Automaton.withOutputAdapter ignore fib0
                |> Automaton.step ()
                ||> Automaton.step
                ||> Automaton.step
                ||> Automaton.step
                ||> Automaton.step
            Expect.equal (Automaton.view fib5) 5I "Should have been equal"
            Expect.equal (Automaton.view fib0) 5I "Should have been mutated"

        testCase "Parallel combination" <| fun _ ->
            let zipped =
                Automaton.zip (sumFrom 0) (Fibonacci())
                |> Automaton.withInputAdapter (fun () -> 1, ())
                |> Automaton.withOutputAdapter ignore
            let fibs =
                zipped
                |> Automaton.trace (Seq.initInfinite ignore)
                |> Seq.map (fun ((a, i), (b, o)) -> a)
            let fib16 = Seq.item 16 fibs
            Expect.equal fib16 (16, 987I) "Should have been equal"

        testCase "Sequential composition" <| fun _ ->
            let delta = 1
            let initial = 1
            let mutable series =
                Automaton.fold (fun previous () -> previous + delta) initial
                |> Automaton.compose (sumFrom initial)
                |> Automaton.withViewAdapter (fun (term, partialSum) -> partialSum)
            for n in 1 .. 100 do
                let nthTerm = initial + delta*(n - 1)
                let expected = n * (initial + nthTerm) / 2
                Expect.equal (Automaton.view series) expected "Should have been equal"
                let _, next = Automaton.step () series
                series <- next
    ]
