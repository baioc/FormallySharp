namespace Formally.Automata


/// Generalized automaton with viewable state.
type IAutomaton<'State, 'Input, 'Output> =
    /// Steps the machine forward by the given input, producing some output.
    abstract member Step : 'Input -> 'Output * IAutomaton<'State, 'Input, 'Output>

    /// Get a view of the state the machine is currently in
    abstract member View : 'State


/// API for using and combining generic automata as synchronous circuits.
[<RequireQualifiedAccess>]
module Automaton =
    /// Stepping function that can be piped with the (||>) operator.
    let step input (automaton: IAutomaton<_, _, _>) = automaton.Step input

    let view (automaton: IAutomaton<_, _, _>) = automaton.View

    /// <summary>
    /// Feeds an input stream to a machine while keeping track of how it reacts.
    /// </summary>
    ///
    /// <returns>
    /// Returns a stream of trace logs `(a, i), (b, o)`, indicating the
    /// machine outputted `o` when transitioning from `a` to `b` by input `i`.
    /// </returns>
    let rec trace inputs automaton =
        if Seq.isEmpty inputs then
            Seq.empty
        else
            seq {
                let a = view automaton
                let i = Seq.head inputs
                let o, next = step i automaton
                let b = view next
                yield (a, i), (b, o)
                let rest = Seq.tail inputs
                yield! trace rest next
            }

    // triple adapter combinator
    let rec private withAdapters viewAdapter inputAdapter outputAdapter automaton =
        { new IAutomaton<_, _, _> with
            member __.View = view automaton |> viewAdapter

            member __.Step input =
                let output, next = step (inputAdapter input) automaton
                (outputAdapter output), (withAdapters viewAdapter inputAdapter outputAdapter next) }

    /// Wraps an automaton with a view adapter.
    let withViewAdapter adapter = withAdapters adapter id id

    /// Wraps an automaton with an input adapter.
    let withInputAdapter adapter = withAdapters id adapter id

    /// Wraps an automaton with an output adapter.
    let withOutputAdapter adapter = withAdapters id id adapter

    /// Combines two automata sequentially as if composing functions (and as in
    /// function composition, `compose g f(x)` = `g(f(x))`, so mind the order).
    let rec compose outer inner =
        { new IAutomaton<_, _, _> with
            member __.Step x =
                let y, nextInner = step x inner
                let z, nextOuter = step y outer
                z, (compose nextOuter nextInner)

            member __.View = view inner, view outer }

    /// Combines two automata in synchronous parallelism.
    let rec zip a b =
        { new IAutomaton<_, _, _> with
            member __.Step input =
                let inA, inB = input
                let outA, nextA = step inA a
                let outB, nextB = step inB b
                (outA, outB), (zip nextA nextB)

            member __.View = view a, view b }

    /// Makes a probe machine that simply pipes all input to its output.
    let rec makeProbe initialValue =
        { new IAutomaton<_, _, _> with
            member __.View = initialValue
            member __.Step x = x, (makeProbe x) }
