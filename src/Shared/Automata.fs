namespace Formal.Automata

/// Generalized transducer with viewable state.
type IAutomaton<'State, 'Input, 'Output> =
    /// Steps the machine forward by the given input, producing some output.
    abstract member Step : 'Input -> IAutomaton<'State, 'Input, 'Output> * 'Output

    /// Get a view of the state the machine is currently in
    abstract member View : 'State

/// API for dealing with various types of automata.
module Automaton =
    let inline step (automaton: IAutomaton<_, _, _>) input = automaton.Step input
    let inline view (automaton: IAutomaton<_, _, _>) = automaton.View

    /// <summary>
    /// Feeds an input sequence to a machine while keeping track of how it reacts.
    /// </summary>
    ///
    /// <returns>
    /// Returns a sequence of transition logs `(q, i), (q', o)`, indicating the
    /// machine outputted `o` when transitioning from `q` to `q'` by input `i`.
    /// </returns>
    let rec trace (automaton: IAutomaton<_, _, _>) inputs =
        if Seq.isEmpty inputs then
            Seq.empty
        else
            seq {
                let q = view automaton
                let i = Seq.head inputs
                let automaton, o = step automaton i
                let q' = view automaton
                yield (q, i), (q', o)
                let inputs = Seq.tail inputs
                yield! trace automaton inputs
            }

    let epsilon = None

    /// Finds the set reachable by epsilon transitions from a given state.
    let epsilonClosure table state =
        let nextStates arc =
            Map.tryFind arc table |> Option.defaultValue Set.empty

        // depth-first traversal in a possibly cyclic graph
        let rec epsilonReachable visited state =
            if Set.contains state visited then
                set []
            else
                let visited = Set.add state visited

                nextStates (state, epsilon)
                |> Seq.map (epsilonReachable visited)
                |> Set.unionMany
                |> Set.add state

        epsilonReachable Set.empty state

    /// Trivial mapping of deterministic to nondeterministic transitions.
    let inline indeterminize transitionTable =
        Map.toSeq transitionTable
        |> Seq.map (fun ((q, i), q') -> (q, Some i), set [ q' ])
        |> Map.ofSeq
