namespace Formal.Automata

[<System.Runtime.CompilerServices.Extension>]
module Automaton =
    type private DeterministicTransitions<'State, 'Input, 'Output when 'State: comparison and 'Input: comparison and 'Output: comparison> =
        Map<('State * 'Input), ('State * 'Output)>

    type private NonedeterministicTransitions<'State, 'Input, 'Output when 'State: comparison and 'Input: comparison and 'Output: comparison> =
        Map<('State * option<'Input>), Set<('State * 'Output)>>

    /// Trivial conversion of a deterministic transition table to a nondeterministic one.
    let indeterminize : DeterministicTransitions<'Q, 'I, 'O> -> NonedeterministicTransitions<'Q, 'I, 'O> =
        fun transitionTable ->
            Map.toSeq transitionTable
            |> Seq.map (fun ((q, i), (q', o)) -> (q, Some i), set [ q', o ])
            |> Map.ofSeq

    /// Converts a nondeterministic transition table to a deterministic one.
    ///
    /// This is NOT the inverse of `indeterminize`, since nondeterministic
    /// states and outputs get wrapped into sets of states and sets of outputs.
    let determinize : NonedeterministicTransitions<'Q, 'I, 'O> -> DeterministicTransitions<Set<'Q>, 'I, Set<'O>> =
        fun table -> failwith "FIXME: implement determinize"
