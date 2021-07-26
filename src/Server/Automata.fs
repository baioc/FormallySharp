namespace Formal.Automata

[<System.Runtime.CompilerServices.Extension>]
module Automaton =
    /// Converts a nondeterministic transition table to a deterministic one.
    ///
    /// This is NOT the inverse of `indeterminize`, since nondeterministic
    /// states and outputs get wrapped into sets of states and sets of outputs.
    let determinize
        (transitionTable: Map<('State * option<'Input>), Set<'State>>)
        : Map<(Set<'State> * 'Input), Set<'State>> =
        failwith "FIXME: implement determinize"
