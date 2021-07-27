namespace Formal.Languages

[<System.Runtime.CompilerServices.Extension>]
[<RequireQualifiedAccess>]
module Automaton =
    open System

    /// Base64-encoded string hash of an automaton.
    let inline hash automaton =
        string automaton
        |> Text.Encoding.UTF8.GetBytes
        // XXX: since Fable doesn't have built-in hashes, we just don't
        // |> (Security.Cryptography.MD5.Create()).ComputeHash
        |> Convert.ToBase64String

    let private renameAutomaton prefix automaton =
        let rename state = sprintf "%s:%s" prefix state

        { Current = Set.map rename automaton.Current
          Accepting = Set.map rename automaton.Accepting
          Transitions =
              Map.toSeq automaton.Transitions
              |> Seq.map (fun ((q, a), q') -> (rename q, a), Set.map rename q')
              |> Map.ofSeq }

    /// Union of two NFAs through epsilon transitions.
    ///
    /// In order to preserve the original graphs within the union, we rename all
    /// states by prefixing them with the `Automaton.hash` of their automata + `:`.
    let union a b =
        let aPrefix = hash a
        let a = renameAutomaton aPrefix a

        let bPrefix = hash b
        let b = renameAutomaton bPrefix b

        let newInitialState = hash $"{aPrefix}|{bPrefix}"
        let transitionsOf x = x.Transitions |> Map.toSeq

        { Accepting = Set.union a.Accepting b.Accepting
          Current = set [ newInitialState ]
          Transitions =
              Seq.append (transitionsOf a) (transitionsOf b)
              |> Map.ofSeq
              |> Map.add (newInitialState, None) (Set.union a.Current b.Current) }
