/// Finite automata and regexps for the domain of regular formal languages.
///
/// The names of generic types defined here are intentionally long: consumers
/// of this library should prefer using operators and the char-specific types.

namespace FormalLanguages


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type RegularExpression<'symbol when 'symbol: comparison> =
    // primitives, used as atomic literals
    | None // Rejects all strings.
    | Empty // The empty string.
    | Symbol of 'symbol // A specific atomic symbol.
    // combinations, should only be constructed through custom operators
    | Alternation of Set<RegularExpression<'symbol>>
    | Concatenation of List<RegularExpression<'symbol>>
    | KleeneClosure of RegularExpression<'symbol>

    /// Choice: associative, neutral on `None`, commutative and idempotent.
    static member (+)(r: RegularExpression<'symbol>, s: RegularExpression<'symbol>) =
        match r, s with
        // neutral on None
        | None, regex
        | regex, None -> regex
        // sets guaratee associativity, commutativity and idempotency
        | Alternation a, Alternation b -> Set.union a b |> Alternation
        | Alternation s, r
        | r, Alternation s -> Set.add r s |> Alternation
        // if no argument was already a set, make one
        | r, s -> Set.ofArray [| r; s |] |> Alternation

    /// Sequence: associative, neutral on `Empty` and annihilates on `None`.
    static member (*)(r: RegularExpression<'symbol>, s: RegularExpression<'symbol>) =
        match r, s with
        // neutral on Empty
        | Empty, regex -> regex
        | regex, Empty -> regex
        // annihilates on None
        | None, _ -> None
        | _, None -> None
        // make sure we append in the right order, then we get associativity
        | Concatenation a, Concatenation b -> List.append a b |> Concatenation
        | Concatenation seq, regex -> List.append seq [ regex ] |> Concatenation
        | regex, Concatenation seq -> regex :: seq |> Concatenation
        | r, s -> [ r; s ] |> Concatenation

    /// Kleene closure: zero or more repetitions of a regex.
    static member (!*)(r: RegularExpression<'symbol>) =
        match r with
        // by definition, 0* = 1* = 1, where 0=None and 1=Empty
        | None
        | Empty -> Empty
        // it is also idempotent: (r*)* = r*
        | KleeneClosure regex
        | regex -> KleeneClosure regex

    /// Optional: equivalent to `Empty + r`.
    static member (!?)(r: RegularExpression<'symbol>) = Empty + r

    /// Positive closure: equivalent to `r * (!*r)`.
    static member (!+)(r: RegularExpression<'symbol>) = r * (!*r)

    /// Repetition: fixed number of repetitions of a regex in sequence.
    static member Pow(r: RegularExpression<'symbol>, n: int) =
        List.init n (fun _ -> r) |> Concatenation

/// The preferred type of algebraic regular expressions.
type Regexp = RegularExpression<char>

module Regexp =
    let union (r: RegularExpression<'s>) (s: RegularExpression<'s>) = r + s
    let append (r: RegularExpression<'s>) (s: RegularExpression<'s>) = r * s
    let star (r: RegularExpression<'s>) = !*r

    let maybe (r: RegularExpression<'s>) = !?r
    let many (r: RegularExpression<'s>) = !+r
    let init (n: int) (r: RegularExpression<'s>) = r ** n

    let ofSeq seq =
        Seq.map Symbol seq |> Seq.toList |> Concatenation

    let ofSet set =
        Seq.map Symbol set |> Set.ofSeq |> Alternation


/// Defines a Deterministic Finite Automaton (DFA). TODO: operations between FAs
type FiniteAutomaton<'symbol, 'state when 'symbol: comparison and 'state: comparison> =
    { Transitions: Map<'state * 'symbol, 'state>
      Current: 'state (* also used as the initial state *)
      Accepting: Set<'state> }
    member this.States =
        Map.toSeq this.Transitions |> Seq.map (fst >> fst)

    member this.Alphabet =
        Map.toSeq this.Transitions |> Seq.map (fst >> snd)

/// The preferred type of finite automata.
type Automaton<'q when 'q: comparison> = FiniteAutomaton<char, option<'q>>

module Automaton =
    let step input automaton =
        { automaton with
              Current = Map.find (automaton.Current, input) automaton.Transitions }

    let tryStep input automaton =
        match Map.tryFind (automaton.Current, input) automaton.Transitions with
        | Option.None -> Option.None
        | Some nextState -> Some { automaton with Current = nextState }

    let isAccepting automaton =
        Set.contains automaton.Current automaton.Accepting
