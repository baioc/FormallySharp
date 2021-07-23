/// Finite automata and regexps for the domain of regular formal languages.

namespace FormalLanguages


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type Regexp =
    // primitives, used as atomic literals
    | Void // Rejects all strings.
    | Empty // The empty word.
    | Symbol of char // A specific atomic symbol.
    // combinations, should only be constructed through custom operators
    | Alternation of Set<Regexp>
    | Concatenation of List<Regexp>
    | KleeneClosure of Regexp

    /// Choice: associative, neutral on `Void`, commutative and idempotent.
    static member (+)(r: Regexp, s: Regexp) =
        match r, s with
        // neutral on Void
        | Void, regex
        | regex, Void -> regex
        // sets guaratee associativity, commutativity and idempotency
        | Alternation a, Alternation b -> Set.union a b |> Alternation
        | Alternation s, r
        | r, Alternation s -> Set.add r s |> Alternation
        // if no argument was already a set, make one
        | r, s -> Set.ofArray [| r; s |] |> Alternation

    /// Sequence: associative, neutral on `Empty` and annihilates on `Void`.
    static member (*)(r: Regexp, s: Regexp) =
        match r, s with
        // neutral on Empty
        | Empty, regex -> regex
        | regex, Empty -> regex
        // annihilates on Void
        | Void, _ -> Void
        | _, Void -> Void
        // make sure we append in the right order, then we get associativity
        | Concatenation a, Concatenation b -> List.append a b |> Concatenation
        | Concatenation seq, regex -> List.append seq [ regex ] |> Concatenation
        | regex, Concatenation seq -> regex :: seq |> Concatenation
        | r, s -> [ r; s ] |> Concatenation

    /// Kleene closure: zero or more repetitions of a regex.
    static member (!*)(r: Regexp) =
        match r with
        // by definition, 0* = 1* = 1, where 0=Void and 1=Empty
        | Void
        | Empty -> Empty
        // it is also idempotent: (r*)* = r*
        | KleeneClosure regex
        | regex -> KleeneClosure regex

    /// Optional: equivalent to `Empty + r`.
    static member (!?)(r: Regexp) = Empty + r

    /// Positive closure: equivalent to `r * (!*r)`.
    static member (!+)(r: Regexp) = r * (!*r)

    /// Repetition: fixed number of repetitions of a regex in sequence.
    static member Pow(r: Regexp, n: int) =
        List.init n (fun _ -> r) |> Concatenation

module Regexp =
    let union (r: Regexp) (s: Regexp) = r + s
    let append (r: Regexp) (s: Regexp) = r * s
    let star (r: Regexp) = !*r

    let maybe (r: Regexp) = !?r
    let many (r: Regexp) = !+r
    let init (n: int) (r: Regexp) = r ** n

    let ofSeq seq =
        Seq.map Symbol seq |> Seq.toList |> Concatenation

    let ofSet set =
        Seq.map Symbol set |> Set.ofSeq |> Alternation


/// Defines the generic interface of Finite-State Automata with mutable state.
type IAutomaton<'symbol, 'state> =
    interface
        /// Current state, mutated on calls to `Step`.
        abstract member Current : 'state

        /// Whether or not the machine is currently in an accepting state.
        abstract member IsAccepting : bool

        /// Steps the machine forward based on its current state and the given input.
        abstract member Step : 'symbol -> unit

        /// Resets this machine to its initial configuration.
        abstract member Reset : unit -> unit
    end

/// Deterministic Finite Automaton (DFA) with optional explicit dead state.
type DeterministicAutomaton<'symbol, 'state when 'symbol: comparison and 'state: comparison>
    (
        transition: Map<'state * 'symbol, 'state>,
        initial: 'state,
        accepts: Set<'state>,
        ?dead: 'state
    ) =

    let mutable current = initial

    // steps forward by the given input, or keeps halted when in a dead state
    let step (current, input) =
        match dead with
        | None -> Map.find (current, input) transition // this may throw
        | Some definedDeadState ->
            Map.tryFind (current, input) transition
            |> Option.defaultValue definedDeadState

    interface IAutomaton<'symbol, 'state> with
        member _.Current = current
        member _.IsAccepting = Set.contains current accepts
        member _.Step(input) = current <- step (current, input)
        member _.Reset() = current <- initial

/// Non-deterministic Finite Automaton (NFA).
type NondeterministicAutomaton<'symbol, 'state when 'symbol: comparison and 'state: comparison>
    (
        transition: Map<'state * option<'symbol>, Set<'state>>,
        initial: 'state,
        accepts: Set<'state>
    ) =

    // local definition of the empty word
    let epsilon : option<'symbol> = None

    // dead state is always properly defined for NFAs
    let dead : Set<'state> = Set.empty

    // get the next state set while ignoring epsilon transitions
    let next (current, symbol) =
        Map.tryFind (current, symbol) transition
        |> Option.defaultValue dead

    // the machine is accepting whenever it is in at least one accepting state
    let hasAccepting states =
        Set.intersect accepts states |> Set.isEmpty |> not

    let rec epsilonReachable visited state =
        if Set.contains state visited then
            Set.empty
        else
            let visited = Set.add state visited
            let nextStates = next (state, epsilon)

            nextStates
            |> Seq.map (epsilonReachable visited)
            |> Set.unionMany
            |> Set.add state

    let mutable epsilonCache =
        new System.Collections.Generic.Dictionary<'state, Set<'state>>()

    let epsilonClosure state =
        if epsilonCache.ContainsKey state then
            epsilonCache.[state]
        else
            let closure = epsilonReachable Set.empty state
            epsilonCache.[state] <- closure
            closure

    let step (currentStates, input) =
        currentStates
        |> Seq.map
            (fun state ->
                next (state, input)
                |> Seq.map epsilonClosure
                |> Set.unionMany)
        |> Set.unionMany

    let mutable current = epsilonClosure initial

    interface IAutomaton<option<'symbol>, Set<'state>> with
        member _.Current = current
        member _.IsAccepting = hasAccepting current
        member _.Step(input) = current <- step (current, input)
        member _.Reset() = current <- epsilonClosure initial
