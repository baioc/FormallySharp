/// Regexp algebra and finite automata for the domain of regular languages.

namespace Formally.Regular

type private Symbol = char


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type Regexp =
    private
    | Symbol of Symbol
    | Alternation of Set<Regexp>
    | Concatenation of Regexp list
    | KleeneClosure of Regexp

    static member Zero = Alternation Set.empty
    static member One = Concatenation []

    /// Choice operator, can accept either one of the given regexps.
    static member (+)(r: Regexp, s: Regexp) =
        // active pattern to help identify special cases
        let (|Zero|Regexp|) r =
            if r = Regexp.Zero then Zero r else Regexp r

        match r, s with
        // if any operand is zero, return the other
        | Zero _, regex
        | regex, Zero _ -> regex
        // when both are sets, unite them
        | Alternation r, Alternation s -> Alternation <| Set.union r s
        // when just one is a set, add the other one to the set
        | Alternation set, regexp
        | regexp, Alternation set -> Alternation <| Set.add regexp set
        // when both are not sets, either apply idempotency or make a new set
        | r, s -> if r = s then r else Alternation <| set [ r; s ]

    /// Concatenation operator, must accept the first, then the second regexp.
    static member (*)(r: Regexp, s: Regexp) =
        let (|Zero|One|Regexp|) r =
            if r = Regexp.Zero then Zero r
            elif r = Regexp.One then One r
            else Regexp r

        match r, s with
        // if any operand is zero, return zero
        | Zero zero, _
        | _, Zero zero -> zero
        // if any operand is one, return the other
        | One _, regex
        | regex, One _ -> regex
        // when both are lists, append them
        | Concatenation a, Concatenation b -> Concatenation <| List.append a b
        // when just one is a list, append the other in the right order
        | Concatenation seq, regexp -> Concatenation <| List.append seq [ regexp ]
        | regexp, Concatenation seq -> Concatenation <| regexp :: seq
        // otherwise, concatenate them
        | r, s -> Concatenation <| [ r; s ]

    /// Kleene star/closure operator: zero or more repetitions of a regexp.
    static member (!*)(r: Regexp) =
        // by definition, 0* = 1* = 1
        if r = Regexp.Zero then
            Regexp.One
        elif r = Regexp.One then
            Regexp.One
        // also, (r*)* = r*
        else
            match r with
            | KleeneClosure r
            | r -> KleeneClosure r

    /// Optional operator: equivalent to `1 + r`.
    static member (!?)(r: Regexp) = Regexp.One + r

    /// Positive closure operator: equivalent to `r * !*r`.
    static member (!+)(r: Regexp) = r * !*r

    /// Repetition: fixed number of repetitions of a regexp in sequence.
    // XXX: the ugly (but grep-able) name disables using the ( ** ) operator,
    // which triggers a bug in Fable: https://github.com/fable-compiler/Fable/issues/2496
    static member _Pow(r: Regexp, n: int) =
        if n <= 0 then Regexp.One
        elif n = 1 then r
        else Seq.init n (fun _ -> r) |> Seq.fold (*) Regexp.One

    /// Pretty printing in a standard-ish format.
    override this.ToString() =
        let escapingCharacters = @"\()|*.^?"

        match this with
        | Symbol c ->
            if String.exists ((=) c) escapingCharacters then
                @"\" + string c
            else
                string c
        | Alternation set ->
            if Set.isEmpty set then
                "(.^)" // regex to reject any input, including empty strings
            elif Set.contains Regexp.One set then
                set
                |> Set.remove Regexp.One
                |> Set.fold (+) Regexp.Zero
                |> sprintf "%A?"
            else
                String.concat "|" (Seq.map string set) |> sprintf "(%s)"
        | Concatenation seq ->
            if List.isEmpty seq then
                ""
            else
                String.concat "" (Seq.map string seq) |> sprintf "(%s)"
        | KleeneClosure regex -> string regex + "*"

[<RequireQualifiedAccess>] // since we use standard collection names
module Regexp =
    /// Constructs a regexp for an atomic symbol.
    let ofChar s = Symbol s

    /// Singleton regexp that rejects everything.
    let none = Regexp.Zero

    /// Also known as epsilon, the empty string.
    let empty = Regexp.One

    /// Alias of (+)
    let inline union (r: Regexp) (s: Regexp) = r + s

    /// Alias of (*)
    let inline append (r: Regexp) (s: Regexp) = r * s

    /// Alias of (!*)
    let inline star (r: Regexp) = !*r

    /// Constructs a regexp from a sequence of symbols.
    let ofSeq group =
        group |> Seq.map ofChar |> Seq.fold (*) Regexp.One

    /// Constructs a regexp from an unordered set of symbols.
    let ofSet group =
        group |> Seq.map ofChar |> Seq.fold (+) Regexp.Zero

    /// Alias of (!?)
    let inline maybe (r: Regexp) = !?r

    /// Alias of (!+)
    let inline many (r: Regexp) = !+r

    /// Alternative for (**)
    let inline init n (r: Regexp) = Regexp._Pow (r, n)


open Formally.Automata

/// Helpers specifically dealing with finite transition tables aka multigraphs.
module private Finite =
    /// Finds, in a nondeterministic transition table, the set of states
    /// recursively reachable only by epsilon transitions from an initial state
    /// while applying a state projection function at each transition output.
    let epsilonClosure getState transitionTable initial =
        let transitions arc =
            Map.tryFind arc transitionTable |> Option.defaultValue Set.empty

        // depth-first traversal in a possibly cyclic graph
        let rec epsilonReachable visited current =
            if Set.contains current visited then
                set []
            else
                let visited = Set.add current visited

                transitions (current, None)
                |> Seq.map (getState >> epsilonReachable visited)
                |> Set.unionMany
                |> Set.add current

        epsilonReachable Set.empty initial

    /// Converts from nondeterministic to deterministic transition table of sets.
    let determinize getState transitionTable =
        failwith "FIXME: implement `determinize`"

/// Deterministic Finite Automaton (DFA) for regular language recognition.
type Dfa<'State when 'State: comparison> =
    { Transitions: Map<('State * Symbol), 'State>
      Current: 'State
      Accepting: Set<'State>
      Dead: 'State }
    // set of states and input alphabet are implicitly given

    member this.States : Set<'State> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> set [ q; q' ])
        |> Set.unionMany
        |> Set.add this.Current
        |> Set.union this.Accepting
        |> Set.add this.Dead

    member this.Alphabet : Set<Symbol> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> set [ a ])
        |> Set.unionMany

    interface IAutomaton<'State, Symbol, unit> with
        override this.View = this.Current

        override this.Step input =
            let next =
                Map.tryFind (this.Current, input) this.Transitions
                |> Option.defaultValue this.Dead
            // Moore style: no output on transitions
            (), { this with Current = next } :> IAutomaton<_, _, _>

/// Nondeterministic Finite Automaton (NFA) for regular language recognition.
type Nfa<'State when 'State: comparison> =
    { Transitions: Map<('State * option<Symbol>), Set<'State>>
      Current: Set<'State>
      Accepting: Set<'State> }
    member __.Dead : Set<'State> = set []

    member this.States : Set<'State> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> Set.add q q')
        |> Set.unionMany
        |> Set.union this.Current
        |> Set.union this.Accepting

    member this.Alphabet : Set<Symbol> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> a)
        |> Seq.filter Option.isSome
        |> Seq.map Option.get
        |> Set.ofSeq

    interface IAutomaton<Set<'State>, option<Symbol>, unit> with
        override this.View = this.Current

        override this.Step input =
            let defaultOf state =
                if Option.isNone input then set [ state ] else set []

            let nextStates =
                this.Current
                |> Seq.map // for each state we're in
                    (fun state ->
                        // transition by the given input
                        Map.tryFind (state, input) this.Transitions
                        |> Option.defaultValue (defaultOf state)
                        // then unite the epsilon closures of each next state
                        |> Seq.map (Finite.epsilonClosure id this.Transitions)
                        |> Set.unionMany)
                |> Set.unionMany // then get the union of all that

            (), { this with Current = nextStates } :> IAutomaton<_, _, _>

/// Operations between NFAs and conversion to/from DFAs and Regexps.
[<RequireQualifiedAccess>]
module Nfa =
    let private map stateMapping nfa =
        { Transitions =
              Map.toSeq nfa.Transitions
              |> Seq.map (fun ((q, a), q') -> (stateMapping q, a), Set.map stateMapping q')
              |> Map.ofSeq
          Current = Set.map stateMapping nfa.Current
          Accepting = Set.map stateMapping nfa.Accepting }

    /// Discriminated union of two NFAs through epsilon transitions.
    let union a b =
        let a = map Choice1Of2 a
        let b = map Choice2Of2 b
        let newInitial = Set.union a.Current b.Current
        let newAccepting = Set.union a.Accepting b.Accepting

        { Accepting = newAccepting
          Current = newInitial
          Transitions =
              Seq.append (Map.toSeq a.Transitions) (Map.toSeq b.Transitions)
              |> Map.ofSeq }

    /// Trivial mapping of deterministic to nondeterministic automaton.
    let ofDfa (dfa: Dfa<_>) =
        { Current = set [ dfa.Current ]
          Accepting = dfa.Accepting
          Transitions =
              Map.toSeq dfa.Transitions
              |> Seq.map (fun ((q, i), q') -> (q, Some i), set [ q' ])
              |> Map.ofSeq }

    /// Converts an NFA into an equivalent DFA.
    ///
    /// This is NOT the inverse of `ofDfa`, since states get wrapped into sets.
    let toDfa nfa =
        let determinized = Finite.determinize id nfa.Transitions

        let accepting =
            Map.toSeq determinized
            |> Seq.map (fun ((q, a), q') -> set [ q; q' ])
            |> Set.unionMany
            |> Set.filter (fun s -> Set.intersect s nfa.Accepting |> (not << Set.isEmpty))

        let initial =
            nfa.Current
            |> Seq.map (Finite.epsilonClosure id nfa.Transitions)
            |> Set.unionMany

        { Transitions = determinized
          Accepting = accepting
          Dead = set []
          Current = initial }
