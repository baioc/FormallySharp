/// Finite automata and regexp algebra for the domain of regular languages.

namespace Formal.Languages


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type Regexp =
    private
    | Symbol of char
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
    /// XXX: the ugly (but grep-able) name disables using the ( ** ) operator,
    /// which triggers a bug in Fable: https://github.com/fable-compiler/Fable/issues/2496
    static member _Pow(r: Regexp, n: int) =
        if n <= 0 then Regexp.One
        elif n = 1 then r
        else Seq.init n (fun _ -> r) |> Seq.fold (*) Regexp.One

    /// Pretty printing in a standard-ish format.
    override this.ToString() =
        let escapingCharacters = @"\()|*.^?"

        match this with
        | Symbol char ->
            if String.exists ((=) char) escapingCharacters then
                @"\" + string char
            else
                string char
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


open Formal.Automata

// type aliases to make the following definitions more readable
type private State = string
type private Symbol = char

/// Deterministic Finite Automaton (DFA) for regular language recognition.
type Dfa =
    { Transitions: Map<State * Symbol, State>
      Current: State
      Accepting: Set<State>
      Dead: State }

    member this.States : Set<State> =
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

    // Moore style: no output on transitions
    interface IAutomaton<State, Symbol, unit> with
        override this.View = this.Current

        override this.Step input =
            let next =
                Map.tryFind (this.Current, input) this.Transitions
                |> Option.defaultValue this.Dead

            ({ this with Current = next } :> IAutomaton<_, _, _>), () // upcast

/// Nondeterministic Finite Automaton (NFA) for regular language recognition.
type Nfa =
    { Transitions: Map<State * option<Symbol>, Set<State>>
      Current: Set<State>
      Accepting: Set<State> }
    member _.Dead : Set<State> = set []

    member this.States : Set<State> =
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
        |> set

    interface IAutomaton<Set<State>, option<Symbol>, unit> with
        override this.View = this.Current

        override this.Step input =
            let nextStates =
                this.Current
                |> Seq.map // for each state we're in
                    (fun state ->
                        // transition by the given input
                        Map.tryFind (state, input) this.Transitions
                        |> Option.defaultValue Set.empty
                        // then unite the epsilon closures of each next state
                        |> Seq.map (Automaton.epsilonClosure this.Transitions)
                        |> Set.unionMany)
                |> Set.unionMany // then get the union of all that

            ({ this with Current = nextStates } :> IAutomaton<_, _, _>), ()
