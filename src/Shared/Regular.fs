/// Finite automata and regexps for the domain of regular languages.

namespace FormalLanguages


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type Regexp =
    private
    | Symbol of char
    | Alternation of Set<Regexp>
    | Concatenation of List<Regexp>
    | KleeneClosure of Regexp

    static member Zero = Alternation Set.empty
    static member One = Concatenation List.empty

    /// Choice operator, can accept either one of the given regexps.
    static member (+)(r: Regexp, s: Regexp) =
        // active pattern to help identify special cases
        let (|Zero|Regexp|) r =
            if r = Regexp.Zero then
                Zero r
            else
                Regexp r

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
        | r, s ->
            if r = s then
                r
            else
                Alternation <| Set([| r; s |])

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
        if n <= 0 then
            Regexp.One
        elif n = 1 then
            r
        else
            Seq.init n (fun _ -> r) |> Seq.fold (*) Regexp.One

    /// Pretty printing in a standard-ish format.
    override this.ToString() =
        let escaping = @"\()|*.^?"

        match this with
        | Symbol char ->
            if String.exists ((=) char) escaping then
                @"\" + string char
            else
                string char
        | Alternation set ->
            if Set.isEmpty set then
                "(.^)"
            elif Set.contains Regexp.One set then
                set
                |> Set.remove Regexp.One
                |> Set.fold (+) Regexp.Zero
                |> string
                |> fun r -> r + "?"
            else
                String.concat "|" (Seq.map string set)
                |> sprintf "(%s)"
        | Concatenation seq ->
            if List.isEmpty seq then
                ""
            else
                String.concat "" (Seq.map string seq)
                |> sprintf "(%s)"
        | KleeneClosure regex -> string regex + "*"

[<RequireQualifiedAccess>] // since we use standard collection names
module Regexp =
    /// Constructs a regexp for an atomic symbol.
    let ofChar = Symbol

    /// Singleton regexp that rejects everything.
    let none = Regexp.Zero

    /// Also known as epsilon, the empty string.
    let empty = Regexp.One

    /// Alias of (+)
    let union (r: Regexp) (s: Regexp) = r + s

    /// Alias of (*)
    let append (r: Regexp) (s: Regexp) = r * s

    /// Alias of (!*)
    let star (r: Regexp) = !*r

    /// Constructs a regexp from a sequence of symbols.
    let ofSeq group =
        group |> Seq.map ofChar |> Seq.fold (*) Regexp.One

    /// Constructs a regexp from an unordered set of symbols.
    let ofSet group =
        group
        |> Seq.map Symbol
        |> Seq.fold (+) Regexp.Zero

    /// Alias of (!?)
    let maybe (r: Regexp) = !?r

    /// Alias of (!+)
    let many (r: Regexp) = !+r

    /// Alternative for (**)
    let init n (r: Regexp) = Regexp._Pow (r, n)


/// Defines the execution interface of Finite-State Automata with mutable state.
type IAutomaton<'symbol, 'state> =
    interface
        /// State the machine is in.
        abstract member Current : 'state

        /// Whether or not the machine is currently in an accepting state.
        abstract member IsAccepting : bool

        /// Steps the machine forward based on the given input.
        abstract member Step : 'symbol -> unit

        /// Resets machine to its initial configuration.
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

    // steps by the given state and input, or keeps halted when in a dead state
    let step arc =
        match dead with
        | None -> Map.find arc transition // this may throw
        | Some definedDeadState ->
            Map.tryFind arc transition
            |> Option.defaultValue definedDeadState

    let mutable current = initial

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

    // the machine is accepting whenever it is in at least one accepting state
    let hasAccepting states =
        Set.intersect accepts states |> Set.isEmpty |> not

    // shallow next state, ignoring epsilon transitions
    let next arc =
        Map.tryFind arc transition |> Option.defaultValue Set.empty

    let rec epsilonReachable visited state =
        if Set.contains state visited then
            Set.singleton state
        else
            let visited = Set.add state visited
            let nextStates = next (state, None)
            nextStates
            |> Seq.map (epsilonReachable visited)
            |> Set.unionMany
            |> Set.add state

    // we find epsilon closures by depth-first traversal, then we cache them
    let mutable epsilonCache = Map.empty
    let epsilonClosure state =
        match Map.tryFind state epsilonCache with
        | Some cached -> cached
        | None ->
            let closure = epsilonReachable Set.empty state
            epsilonCache <- Map.add state closure epsilonCache
            closure

    // gets the next set of states after a non-deterministic transition
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
