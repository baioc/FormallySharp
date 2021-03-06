/// Regexp algebra and finite automata for the domain of regular languages.

namespace Formally.Regular


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
///
/// The union cases are only exposed for consuming Regexps. Construction
/// should be done with the algebraic operators or module functions.
type Regexp<'Symbol when 'Symbol: comparison> =
    | Literal of 'Symbol
    | Alternation of Set<Regexp<'Symbol>>
    | Concatenation of Regexp<'Symbol> list
    | KleeneClosure of Regexp<'Symbol>

    static member Zero = Alternation Set.empty
    static member One = Concatenation []

    /// Choice operator, can accept either one of the given regexps.
    static member (+)(r: Regexp<'Symbol>, s: Regexp<'Symbol>) =
        match r, s with
        // if any operand is zero, return the other
        | zero, regex
        | regex, zero when zero = Regexp<'Symbol>.Zero -> regex
        // when both are sets, unite them
        | Alternation r, Alternation s -> Alternation <| Set.union r s
        // when just one is a set, add the other one to the set
        | Alternation set, regexp
        | regexp, Alternation set -> Alternation <| Set.add regexp set
        // when both are not sets, either apply idempotency or make a new set
        | r, s -> if r = s then r else Alternation <| set [ r; s ]

    /// Concatenation operator, must accept the first, then the second regexp.
    static member (*)(r: Regexp<'Symbol>, s: Regexp<'Symbol>) =
        match r, s with
        // if any operand is zero, return zero
        | zero, regex
        | regex, zero when zero = Regexp<'Symbol>.Zero -> zero
        // if any operand is one, return the other
        | one, regex
        | regex, one when one = Regexp<'Symbol>.One -> regex
        // when both are lists, append them
        | Concatenation a, Concatenation b -> Concatenation <| List.append a b
        // when just one is a list, append the other in the right order
        | Concatenation seq, regexp -> Concatenation <| List.append seq [ regexp ]
        | regexp, Concatenation seq -> Concatenation <| regexp :: seq
        // otherwise, concatenate them
        | a, b -> Concatenation <| [ a; b ]

    /// Kleene star/closure operator: zero or more repetitions of a regexp.
    static member (!*)(r: Regexp<'Symbol>) =
        match r with
        // by definition, 0* = 1* = 1
        | r when r = Regexp<'Symbol>.Zero -> Regexp<'Symbol>.One
        | r when r = Regexp<'Symbol>.One -> Regexp<'Symbol>.One
        // (r*)* = r*
        | KleeneClosure r -> KleeneClosure r
        // (r?)* = r*
        | Alternation set when Set.contains Regexp<'Symbol>.One set ->
            let set = Set.remove Regexp<'Symbol>.One set
            if Set.count set = 1 then Set.minElement set |> (!*)
            else set |> Alternation |> (!*)
        // otherwise, just star it
        | r -> KleeneClosure r

    /// Optional operator: equivalent to `1 + r`.
    static member (!?)(r: Regexp<'Symbol>) = Regexp<'Symbol>.One + r

    /// Positive closure operator: equivalent to `r * !*r`.
    static member (!+)(r: Regexp<'Symbol>) = r * !*r

    /// Repetition: fixed number of repetitions of a regexp in sequence.
    static member Pow(r: Regexp<'Symbol>, n: int) =
        if n <= 0 then Regexp<'Symbol>.One
        elif n = 1 then r
        else Seq.init n (fun _ -> r) |> Seq.fold (*) Regexp<'Symbol>.One

/// Standard regexp, using characters as atomic symbols.
type Regexp = Regexp<char>

[<RequireQualifiedAccess>] // since we use standard collection names
module Regexp =
    /// Constructs a regexp from an atomic literal.
    let singleton s = Literal s

    /// Rejects everything. Equivalent to `Alternation Set.empty`.
    let none<'Symbol when 'Symbol: comparison> = Regexp<'Symbol>.Zero

    /// The empty string, a.k.a. epsilon. Equivalent to `Concatenation []`.
    let empty<'Symbol when 'Symbol: comparison> = Regexp<'Symbol>.One

    /// Alias of (+)
    let inline union (r: Regexp<'Symbol>) (s: Regexp<'Symbol>) = r + s

    /// Alias of (*)
    let inline append (r: Regexp<'Symbol>) (s: Regexp<'Symbol>) = r * s

    /// Alias of (!*)
    let inline star (r: Regexp<'Symbol>) = !*r

    /// Constructs a regexp from a sequence of symbols.
    let ofSeq group =
        group |> Seq.map singleton |> Seq.fold (*) Regexp<'Symbol>.One

    /// Constructs a regexp from an unordered set of symbols.
    let ofSet group =
        group |> Seq.map singleton |> Seq.fold (+) Regexp<'Symbol>.Zero

    /// Alias of (!?)
    let inline maybe (r: Regexp<'Symbol>) = !?r

    /// Alias of (!+)
    let inline many (r: Regexp<'Symbol>) = !+r

    /// Alternative for (**)
    let inline init n (r: Regexp<'Symbol>) = r ** n

    /// Built-in character class.
    let word, notWord, digit, notDigit, space, notSpace, dot =
        let space = set [ ' '; '\t'; '\r'; '\n'; '\v'; '\f' ]
        let all = set [ '\x20' .. '\x7E' ] + space
        let digit = set [ '0' .. '9' ]
        let letter = set [ 'A' .. 'Z' ] + set [ 'a' .. 'z' ]
        let word = letter + digit + set [ '_' ]
        let w = ofSet word
        let W = ofSet (all - word)
        let d = ofSet digit
        let D = ofSet (all - digit)
        let s = ofSet space
        let S = ofSet (all - space)
        let dot = ofSet (all - set [ '\n'; '\r' ])
        w, W, d, D, s, S, dot

    type private Token =
        | Terminator
        | LeftParenthesis
        | RightParenthesis
        | Pipe
        | Star
        | Atom of char
        | Plus
        | QuestionMark
        | Group of Regexp
        | LeftBracket
        | RightBracket
        | UnknownSequence of string

    let private escaped = set @"+*?^$\.[]{}()|/"
    let private unescaped = set [ '\x20' .. '\x7E' ] - escaped
    let private escaped_classes = set "sSdDwW"

    /// Tries to parse a regexp based on standard-ish Perl regex syntax.
    let tryParse str =
        let n = String.length str

        // prefix tree lexer
        let next pos =
            if pos >= n then
                pos, Terminator
            else
                match str.[pos] with
                | '(' -> pos + 1, LeftParenthesis
                | ')' -> pos + 1, RightParenthesis
                | '|' -> pos + 1, Pipe
                | '*' -> pos + 1, Star
                | c when Set.contains c unescaped -> pos + 1, Atom c
                | '\\' when pos + 1 < n && Set.contains str.[pos + 1] (escaped + set "tvfrn") ->
                    pos + 2, Atom str.[pos + 1]
                | '+' -> pos + 1, Plus
                | '?' -> pos + 1, QuestionMark
                | '.' -> pos + 1, Group dot
                | '\\' when pos + 1 < n && Set.contains str.[pos + 1] escaped_classes ->
                    let charClass =
                        match str.[pos + 1] with
                        | 's' -> space
                        | 'S' -> notSpace
                        | 'd' -> digit
                        | 'D' -> notDigit
                        | 'w' -> word
                        | 'W' -> notWord
                        | _ -> failwithf "unreachable"
                    pos + 2, Group charClass
                | '[' -> pos + 1, LeftBracket
                | ']' -> pos + 1, RightBracket
                | c -> n, UnknownSequence str.[pos..]

        // recursive descent parser
        let rec expression pos lookahead =
            match lookahead with
            | Atom _ | Group _ | LeftParenthesis | LeftBracket -> choice pos lookahead
            | Terminator | RightParenthesis -> pos - 1, empty
            | token -> failwithf "Syntax error: unexpected %O" token
        and choice pos lookahead =
            match lookahead with
            | Atom _ | Group _ | LeftParenthesis | LeftBracket ->
                let pos, lhs = sequence pos lookahead
                match next pos with
                | pos, Pipe ->
                    let pos, rhs = next pos ||> choice
                    pos, lhs + rhs
                | _, Terminator | _, RightParenthesis -> pos, lhs
                | _, token -> failwithf "Syntax error: unexpected %O" token
            | token -> failwithf "Syntax error: unexpected %O" token
        and sequence pos lookahead =
            match lookahead with
            | Atom _ | Group _ | LeftParenthesis | LeftBracket ->
                let pos, lhs = term pos lookahead
                match next pos with
                | _, Atom _ | _, Group _ | _, LeftParenthesis | _, LeftBracket as next ->
                    let pos, lookahead = next
                    let pos, rhs = sequence pos lookahead
                    pos, lhs * rhs
                | _, Terminator | _, Pipe | _, RightParenthesis -> pos, lhs
                | _, token -> failwithf "Syntax error: unexpected %O" token
            | token -> failwithf "Syntax error: unexpected %O" token
        and term pos lookahead =
            match lookahead with
            | Atom _ | Group _ | LeftParenthesis | LeftBracket ->
                let pos, expr = factor pos lookahead
                match next pos with
                | pos, Star -> pos, !* expr
                | pos, Plus -> pos, !+ expr
                | pos, QuestionMark -> pos, !? expr
                | _, Terminator | _, Atom _ | _, Group _ | _, LeftParenthesis | _, LeftBracket | _, Pipe | _, RightParenthesis -> pos, expr
                | _, token -> failwithf "Syntax error: unexpected %O" token
            | token -> failwithf "Syntax error: unexpected %O" token
        and factor pos lookahead =
            match lookahead with
            | Atom atom -> pos, Literal atom
            | Group group -> pos, group
            | LeftParenthesis ->
                let pos, expr = next pos ||> expression
                match next pos with
                | pos, RightParenthesis -> pos, expr
                | _, notRParen -> failwithf "Syntax error at %d: expected %O, found %O" pos RightParenthesis notRParen
            | LeftBracket ->
                let pos, expr = next pos ||> charset
                match next pos with
                | pos, RightBracket -> pos, expr
                | _, notRBrack -> failwithf "Syntax error at %d: expected %O, found %O" pos RightBracket notRBrack
            | token -> failwithf "Syntax error: unexpected %O" token
        and charset pos lookahead =
            match lookahead with
            | RightBracket -> pos - 1, none
            | Group group ->
                let pos, rest = next pos ||> charset
                pos, group + rest
            | Atom a ->
                let pos, lookahead, set =
                    match next pos with
                    | pos, Atom b when b = '-' ->
                        match next pos with
                        | pos, (Atom b as lookahead) -> pos, lookahead, ofSet [ a .. b ]
                        | _, notAtom -> failwithf "Syntax error at %d: expected an atom, found %O" pos notAtom
                    | pos, lookahead ->
                        pos, lookahead, singleton a
                let pos, rest = charset pos lookahead
                pos, set + rest
            | token -> failwithf "Syntax error: unexpected %O" token

        // starts the parser and checks for unconsumed input
        try
            if str = "" then
                Some empty
            else
                let pos, expr = next 0 ||> expression
                match snd <| next pos with
                | Terminator -> Some expr
                | token -> failwithf "Syntax error: unexpected %O" token
        with
        | ex -> None


open Formally.Automata

// Helpers specifically dealing with finite transition tables.
module Finite =
    /// Finds, in a nondeterministic transition table, the set of states
    /// recursively reachable only by epsilon transitions from an initial state
    /// while applying a state projection function at each transition output.
    let closure epsilon getState transitionTable initial =
        let transitions arc =
            Map.tryFind arc transitionTable |> Option.defaultValue Set.empty

        // depth-first traversal in a possibly cyclic graph
        let rec epsilonReachable visited current =
            if Set.contains current visited then
                Set.empty
            else
                let visited = Set.add current visited
                transitions (current, epsilon)
                |> Seq.map (getState >> epsilonReachable visited)
                |> Set.unionMany
                |> Set.add current

        epsilonReachable Set.empty initial

    let internal epsilonClosure table state = closure None id table state

/// Deterministic Finite Automaton (DFA) for regular language recognition.
type Dfa<'State, 'Symbol when 'State: comparison and 'Symbol: comparison> =
    { Transitions: Map<('State * 'Symbol), 'State>
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

    member this.Alphabet : Set<'Symbol> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> a)
        |> Set.ofSeq

    interface IAutomaton<'State, 'Symbol, unit> with
        override this.View = this.Current
        override this.Step input =
            let next =
                Map.tryFind (this.Current, input) this.Transitions
                |> Option.defaultValue this.Dead
            // Moore style: no output on transitions
            (), { this with Current = next } :> IAutomaton<_, _, _>

/// Nondeterministic Finite Automaton (NFA) for regular language recognition.
type Nfa<'State, 'Symbol when 'State: comparison and 'Symbol: comparison> =
    { Transitions: Map<('State * option<'Symbol>), Set<'State>>
      Current: Set<'State>
      Accepting: Set<'State> }
    member __.Dead : Set<'State> = Set.empty

    member this.States : Set<'State> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> Set.add q q')
        |> Set.unionMany
        |> Set.union this.Current
        |> Set.union this.Accepting

    member this.Alphabet : Set<'Symbol> =
        Map.toSeq this.Transitions
        |> Seq.map (fun ((q, a), q') -> a)
        |> Seq.choose id
        |> Set.ofSeq

    interface IAutomaton<Set<'State>, option<'Symbol>, unit> with
        override this.View = this.Current

        override this.Step input =
            let defaultOf state =
                if Option.isNone input then Set.singleton state else Set.empty

            let nextStates =
                this.Current
                |> Seq.map // for each state we're in
                    (fun state ->
                        // transition by the given input
                        Map.tryFind (state, input) this.Transitions
                        |> Option.defaultValue (defaultOf state)
                        // then unite the epsilon closures of each next state
                        |> Seq.map (Finite.epsilonClosure this.Transitions)
                        |> Set.unionMany)
                |> Set.unionMany // then get the union of all that

            (), { this with Current = nextStates } :> IAutomaton<_, _, _>

// slightly less parametric types for the usual case of char symbols
type Dfa<'State when 'State: comparison> = Dfa<'State, char>
type Nfa<'State when 'State: comparison> = Nfa<'State, char>

[<RequireQualifiedAccess>]
module Nfa =
    /// Transforms an NFA by applying a function over all of its states.
    let map stateMapping nfa =
        { Transitions =
              Map.toSeq nfa.Transitions
              |> Seq.map (fun ((q, a), q') -> (stateMapping q, a), Set.map stateMapping q')
              |> Map.ofSeq
          Current = Set.map stateMapping nfa.Current
          Accepting = Set.map stateMapping nfa.Accepting }

    /// Transforms an NFA by filtering its transitions.
    let filter transitionFilter (nfa: Nfa<_, _>) =
        { nfa with
              Transitions = Map.filter transitionFilter nfa.Transitions }

    /// Discriminated union of two NFAs through epsilon transitions.
    let union a b =
        // we need to discriminate states from each NFA in order to maintain their structures,
        // which would otherwise be lost when building the transition table below
        let a = map Choice1Of2 a
        let b = map Choice2Of2 b
        { Accepting = Set.union a.Accepting b.Accepting
          Current = Set.union a.Current b.Current
          Transitions =
              Seq.append (Map.toSeq a.Transitions) (Map.toSeq b.Transitions)
              |> Map.ofSeq }

    /// Trivial mapping of deterministic to nondeterministic automaton.
    let ofDfa (dfa: Dfa<_, _>) =
        { Current = set [ dfa.Current ]
          Accepting = dfa.Accepting
          Transitions =
              Map.toSeq dfa.Transitions
              |> Seq.map (fun ((q, i), q') -> (q, Some i), set [ q' ])
              |> Map.ofSeq }

    /// Converts an NFA into an equivalent DFA with no unreachable states.
    ///
    /// This is NOT the inverse of `ofDfa`, since states get wrapped into sets.
    let toDfa (nfa: Nfa<_, _>) =
        // compute and store the epsilon closure of each atomic state
        let epsilonClosures =
            nfa.States
            |> Seq.map (fun state -> state, Finite.epsilonClosure nfa.Transitions state)
            |> Map.ofSeq

        // define an epsilon-free transition from a composed state
        let step states symbol =
            states
            |> Set.map
                (fun atomicState ->
                    Map.tryFind (atomicState, Some symbol) nfa.Transitions
                    |> Option.defaultValue Set.empty
                    |> Seq.map (fun nextState -> Map.find nextState epsilonClosures)
                    |> Set.unionMany)
            |> Set.unionMany

        let alphabet = nfa.Alphabet
        // recursively transition by each symbol until we get to every reachable state
        let rec buildTable transitionTable visitedStates missingStates =
            match missingStates with
            | [] -> transitionTable
            | currentState :: missingStates ->
                // find all non-empty transitions from the current state
                let newTransitions =
                    alphabet
                    |> Seq.map (fun symbol -> symbol, step currentState symbol)
                    |> Seq.filter (fun (symbol, reached) -> reached <> Set.empty)
                    |> Set.ofSeq
                // add those transitions to the table
                let transitionTable =
                    newTransitions
                    |> Set.fold
                        (fun table (symbol, nextState) ->
                            Map.add (currentState, symbol) nextState table)
                        transitionTable
                // add the newly reached states to:
                let nextStates =
                    newTransitions
                    |> Seq.choose
                        (fun (symbol, state) ->
                            if Set.contains state visitedStates then None
                            else Some state)
                    |> Set.ofSeq
                // a) the visited set
                let visitedStates = Set.union visitedStates nextStates
                // b) the to-do list
                let missingStates =
                    nextStates
                    |> Set.fold (fun missing state -> state :: missing) missingStates
                // then, keep going
                buildTable transitionTable visitedStates missingStates

        let determinized =
            buildTable Map.empty (Set.singleton nfa.Current) [ nfa.Current ]

        // accepting states are any that intersect with the initial accepting set
        let accepting =
            Map.toSeq determinized
            |> Seq.map (fun ((q, a), q') -> set [ q; q' ])
            |> Set.unionMany
            |> Set.filter (fun state -> Set.intersect state nfa.Accepting <> Set.empty)

        { Dead = Set.empty
          Current = nfa.Current
          Transitions = determinized
          Accepting = accepting }

[<RequireQualifiedAccess>]
module Dfa =
    /// Transforms a DFA by applying a function over all of its states.
    let map stateMapping dfa =
        { Dead = stateMapping dfa.Dead
          Transitions =
              Map.toSeq dfa.Transitions
              |> Seq.map (fun ((q, a), q') -> (stateMapping q, a), stateMapping q')
              |> Map.ofSeq
          Current = stateMapping dfa.Current
          Accepting = Set.map stateMapping dfa.Accepting }

    /// Transforms a DFA by filtering its transitions.
    let filter transitionFilter (dfa: Dfa<_, _>) =
        { dfa with
              Transitions = Map.filter transitionFilter dfa.Transitions }

    let toNfa = Nfa.ofDfa
    let ofNfa = Nfa.toDfa

    /// Converts a Regexp directly to a DFA through Aho's algorithm.
    let ofRegexp terminator regexp =
        let mutable followposTable = System.Collections.Generic.Dictionary()
        let mutable correspondenceTable = System.Collections.Generic.Dictionary()
        let mutable inputSymbols = Set.empty
        let mutable index = 0
        // assigns a unique index to every leaf node while initializing the tables above
        let rec doIndexing =
            function
            | Literal symbol ->
                do
                    index <- index + 1
                    followposTable.[index] <- Set.empty
                    correspondenceTable.[index] <- symbol
                    if symbol <> terminator then
                        inputSymbols <- Set.add symbol inputSymbols
                Literal(symbol, index)
            | Alternation set -> Alternation <| Set.map doIndexing set
            | Concatenation seq -> Concatenation <| List.map doIndexing seq
            | KleeneClosure r -> KleeneClosure <| doIndexing r

        // also concatenate a unique terminator to our regexp
        let regexp =
            doIndexing <| (regexp * (Regexp.singleton terminator))

        let rec nullable =
            function
            | Literal c -> false
            | Alternation set -> Set.exists nullable set
            | Concatenation [] -> true
            | Concatenation (first :: rest) -> nullable first && nullable (Concatenation rest)
            | KleeneClosure r -> true

        let rec firstpos =
            function
            | Literal (c, index) -> Set.singleton index
            | Alternation set -> Seq.map firstpos set |> Set.unionMany
            | Concatenation [] -> Set.empty
            | Concatenation (first :: rest) ->
                let second = Concatenation rest
                if nullable first then
                    Set.union (firstpos first) (firstpos second)
                else
                    firstpos first
            | KleeneClosure r -> firstpos r

        let rec lastpos =
            function
            | Literal (c, index) -> Set.singleton index
            | Alternation set -> Seq.map lastpos set |> Set.unionMany
            | Concatenation [] -> Set.empty
            | Concatenation (first :: rest) ->
                let second = Concatenation rest
                if nullable second then
                    Set.union (lastpos first) (lastpos second)
                else
                    lastpos second
            | KleeneClosure r -> lastpos r

        // incrementally fills the followpos table
        let rec fillFollowpos =
            function
            | KleeneClosure node ->
                let firsts = firstpos node
                for i in lastpos node do
                    followposTable.[i] <- Set.union followposTable.[i] firsts
                do fillFollowpos node
            | Concatenation (left :: rest) ->
                let right = Concatenation rest
                let firsts = firstpos right
                for i in lastpos left do
                    followposTable.[i] <- Set.union followposTable.[i] firsts
                do
                    fillFollowpos left
                    fillFollowpos right
            | Alternation set -> Set.iter fillFollowpos set
            | Concatenation []
            | Literal _ -> ()

        do fillFollowpos regexp
        // after filling the followpos table, build the transition table
        let mutable transitionTable = Map.empty
        let initialState = firstpos regexp
        let mutable visitedStates = Set.empty
        let mutable unmarkedStates = set [ initialState ]

        while not (Set.isEmpty unmarkedStates) do
            let state = Set.minElement unmarkedStates
            unmarkedStates <- Set.remove state unmarkedStates
            visitedStates <- Set.add state visitedStates
            for symbol in inputSymbols do
                let next =
                    state
                    |> Seq.choose
                        (fun i ->
                            if correspondenceTable.[i] <> symbol then None
                            else Some followposTable.[i])
                    |> Set.unionMany
                transitionTable <- Map.add (state, symbol) next transitionTable
                if not (Set.contains next visitedStates) then
                    unmarkedStates <- Set.add next unmarkedStates

        // accepting states are all which contain the terminator's index
        let accepting =
            visitedStates
            |> Set.filter
                (Set.exists (fun i -> correspondenceTable.[i] = terminator))

        { Dead = set []
          Transitions = transitionTable
          Current = initialState
          Accepting = accepting }
