module Finite


/// Defines the canonic Kleene algebra, plus some regular expression extensions.
type Regex<'symbol when 'symbol: comparison> =
    // primitives, used as atomic literals
    | None // Rejects all strings.
    | Empty // The empty string.
    | Symbol of 'symbol // A specific atomic symbol.
    // combinations, should only be constructed through custom operators
    | Alternation of Set<Regex<'symbol>>
    | Concatenation of List<Regex<'symbol>>
    | KleeneClosure of Regex<'symbol>

    /// Choice: associative, neutral on `None`, commutative and idempotent.
    static member (+)(r: Regex<'symbol>, s: Regex<'symbol>) =
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
    static member (*)(r: Regex<'symbol>, s: Regex<'symbol>) =
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
    static member (!*)(r: Regex<'symbol>) =
        match r with
        // by definition, 0* = 1* = 1, where 0=None and 1=Empty
        | None
        | Empty -> Empty
        // it is also idempotent: (r*)* = r*
        | KleeneClosure regex
        | regex -> KleeneClosure regex

    /// Optional: equivalent to `Empty + r`.
    static member (!?)(r: Regex<'symbol>) = Empty + r

    /// Positive closure: equivalent to `r * (!*r)`.
    static member (!+)(r: Regex<'symbol>) = r * (!*r)

    /// Repetition: fixed number of repetitions of a regex in sequence.
    static member Pow(r: Regex<'symbol>, n: int) =
        List.init n (fun _ -> r) |> Concatenation

module Regex =
    let union (r: Regex<'s>) (s: Regex<'s>) = r + s
    let append (r: Regex<'s>) (s: Regex<'s>) = r * s
    let star (r: Regex<'s>) = !*r

    let maybe (r: Regex<'s>) = !?r
    let many (r: Regex<'s>) = !+r
    let init (n: int) (r: Regex<'s>) = r ** n

    let ofSeq seq =
        Seq.map Symbol seq |> Seq.toList |> Concatenation

    let ofSet set =
        Seq.map Symbol set |> Set.ofSeq |> Alternation
