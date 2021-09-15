/// Grammar manipulation and pushdown automata for context-free languages.

namespace Formally.ContextFree


/// Disjoint union of grammar terminal and non-terminal symbols: `T + N`.
type Symbol<'T, 'N> =
    | Terminal of 'T
    | NonTerminal of 'N

/// Represents the (possibly empty) body of a single production: `(T + N)*`.
type ProductionBody<'T, 'N> = Symbol<'T, 'N> list

/// Models context-free production rules like `N -> (T + N)*`.
type ContextFreeProduction<'T, 'N> = 'N * ProductionBody<'T, 'N>

/// Fully determines a Context-Free Grammar (CFG).
type Grammar<'Terminal, 'NonTerminal when 'Terminal: comparison and 'NonTerminal: comparison> =
    { Initial: 'NonTerminal
      Rules: Set<ContextFreeProduction<'Terminal, 'NonTerminal>> }

    member this.Terminals: Set<'Terminal> =
        let terminals body =
            body
            |> Seq.choose
                (function Terminal t -> Some t | NonTerminal n -> None)
            |> Set.ofSeq
        this.Rules
        |> Seq.map (fun (head, body) -> terminals body)
        |> Set.unionMany

    member this.NonTerminals: Set<'NonTerminal> =
        let nonTerminals body =
            body
            |> Seq.choose
                (function Terminal t -> None | NonTerminal n -> Some n)
            |> Set.ofSeq
        this.Rules
        |> Seq.map (fun (head, body) -> nonTerminals body |> Set.add head)
        |> Set.unionMany
        |> Set.add this.Initial

[<RequireQualifiedAccess>]
module Grammar =
    /// Finds the FIRST set of a given symbol sequence in a grammar.
    let rec first (symbols: Symbol<'T, 'N> list) (grammar: Grammar<'T, 'N>) : Set<'T option> =
        match symbols with
        // if we get an empty body, it means we are producing epsilon directly
        // or every symbol in the sequence was nullable, so return { epsilon }
        | [] -> Set.singleton None
        // if the first symbol is a terminal T, then FIRST(T) is { T }
        | Terminal t :: rest -> Set.singleton (Some t)
        // if the symbol is a non-terminal, unite the FIRST of its productions
        // (but with an altered grammar to avoid going infinite on cycles)
        | NonTerminal n :: rest ->
            let firstSet =
                grammar.Rules
                |> Seq.filter (fun (head, body) -> head = n)
                |> Seq.map
                    (fun (head, body) ->
                        let grammar =
                            { grammar with
                                  Rules = Set.remove (head, body) grammar.Rules }
                        first body grammar)
                |> Set.unionMany
            // if the resulting set is NOT nullable, we can stop here
            if not (Set.contains None firstSet) then
                firstSet
            // otherwise, iterate down the rest of the body
            else
                Set.union
                    (Set.remove None firstSet)
                    (first rest grammar)

    /// Finds the FOLLOW set of a non-terminal symbol in a given grammar.
    let rec follow (symbol: 'N) (grammar: Grammar<'T, 'N>) (terminator: 'T) : Set<'T> =
        let mutable followSet = Set.empty
        let initialHead, initialBody = Set.toList(grammar.Rules).[0]
        if (symbol = initialHead) then
            followSet <- followSet.Add(Terminal '$')
        else
            for head, body in grammar.Rules do
                for i=0 to body.Length do
                    match body.[i] with
                    | Terminal t -> ()
                    | NonTerminal nt->
                        if (nt = symbol) then
                            if (body.Length <> 0) then
                                let firstWithoutEpsilon =
                                    first (List.singleton body.[i+1]) grammar
                                    |> Seq.choose Terminal
                                    |> Set.ofSeq
                                followSet <- followSet + (firstWithoutEpsilon)
                            else
                                let firstSetEpsilon =
                                    first (List.singleton nt) grammar
                                    |> Seq.filter (fun (firstItem) -> firstItem = None)
                                    |> Set.ofSeq
                                if (firstSetEpsilon.Length = 0) then
                                    followSet <- followSet + (follow head grammar) 
        followSet


    let eliminateLeftRecursions (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: eliminateLeftRecursions"

    let leftFactor (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: leftFactor"


// TODO: deterministic pushdown automaton (DPDA)


// TODO: generate LL(1) parser
