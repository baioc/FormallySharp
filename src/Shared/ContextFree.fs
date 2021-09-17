/// Grammar manipulation and pushdown automata for context-free languages.

namespace Formally.ContextFree


/// Disjoint union of grammar terminal and non-terminal symbols: `T + N`.
type Symbol<'T, 'N> =
    | Terminal of 'T
    | NonTerminal of 'N

/// Represents the (possibly empty) body of a single production: `(T + N)*`.
///
/// Epsilon productions are represented as the empty list.
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
    /// Finds the subset of derivations with a specific symbol at its head.
    let derivationsFrom symbol grammar =
        Set.filter (fun (head, body) -> head = symbol) grammar.Rules

    /// Computes the FIRST set of a given symbol sequence in a grammar.
    ///
    /// Epsilon is a terminal symbol represented by `None`.
    let rec first symbols grammar =
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
                grammar
                |> derivationsFrom n
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

    /// Computes the FOLLOW set of every non-terminal symbol in the grammar.
    let followSets endmarker (grammar: Grammar<_, _>) =
        // initially, FOLLOW(<startSymbol>) = { endmarker }
        let mutable follows = System.Collections.Generic.Dictionary()
        for symbol in grammar.NonTerminals do
            if symbol = grammar.Initial then
                follows.[symbol] <- Set.singleton endmarker
            else
                follows.[symbol] <- Set.empty

        // for every non-terminal <A> followed by some non-empty sequence [B]
        // in a production body, add FIRST(B)/{epsilon} to FOLLOW(A)
        let rec doFollowsInBody body =
            match body with
            | [] -> ()
            | Terminal _ :: rest -> doFollowsInBody rest
            | NonTerminal a :: b ->
                let firstB = first b grammar |> Seq.choose id |> Set.ofSeq
                do follows.[a] <- Set.union follows.[a] firstB
                doFollowsInBody b

        for _, body in grammar.Rules do
            doFollowsInBody body

        // for every rule headed by <A> and tailed by a non-terminal <B>
        // (that is, <A> ::= ... <B> [X] and epsilon in FIRST[X]), we want to
        // add everything in FOLLOW(A) to FOLLOW(B). repeat until convergence
        let rec nonTerminalTails body tails =
            match body with
            | [] -> tails
            | Terminal _ :: rest -> nonTerminalTails rest tails
            | NonTerminal b :: x ->
                let firstX = first x grammar
                if Set.contains None firstX then
                    nonTerminalTails x (Set.add b tails)
                else
                    nonTerminalTails x tails

        let mutable converged = false
        while not converged do
            converged <- true
            for a, production in grammar.Rules do
                for b in nonTerminalTails production Set.empty do
                    let additions = Set.difference follows.[a] follows.[b]
                    if not (Set.isEmpty additions) then
                        follows.[b] <- Set.union follows.[b] additions
                        converged <- false

        // convert mutable to immutable mapping
        follows
        |> Seq.map (fun entry -> entry.Key, entry.Value)
        |> Map.ofSeq

    let eliminateLeftRecursions (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: Grammar.eliminateLeftRecursions"

    let leftFactor (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: Grammar.leftFactor"


// TODO: deterministic pushdown automaton (DPDA)


// TODO: generate LL(1) parser
