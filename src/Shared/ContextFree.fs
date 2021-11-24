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
    /// Finds the DIRECT derivations with a specific symbol at its head.
    let derivationsOf symbol grammar =
        Set.filter (fun (head, body) -> head = symbol) grammar.Rules

    /// Computes the FIRST-set of a given symbol sequence in a grammar.
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
                derivationsOf n grammar
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

    /// Computes the FOLLOW-set of every non-terminal symbol in the grammar.
    let followSets (grammar: Grammar<_, _>) endmarker =
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


open Formally.Automata

type Stack<'T> = 'T list // top -> [ head ... ] <- bottom

/// These define what happens to the stack on each transition.
type StackAction<'T> =
    | NoOp
    | ReplaceTop of List<'T>

module private Stack =
    let tryAction action stack =
        match action with
        | NoOp -> Some stack
        | ReplaceTop newTop ->
            match stack with
            | [] -> None // stack underflow
            | top :: stack -> List.append newTop stack |> Some

type DpdaTransition<'State, 'InputSymbol, 'StackSymbol
        when 'State: comparison and 'InputSymbol: comparison and 'StackSymbol: comparison> =
    | EpsilonTransition of 'State * StackAction<'StackSymbol>
    | InputConsumingTransitions of Map<'InputSymbol, ('State * StackAction<'StackSymbol>)>

/// This type is defined such that building a non-deterministic PDA is impossible.
type private DpdaTransitionTable<'State, 'InputSymbol, 'StackSymbol
        when 'State: comparison and 'InputSymbol: comparison and 'StackSymbol: comparison> =
    Map<('State * 'StackSymbol),
        DpdaTransition<'State, 'InputSymbol, 'StackSymbol>>

/// Deterministic Pushdown Automaton (DPDA), accepting by final state.
///
/// NOTE: unlike finite automata, PDAs can loop infinitely on finite inputs.
type Dpda<'State, 'InputSymbol, 'StackSymbol
        when 'State: comparison and 'InputSymbol: comparison and 'StackSymbol: comparison> =
    { Current: 'State * Stack<'StackSymbol>
      Accepting: Set<'State>
      Transitions: DpdaTransitionTable<'State, 'InputSymbol, 'StackSymbol>
      Dead: 'State }
    // set of states and input/stack alphabets are implicitly given

    member this.States : Set<'State> =
        Map.toSeq this.Transitions
        |> Seq.map
            (fun ((q, topOfStack), transition) ->
                match transition with
                | EpsilonTransition (q', action) -> set [ q; q' ]
                | InputConsumingTransitions options ->
                    Map.toSeq options
                    |> Seq.map (fun (input, (q', action)) -> q')
                    |> Set.ofSeq
                    |> Set.add q)
        |> Set.unionMany
        |> Set.add (this.Current |> fst)
        |> Set.union this.Accepting
        |> Set.add this.Dead

    member this.InputAlphabet : Set<'InputSymbol> =
        Map.toSeq this.Transitions
        |> Seq.map
            (fun (_, transition) ->
                match transition with
                | EpsilonTransition _ -> Set.empty
                | InputConsumingTransitions options ->
                    Map.toSeq options
                    |> Seq.map (fun (input, action) -> input)
                    |> Set.ofSeq)
        |> Set.unionMany

    member this.StackAlphabet : Set<'StackSymbol> =
        let symbolsInAction = function
            | ReplaceTop symbols -> Set.ofSeq symbols
            | NoOp -> Set.empty
        Map.toSeq this.Transitions
        |> Seq.map
            (fun ((q, topOfStack), transition) ->
                match transition with
                | EpsilonTransition (q', action) ->
                    symbolsInAction action
                    |> Set.add topOfStack
                | InputConsumingTransitions options ->
                    Map.toSeq options
                    |> Seq.map (fun (input, (q', action)) -> symbolsInAction action)
                    |> Set.unionMany
                    |> Set.add topOfStack)
        |> Set.unionMany
        |> Set.union (this.Current |> snd |> Set.ofSeq)

    interface IAutomaton<('State * Stack<'StackSymbol>), 'InputSymbol, Result<StackAction<'StackSymbol>, unit>> with
        override this.View = this.Current

        override this.Step input =
            let tryTransition stack (nextState, action) =
                match Stack.tryAction action stack with
                | None -> this.Dead, stack, Error ()
                | Some newStack -> nextState, newStack, Ok action

            let hasEpsilon state stack =
                match List.tryHead stack with
                | None -> false
                | Some topOfStack ->
                    match Map.tryFind (state, topOfStack) this.Transitions with
                    | Some (EpsilonTransition _) -> true
                    | notEpsilon -> false

            // steps based on the current combination of state, top of stack and input
            let state, stack, output =
                match this.Current with
                | state, [] -> this.Dead, [], Error () // this is PDA U.B.
                | state, (topOfStack::restOfStack as stack) ->
                    match Map.tryFind (state, topOfStack) this.Transitions with
                    | None -> this.Dead, stack, Ok NoOp
                    | Some (EpsilonTransition (nextState, action)) ->
                        tryTransition stack (nextState, action)
                    | Some (InputConsumingTransitions options) ->
                        match Map.tryFind input options with
                        | None -> this.Dead, stack, Ok NoOp
                        | Some (nextState, action) ->
                            tryTransition stack (nextState, action)

            // if there's an epsilon transition after a step, we keep going.
            // this means epsilon cycles are an easy way to cause infinite loops
            let next = { this with Current = state, stack } :> IAutomaton<_, _, _>
            match output with
            | Error () -> Error (), next
            | Ok action ->
                if hasEpsilon state stack then
                    next.Step input
                else
                    Ok action, next

/// Less parametric DPDA for the case when stack symbols <= input symbols.
type Dpda<'State, 'Symbol when 'State: comparison and 'Symbol: comparison> =
    Dpda<'State, 'Symbol, 'Symbol>
