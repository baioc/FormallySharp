/// Grammar manipulation and pushdown automata for context-free languages.

namespace Formally.ContextFree


/// Disjoint union of grammar terminal and non-terminal symbols: `T + N`.
type Symbol<'T, 'N> =
    | Terminal of 'T
    | NonTerminal of 'N

/// Represents the (possibly empty) body of a production rule: `(T + N)*`.
type ProductionBody<'T, 'N> = Symbol<'T, 'N> list

/// Models context-free production rules like `N -> (T + N)*`.
type ContextFreeProduction<'T, 'N> = 'N * ProductionBody<'T, 'N>

/// Defines a Context-Free Grammar (CFG).
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
    let first (symbol: Symbol<'T, 'N>) (grammar: Grammar<'T, 'N>) : Set<Symbol<'T, 'N>> =
        failwith "TODO: first"

    let follow (symbol: Symbol<'T, 'N>) (grammar: Grammar<'T, 'N>): Set<Symbol<'T, 'N>> =
        failwith "TODO: follow"

    let eliminateLeftRecursions (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: eliminateLeftRecursions"

    let leftFactor (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: leftFactor"


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
    | InputConsumingTransition of Map<'InputSymbol, ('State * StackAction<'StackSymbol>)>

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
                | InputConsumingTransition options ->
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
                | InputConsumingTransition options ->
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
                | InputConsumingTransition options ->
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
                    | Some (InputConsumingTransition options) ->
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

/// Less parametric DPDA for the common case when input symbols = stack symbols.
type Dpda<'State, 'Symbol when 'State: comparison and 'Symbol: comparison> =
    Dpda<'State, 'Symbol, 'Symbol>
