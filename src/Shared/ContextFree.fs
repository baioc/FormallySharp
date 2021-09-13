/// Grammar manipulation and pushdown automata for context-free languages.

namespace Formally.ContextFree


/// Disjoint union of grammar terminal and non-terminal symbols: `T + N`.
type Symbol<'T, 'N> =
    | Terminal of Option<'T>
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
// não conseguimos pensar em como fazer isso na sintaxe dele, mas a gnt tentou deixar a ideia pra ver ctg Baiocchi
    let rec first (symbol: Symbol<'T, 'N>) (grammar: Grammar<'T, 'N>) : Set<Option<'T>> =
        let mutable nonTerminalRules = Set.empty
        let mutable output = Set.empty
        match symbol with 
        | Terminal t -> Set.singleton t
        | NonTerminal n -> 
            nonTerminalRules <- Set.filter (fun (head, body) -> head = n) grammar.Rules
            for rule in nonTerminalRules do
                if (rule.body[0] = Terminal) then
                    output <- output.Add rule.body[0]
                elif (rule.body[0] = None) then
                    output <- output.Add None
                else
                    output <- output + (first rule.body[0] grammar)
            output

        

    let follow (symbol: Symbol<'T, 'N>) (grammar: Grammar<'T, 'N>): Set<Option<'T>> =
        failwith "TODO: follow"

    let eliminateLeftRecursions (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: eliminateLeftRecursions"

    let leftFactor (grammar: Grammar<'T, 'N>) : Grammar<'T, 'N> =
        failwith "TODO: leftFactor"


// TODO: deterministic pushdown automaton (DPDA)


// TODO: generate LL(1) parser
