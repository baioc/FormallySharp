/// Grammar manipulation and pushdown automata for context free languages.

namespace Formally.ContextFree


type GrammarSymbol<'Symbol> =
    | Terminal of 'Symbol
    | NonTerminal of 'Symbol

/// Represents the (possibly empty) body of a production rule.
type ContextFreeProduction<'S when 'S: comparison> = Set<GrammarSymbol<'S>>

[<AutoOpen>]
module ContextFreeProduction =
    let inline (|Symbols|Epsilon|) (production: ContextFreeProduction<_>) =
        if production = Set.empty then
            Epsilon
        else
            Symbols production

/// Defines a Context Free Grammar (CFG).
type Grammar<'Symbol when 'Symbol: comparison> =
    { Rules: Map<'Symbol, ContextFreeProduction<'Symbol>>
      Initial: 'Symbol }
