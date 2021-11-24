namespace Shared

open System.Collections.Generic
open System.Text.RegularExpressions
open System.Runtime.CompilerServices

open Formally.Automata
open Formally.Regular
open Formally.ContextFree


/// Represents a valid string to be used as an identifier for syntax rules.
///
/// NOTE: this is a simple type alias, so nothing is enforced in construction.
type Identifier = string

module Identifier =
    /// Regular expression that matches a valid identifier.
    let regex = @"[A-Za-z_]\w*"

    /// Checks whether the given string is a valid identifier.
    let isValid str = Regex.IsMatch(str, $"^{regex}$")

module String =
    /// Returns an escaped version of given string for user visibility.
    let visual (str: string) =
        str.Replace("\t", @"\t")
           .Replace("\v", @"\v")
           .Replace("\f", @"\f")
           .Replace("\r", @"\r")
           .Replace("\n", @"\n")
           .Replace(" ",  "\u00B7") // <- unicode for visual space


/// Regexp with a user-provided string representation.
[<AutoOpen>] // so that we may use unqualified constructors
type UserRegexp =
    { Regexp: Regexp
      String: string }

    static member UserRegexp(string, regexp) =
        { String = string; Regexp = regexp }

type RegularDefinition =
    | TokenClass of UserRegexp * priority:int
    | Fragment of UserRegexp
    | Separator of UserRegexp

type LexicalSpecification = Map<Identifier, RegularDefinition>

type LexerState =
    | AcceptToken of name:Identifier * discriminant:Set<int> * priority:int
    | AcceptSeparator of name:Identifier * discriminant:Set<int>
    | Intermediary of name:Identifier * discriminant:Set<int>

/// Current lexer state over some input.
type Lexer =
    { Automaton: Dfa<Set<LexerState>>
      Initial: Set<LexerState>
      String: string
      Start: uint
      Position: uint }

type TokenInstance =
    { Token: Identifier
      Lexeme: string
      Position: uint }

/// Indicates a lexical error and keeps track of non-lexed input.
type LexicalError =
    { Irritant: char seq
      Position: uint }

/// Functions for creating and manipulating lexers.
module Lexer =
    /// Builds a new Lexer according to the given lexical specification.
    let make spec =
        let dead = ("", Set.empty)
        let makeAutomaton name regexp =
            regexp
            |> Dfa.ofRegexp '\u0000'
            |> Dfa.map
                (fun state -> // dead states can be shared
                    if state = Set.empty then dead
                    else (name, state))

        // make an automaton for each token and separator
        let automatons =
            Map.toSeq spec
            |> Seq.choose
                (fun (name, def) ->
                    match def with
                    | TokenClass (r, _) | Separator r ->
                        Some (name, makeAutomaton name r.Regexp)
                    | Fragment _ -> None)
            |> Map.ofSeq

        let undiscriminatedUnion union dfa =
            Dfa.toNfa dfa
            |> Nfa.union union
            |> Nfa.map // maintains structure due to uniqueness of names
                (function Choice1Of2 s | Choice2Of2 s -> s)

        let initial =
            { Transitions = Map.empty; Accepting = set []; Current = set [] }

        // compute the determinized union of all automatons
        let union =
            automatons
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.fold undiscriminatedUnion initial
            |> Nfa.toDfa

        // filter out dead transitions and identify accepting states + provenance
        let automaton =
            union
            |> Dfa.filter (fun (q, a) q' -> q' <> set [ dead ])
            |> Dfa.map
                (Set.map
                    (fun ((prefix, discriminant) as state) ->
                        match Map.tryFind prefix automatons with
                        | Some automaton when Set.contains state automaton.Accepting ->
                            match Map.find prefix spec with
                            | TokenClass (r, priority) ->
                                AcceptToken (prefix, discriminant, priority)
                            | Separator r ->
                                AcceptSeparator (prefix, discriminant)
                            | Fragment r ->
                                Intermediary (prefix, discriminant)
                        | notAccepting -> Intermediary state))

        { Automaton = automaton; Initial = automaton.Current
          String = ""; Start = 0u; Position = 0u }

    /// Makes a token iff the lexer is currently accepting a non-empty lexeme.
    let private tryMakeToken lexer =
        if not (Set.contains lexer.Automaton.Current lexer.Automaton.Accepting) then
            None
        elif lexer.String = "" then
            None // a lexer must never produce a token from an empty lexeme
        else
            // choose the state with the highest priority
            let state =
                Set.toSeq lexer.Automaton.Current
                |> Seq.map
                    (fun state ->
                        match state with
                        | Intermediary _ -> System.Int64.MinValue, state
                        | AcceptSeparator _ -> System.Int64.MinValue + 1L, state
                        | AcceptToken (_, _, priority) -> int64 priority, state)
                |> Seq.maxBy fst
                |> snd
            // ensure we're actually accepting a token
            match state with
            | AcceptToken (token, state, priority) ->
                Some { Token = token
                       Lexeme = lexer.String
                       Position = lexer.Start }
            | notToken -> None

    /// Checks whether the lexer is in an accepting state.
    let private isAccepting lexer =
        Set.contains lexer.Automaton.Current lexer.Automaton.Accepting

    /// Lazily generate tokens by an input sequence up until the first error.
    let rec tokenize lexer inputs = seq {
        // when the input is over, check what state we ended up in
        if Seq.isEmpty inputs then
            if isAccepting lexer then
                match tryMakeToken lexer with
                | Some lastToken -> yield Ok lastToken
                | None -> ()
            else
                let error = { Position = lexer.Start; Irritant = lexer.String }
                yield Error error // aka "unexpected end of file ..."

        // otherwise, apply transition logic and iterate down the input stream
        else
            let wasAccepting = isAccepting lexer
            let input = Seq.head inputs
            let nextState = Automaton.step input lexer.Automaton |> snd |> Automaton.view
            let justDied = nextState = lexer.Automaton.Dead

            if justDied && wasAccepting && lexer.String <> "" then
                // if accepting a token, emit it
                match tryMakeToken lexer with
                | Some token -> yield Ok token
                | None -> ()

                // since the condition above avoids empty strings, we can reset the
                // DFA and recurse with the same inputs without going infinite
                let lexer =
                    { lexer with
                          Automaton = { lexer.Automaton with Current = lexer.Initial }
                          String = ""
                          Start = lexer.Position // change where the next lexeme begins
                          Position = lexer.Position }
                yield! tokenize lexer inputs

            elif justDied && (not wasAccepting) then
                // make an error containing all input from this point forward
                yield Error { Position = lexer.Start
                              Irritant = Seq.append lexer.String inputs }

            else
                // otherwise, keep going with the updated lexer
                let lexer =
                    { lexer with
                          Automaton = { lexer.Automaton with Current = nextState }
                          String = lexer.String + (string input)
                          Position = lexer.Position + 1u }
                yield! tokenize lexer (Seq.tail inputs)
    }


/// Syntactical spec, where terminals are assumed to identify tokens.
type Grammar = Grammar<Identifier, Identifier>
type Symbol = Symbol<Identifier, Identifier>
type SyntacticalAnalysisTable = Map<Identifier * Identifier, Set<list<Symbol>>>

// these help us handle the differences between a DPDA and an LL(1) parser
type LL1State = Parse | Accept | Dead
type InputAction<'InputSymbol> = Consume | Keep of 'InputSymbol
type ParserAction<'InputSymbol, 'StackSymbol> =
    StackAction<'StackSymbol> * InputAction<'InputSymbol>

/// Table-based LL(1) parser.
type Parser =
    { Automaton: Dpda<LL1State, Identifier, Symbol>
      AcceptsEmpty: bool }

    interface IAutomaton<(LL1State * Stack<Symbol>), TokenInstance, Result<ParserAction<TokenInstance, Symbol>, unit>> with
        member this.View = this.Automaton.Current

        member this.Step input =
            let output, automaton = Automaton.step input.Token this.Automaton
            let state = Automaton.view automaton
            let automaton = { this.Automaton with Current = state }
            let output =
                match output with
                | Error () -> Error ()
                | Ok action ->
                    // input should NOT be consumed when a derivation is performed
                    match action, snd this.Automaton.Current with
                    | ReplaceTop _, NonTerminal _ :: _ -> Ok (action, Keep input)
                    | _ -> Ok (action, Consume)

            output, { this with Automaton = automaton } :> IAutomaton<_, _, _>

/// Functions for creating and manipulating LL(1) parsers.
module Parser =
    let [<Literal>] private Endmarker = "$"

    /// <summary>
    /// Makes an LL(1) parser according to the given syntactical specification.
    /// </summary>
    ///
    /// <returns>
    /// Either a ready-to-use `Parser` or a parsing table with LL(1) conflicts.
    /// </returns>
    let make grammar =
        let follows = Grammar.followSets grammar Endmarker

        // finds all the entries in the table to contain a given production rule
        let entriesForRule (head, body) =
            Grammar.first body grammar
            |> Seq.map
                (function
                // (head, x) for every x in FIRST(body)
                | Some lookahead ->
                    set [ (head, lookahead), body ]
                // if epsilon is in FIRST(body), (head, x) for every x in FOLLOW(head)
                | None ->
                    follows.[head]
                    |> Set.map (fun lookahead -> ((head, lookahead), body)))
            |> Set.unionMany

        // build the parsing table, with a set of productions at each cell
        let entries = grammar.Rules |> Seq.map entriesForRule |> Set.unionMany
        let mutable table = Dictionary()
        for cell, rule in entries do
            if table.ContainsKey(cell) then
                table.[cell] <- Set.add rule table.[cell]
            else
                table.[cell] <- Set.singleton rule

        let isLL1 =
            table
            |> Seq.forall (fun (entry: KeyValuePair<_, _>) -> Set.count entry.Value <= 1)
        if not isLL1 then
            table
            |> Seq.map (fun entry -> entry.Key, entry.Value)
            |> Map.ofSeq
            |> Error
        else
            let mutable transitions = Dictionary()

            let (|->) (state, input, topOfStack) (next, action) =
                if transitions.ContainsKey((state, topOfStack)) then
                    transitions.[(state, topOfStack)] <-
                        Map.add input (next, action) transitions.[(state, topOfStack)]
                else
                    transitions.[(state, topOfStack)] <-
                        Map.ofSeq [ input, (next, action) ]

            // for every terminal, there's a transition (Parse -> Parse) where,
            // if the top of the stack and the input symbol match, remove both
            for symbol in grammar.Terminals do
                (Parse, symbol, Terminal symbol) |-> (Parse, ReplaceTop [])

            // for non-terminals, we add a transition that does a derivation
            // on the stack based on the syntactical analysis table
            // NOTE: PDAs always step on input, so the lookahead is consumed
            for entry in table do
                let (symbol, lookahead), rules = entry.Key, entry.Value
                let derivation = Set.minElement rules
                (Parse, lookahead, NonTerminal symbol) |-> (Parse, ReplaceTop derivation)

            // matching the endmarker as a terminal moves to the accept state
            do (Parse, Endmarker, Terminal Endmarker) |-> (Accept, ReplaceTop [])

            let transitions =
                Map.ofSeq <| seq {
                    for entry in transitions do
                        entry.Key, InputConsumingTransitions entry.Value
                }

            let automaton =
                { Transitions = transitions
                  Current = Parse, [ NonTerminal grammar.Initial; Terminal Endmarker ]
                  Accepting = Set.singleton Accept
                  Dead = Dead }

            let acceptsEmtpy =
                Grammar.first [ NonTerminal grammar.Initial ] grammar
                |> Set.contains None

            Ok { Automaton = automaton; AcceptsEmpty = acceptsEmtpy }

    /// Tests whether a sequence of tokens is accepted by the given parser.
    let accepts parser tokens =
        if Seq.isEmpty tokens then
            parser.AcceptsEmpty
        else
            let rec loop currentState inputs =
                match Seq.tryHead inputs with
                | None -> (fst <| Automaton.view currentState) = Accept
                | Some input ->
                    match Automaton.step input currentState with
                    | Error (), _ -> false
                    | Ok (_, Keep _), nextState -> loop nextState inputs
                    | Ok (_, Consume), nextState -> loop nextState (Seq.tail inputs)

            let tokens =
                Seq.append
                    tokens
                    (Seq.singleton { Token = Endmarker; Lexeme = ""; Position = 0u })

            loop parser tokens


/// A formal language project.
type Project =
    { Id: Identifier
      Lexicon: LexicalSpecification
      Syntax: Grammar }

/// Defines the API between our web app and server backend.
///
/// NOTE: in order to get automatic (de)serialization to and from JSON through
/// Fable.Remoting, everything transmitted needs to be a (public) value type.
type FormallySharp =
    { generateLexer: LexicalSpecification -> Async<Lexer>
      generateParser: Grammar -> Async<Result<Parser, SyntacticalAnalysisTable>>
      saveProject: Project -> Async<unit>
      loadProject: Identifier -> Async<Project> }

/// Defines all Fable.Remoting API endpoints through the `builder` function.
[<RequireQualifiedAccess>]
module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
