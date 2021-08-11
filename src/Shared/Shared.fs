namespace Shared

open System.Text.RegularExpressions
open System.Runtime.CompilerServices

open Formally.Automata
open Formally.Regular
open Formally.Converter


module Regexp =
    /// Unescapes some sequences into their "raw" characters.
    let unescape (str: string) =
        // XXX: for some reason, this works while `Regex.Replace` doesn't
        str.Replace(@"\t",   "\t")
           .Replace(@"\r\n", "\r\n")
           .Replace(@"\n",   "\n")
           .Replace(@"\\",   "\\")

    /// The inverse of `unescape`.
    let escape (str: string) =
        str.Replace("\\",   @"\\")
           .Replace("\t",   @"\t")
           .Replace("\r\n", @"\r\n")
           .Replace("\n",   @"\n")

    let tryParse str =
        let str = unescape str
        // FIXME: even when input is not valid, this will still give a (weird) regexp
        Some <| Converter.convertRegularDefinitionTextToRegexp(str)


[<Extension>]
module String =
    /// Returns an escaped version of given string for user visibility.
    let visual (str: string) =
        str.Replace("\r\n", "\\r\\n")
           .Replace("\n", "\\n")
           .Replace("\t", "\\t")
           .Replace(" ", "\u00B7") // <- unicode for visual space

/// Regexp with a user-facing string representation.
[<AutoOpen>] // so that we may use unqualified constructors
type UserRegexp =
    { Regexp: Regexp
      String: string }

    static member UserRegexp(string, regexp) =
        { String = string; Regexp = regexp }

type TokenPriority = int

type RegularDefinition =
    | TokenClass of UserRegexp * TokenPriority
    | Fragment of UserRegexp
    | Separator of UserRegexp

type Identifier = string

module Identifier =
    let isValid str = Regex.IsMatch(str, @"\w+")

type LexicalSpecification = Map<Identifier, RegularDefinition>

/// A formal language project.
type Project =
    { Id: Identifier
      Lexer: LexicalSpecification }

type LexerState =
    | AcceptToken of Identifier * TokenPriority
    | AcceptSeparator of Identifier
    | Intermediary of Identifier

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

type LexicalError =
    { String: char seq
      Position: uint }

/// Functions for creating and manipulating lexers.
// TODO: write tests for these
module Lexer =
    /// Builds a Lexer with a clean state according to the given specification.
    let make spec =
        let makeAutomaton name regexp =
            let automaton = Dfa.ofRegexp regexp

            let markFinal state =
                if Set.contains state automaton.Accepting then
                    name // guaranteed singleton since generated with Aho's algorithm
                elif state = Set.empty then
                    "{}" // dead state can be shared
                else
                    Set.map string state
                    |> String.concat ","
                    |> sprintf "%s:{%s}" name // prefix is unique

            // mark final states and prepare for union
            automaton
            |> Dfa.map markFinal
            |> Dfa.toNfa

        let undiscriminatedUnion union nfa =
            Nfa.union union nfa
            |> Nfa.map
                (fun state -> // maintains structure due to uniqueness of names
                    match state with
                    | Choice1Of2 s
                    | Choice2Of2 s -> s)

        // make an automaton for each token and separator
        let automatons =
            Map.toSeq spec
            |> Seq.choose
                (fun (name, def) ->
                    match def with
                    | TokenClass (r, _)
                    | Separator r -> Some (makeAutomaton name r.Regexp)
                    | Fragment _ -> None)

        let initial =
            { Transitions = Map.empty; Accepting = set []; Current = set [] }

        let automaton =
            // compute the union of all automatons
            automatons
            |> Seq.fold undiscriminatedUnion initial
            // determinze the result
            |> Nfa.toDfa
            // filter out dead transitions
            |> Dfa.filter (fun (q, a) q' -> q' <> set [ "{}" ])
            // identify lexer states
            |> Dfa.map
                (fun set ->
                    set
                    |> Seq.filter (fun state -> state <> "{}")
                    |> Seq.map
                        (fun state ->
                            match Map.tryFind state spec with
                            | Some (TokenClass (r, priority)) -> AcceptToken (state, priority)
                            | Some (Separator r) -> AcceptSeparator state
                            | notAccepting -> Intermediary state)
                    |> Set.ofSeq)

        { Automaton = automaton; Initial = automaton.Current
          String = ""; Start = 0u; Position = 0u }

    /// Makes a token iff the lexer is currently accepting a non-empty lexeme.
    let private tryMakeToken lexer =
        if lexer.Automaton.Current = Set.empty then
            None // ^ dead state, so obviously not accepting
        elif lexer.String = "" then
            None // a lexer must never produce a token from an empty lexeme
        else
            // choose the state with the highest priority
            let acceptLabel =
                Set.toSeq lexer.Automaton.Current
                |> Seq.map
                    (fun label ->
                        match label with
                        | Intermediary _ -> System.Int64.MinValue, label
                        | AcceptSeparator _ -> System.Int64.MinValue + 1L, label
                        | AcceptToken (_, priority) -> int64 priority, label)
                |> Seq.maxBy fst
                |> snd
            // ensure we're actually accepting a token
            match acceptLabel with
            | AcceptToken (token, priority) ->
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
                let error = { Position = lexer.Start; String = lexer.String }
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
                              String = Seq.append lexer.String inputs }

            else
                // otherwise, keep going with the updated lexer
                let lexer =
                    { lexer with
                          Automaton = { lexer.Automaton with Current = nextState }
                          String = lexer.String + (string input)
                          Position = lexer.Position + 1u }
                yield! tokenize lexer (Seq.tail inputs)
    }


/// Defines the API between our web app and server backend.
///
/// NOTE: in order to get automatic (de)serialization to and from JSON through
/// Fable.Remoting, every type that is transmitted needs to be *fully* public.
type FormallySharp =
    { generateLexer: LexicalSpecification -> Async<Lexer>
      saveProject: Project -> Async<unit>
      loadProject: Identifier -> Async<Project> }

/// Defines all Fable.Remoting API endpoints through the `builder` function.
[<RequireQualifiedAccess>]
module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
