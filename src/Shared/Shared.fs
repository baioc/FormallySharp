/// Miscellaneous stuff that is used by both Client and Server.
namespace Shared

open System.Text.RegularExpressions
open System.Runtime.CompilerServices

open Formally.Automata
open Formally.Regular
open Formally.Converter


module Regexp =
    let tryParse (str: string) =
        let str = System.String.Concat(str.Split(' '))
        Some <| Converter.convertRegularDefinitionTextToRegexp(str)


[<Extension>]
module String =
    /// Returns an escaped version of given string for user visibility.
    let visual str =
        let str = Regex.Replace(str, "\n", "\\n")
        let str = Regex.Replace(str, "\t", "\\t")
        let str = Regex.Replace(str, " ", "\u00B7")
        str

/// Regexp with a user-facing string representation.
[<AutoOpen>] // so that we may use unqualified constructors
type UserRegexp =
    { Regexp: Regexp
      String: string }

    static member UserRegexp(string, regexp) =
        { String = string; Regexp = regexp }

    override this.ToString() =
        $"/{String.visual this.String}/"

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

/// Wraps a DFA into a ready-to-use lexer.
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
    { String: string
      Position: uint }

/// Functions for creating and manipulating lexers.
module Lexer =
    /// Builds a Lexer from the given specification.
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

            // mark final states and convert to NFA before union
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
                    | Fragment r -> None)

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
                            | None | Some (Fragment _) -> Intermediary state
                            | Some (TokenClass (r, priority)) -> AcceptToken (state, priority)
                            | Some (Separator r) -> AcceptSeparator state)
                    |> Set.ofSeq)

        { Automaton = automaton; Initial = automaton.Current
          String = ""; Start = 0u; Position = 0u }

    /// Makes a token iff the lexer is currently accepting a **non-empty token**.
    let private tryMakeToken lexer =
        if lexer.Automaton.Current = Set.empty then
            None // ^ dead state, so obviously not accepting
        elif lexer.String = "" then
            None // a lexer should never produce an empty token
        else
            // choose the accepting state with the highest priority
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
            // ensure we're actually accepting a token (and not a separator)
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
                yield Error { Position = lexer.Start; String = lexer.String }

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

                // since the condition above avoids empty strings,
                // we can reset the DFA and recurse with the same inputs
                let lexer =
                    { lexer with
                        Automaton = { lexer.Automaton with Current = lexer.Initial }
                        String = ""
                        Start = lexer.Position
                        Position = lexer.Position }
                yield! tokenize lexer inputs

            // not accepting -> dead ? halt with an error
            elif justDied && (not wasAccepting) then
                yield Error { Position = lexer.Start
                              String = lexer.String + (string input) }

            // otherwise, keep going with the updated lexer
            else
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
