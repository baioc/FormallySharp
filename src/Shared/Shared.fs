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

type TokenInstance =
    { Token: Identifier
      Lexeme: string
      Position: uint }

type LexicalError =
    { String: string
      Position: uint }

type LexerOutput =
    | Token of TokenInstance
    | Error of LexicalError
    | Nothing
    | Unget of char

/// Wraps a DFA into a ready-to-use lexer.
type Lexer =
    { Automaton: Dfa<Set<LexerState>>
      Initial: Set<LexerState>
      String: string
      Start: uint
      Position: uint }

    interface IAutomaton<Identifier, char, LexerOutput> with
        override this.View =
            Set.toSeq this.Automaton.Current
            |> Seq.map
                (function
                | AcceptToken (label, _)
                | AcceptSeparator label
                | Intermediary label -> label)
            |> String.concat "|"

        override this.Step input =
            // add input to the buffer and feed it to the automaton
            let previous = this.Automaton.Current
            let wasAccepting = Set.contains previous this.Automaton.Accepting
            let next = Automaton.step input this.Automaton |> snd |> Automaton.view
            let updatedString = this.String + (sprintf "%c" input)
            let nextPosition = this.Position + 1u

            // transition: dead -> * ? try to get out of panic mode
            if previous = this.Automaton.Dead then
                let next =
                    { this.Automaton with Current = this.Initial }
                    |> Automaton.step input
                    |> snd
                    |> Automaton.view

                // dead -> accepting ? escape panic mode
                if Set.contains next this.Automaton.Accepting then
                    Unget input,
                    { this with
                          String = ""
                          Start = this.Position
                          Automaton = { this.Automaton with Current = this.Initial }
                    } :> IAutomaton<_, _, _>
                // dead -> not-accepting ? keep accumulating errors
                else
                    Error { String = updatedString; Position = this.Start },
                    { this with
                          String = updatedString
                          Position = nextPosition
                          Automaton = { this.Automaton with Current = this.Automaton.Dead }
                    } :> IAutomaton<_, _, _>

            // transition: not-accepting -> dead ? error and go into panic mode
            elif next = this.Automaton.Dead && not wasAccepting then
                Error { String = updatedString; Position = this.Start },
                { this with
                      String = updatedString
                      Position = nextPosition
                      Automaton = { this.Automaton with Current = this.Automaton.Dead }
                } :> IAutomaton<_, _, _>

            // transition: accepting -> dead ? generate a token depending on the state
            elif next = this.Automaton.Dead && wasAccepting then
                // choose the accepting state with the highest priority
                let acceptLabel =
                    Set.toSeq previous
                    |> Seq.map
                        (fun label ->
                            match label with
                            | Intermediary _ -> System.Int64.MinValue, label
                            | AcceptSeparator _ -> System.Int64.MinValue + 1L, label
                            | AcceptToken (_, priority) -> int64 priority, label)
                    |> Seq.maxBy fst
                    |> snd

                // output is only some token instance when the accepting state refers to a token
                let output =
                    match acceptLabel with
                    | AcceptSeparator _ -> Unget input
                    | AcceptToken (token, _) ->
                        Token { Token = token
                                Lexeme = this.String
                                Position = this.Start }
                    | Intermediary _ -> Nothing // <- unreachable

                // since *something* was accepted, we reset both the buffer and the automaton,
                // then, it is up to the caller to try this input again
                output,
                { this with
                      String = ""
                      Start = this.Position
                      Automaton = { this.Automaton with Current = this.Initial }
                } :> IAutomaton<_, _, _>

            // otherwise, keep going with the updated buffer and automaton state
            else
                Nothing,
                { this with
                      String = updatedString
                      Position = nextPosition
                      Automaton = { this.Automaton with Current = next }
                } :> IAutomaton<_, _, _>

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

    /// Generates tokens and/or errors by an input sequence.
    let rec tokenize (lexer: IAutomaton<_, char, LexerOutput>) inputs =
        seq {
            // when the input stream is empty, check what state we ended up in
            if Seq.isEmpty inputs then
                let eof = '\u0000' // fake input, we don't care if it rejects
                let output, _ = Automaton.step eof lexer
                match output with
                | Token last -> yield Ok last
                | notToken -> ()

            // otherwise, iterate down the input stream
            else
                let input = Seq.head inputs
                let output, lexer = Automaton.step input lexer

                // define how to continue based on output
                match output with
                // "ungetc" due to lookahead
                | Token token ->
                    yield Ok token
                    yield! tokenize lexer inputs
                | Unget c ->
                    yield! tokenize lexer inputs

                // skip errors (all but the last) until we leave panic mode
                | Error error ->
                    let mutable lastError = error
                    let isError = function Error e -> true | notError -> false
                    let mutable lexer = lexer
                    let mutable inputs = Seq.tail inputs
                    let mutable output = output
                    while not (Seq.isEmpty inputs) && isError output do
                        let out, next = Automaton.step (Seq.head inputs) lexer
                        output <- out
                        match out with
                        | Error error ->
                            lexer <- next
                            inputs <- Seq.tail inputs
                            lastError <- error
                        | notError -> ()
                    yield Result.Error lastError
                    yield! tokenize lexer inputs

                // simply keep going
                | Nothing ->
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
