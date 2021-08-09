/// Miscellaneous stuff that is used by both Client and Server.
namespace Shared


// defines all API endpoints
module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName


open System.Text.RegularExpressions
open Formally.Regular

module Regexp =
    let tryParse s =
        match s with
        | "" -> None
        | s -> Some <| Regexp.ofSeq s // TODO

/// Regexp with a user-facing string representation.
type UserRegexp =
    struct
        val Regexp: Regexp
        val private String: string

        new(string) =
            UserRegexp(string, Regexp.tryParse string |> Option.get)

        new(string, regexp) =
            { String = string; Regexp = regexp }

        override this.ToString() =
            let str = this.String
            let str = Regex.Replace(str, "\n", "\\n")
            let str = Regex.Replace(str, "\t", "\\t")
            $"/{str}/"
    end

type TokenPriority = int

type RegularDefinition =
    | Token of UserRegexp * TokenPriority
    | Fragment of UserRegexp
    | Separator of UserRegexp

type Identifier = string
module Identifier =
    let isValid str = Regex.IsMatch(str, @"\w+")

type LexicalSpecification = Map<Identifier, RegularDefinition>

type Project =
    { Id: Identifier
      Lexer: LexicalSpecification }

type LexerState =
    | AcceptToken of string * TokenPriority
    | AcceptSeparator of string
    | Intermediary of string

type Lexer = Dfa<LexerState>

module Lexer =
    /// Makes a Lexer from the given specification.
    let make spec =
        let makeAutomaton name def =
            let markFinal (automaton: Dfa<_>) state =
                if Set.contains state automaton.Accepting then
                    name // guaranteed singleton since generated from a regexp
                else
                    if state = Set.empty then
                        "{}" // dead state can be shared
                    else
                        Set.map string state
                        |> String.concat ","
                        |> sprintf "%s:{%s}" name // prefix is unique

            // convert regexp to DFA in the case of tokens and separators
            match def with
            | Fragment _ -> None
            | Token (r, _)
            | Separator r ->
                let automaton = Dfa.ofRegexp r.Regexp
                automaton // mark final states and build NFA before union
                |> Dfa.map (markFinal automaton)
                |> Dfa.toNfa
                |> Some

        // make an automaton for each token and separator definition
        let automatons =
            Map.toSeq spec
            |> Seq.map (fun (name, def) -> makeAutomaton name def)
            |> Seq.filter Option.isSome
            |> Seq.map Option.get

        let undiscriminatedUnion union nfa =
            Nfa.union union nfa
            |> Nfa.map
                (fun state -> // maintains structure due to uniqueness of names
                    match state with
                    | Choice1Of2 s
                    | Choice2Of2 s -> s)

        let initial =
            { Transitions = Map.empty; Accepting = set []; Current = set [] }

        // compute the union of all automatons
        automatons
        |> Seq.fold undiscriminatedUnion initial
        // determinze the result
        |> Nfa.toDfa
        // filter out dead transitions
        |> Dfa.filter (fun (q, a) q' -> q' <> set [ "{}" ])
        // distinguish states
        |> Dfa.map
            (fun set ->
                set
                |> Seq.filter (fun state -> state <> "{}")
                |> Seq.map
                    (fun state ->
                        match Map.tryFind state spec with
                        | None | Some (Fragment _) -> Intermediary state
                        | Some (Token (_, priority)) -> AcceptToken (state, priority)
                        | Some (Separator _) -> AcceptSeparator state)
                |> Set.ofSeq)

    let tokenize lexer input =
        null // TODO


/// Defines the API between our webapp and server backend.
type FormallySharp =
    { generateLexer: LexicalSpecification -> Async<Lexer>
      saveProject: Project -> Async<unit>
      loadProject: Identifier -> Async<Project> }
