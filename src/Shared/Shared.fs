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
    let isValid str = Regex.IsMatch(str, "\w+")

type LexicalSpecification = Map<Identifier, RegularDefinition>

type Project =
    { Id: Identifier
      Lexer: LexicalSpecification }

type TokenInstance =
    { Token: Identifier
      Lexeme: string
      Position: uint }

type Lexer = obj // TODO

module Lexer =
    let tokenize (lexer: Lexer) (input: char seq) : TokenInstance seq =
        null // TODO


/// Defines the API between our webapp and server backend.
type FormallySharp =
    { generateLexer: LexicalSpecification -> Async<Lexer>
      saveProject: Project -> Async<unit>
      loadProject: Identifier -> Async<Project> }
