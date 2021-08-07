module Index

open Elmish

open Formally.Regular


type LexerPart =
    | Token of Regexp
    | Fragment of Regexp

type LexerSpec =
    { RegularDefinitions: Map<string, LexerPart>
      Separators: Set<Regexp> }

type Project = { Id: string; Lexer: LexerSpec }


type Model =
    { ProjectInterface: Project
      Error: string option }

type Msg =
    | SetProjectId of string
    | AddRegularDefinition of string * LexerPart
    | RemoveRegularDefinition of string
    | AddSeparator of Regexp
    | RemoveSeparator of Regexp

let init () : Model * Cmd<Msg> =
    let emptyProject =
        { Id = ""
          Lexer =
              { RegularDefinitions = Map.empty
                Separators = Set.empty } }

    let model =
        { ProjectInterface = emptyProject
          Error = None }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetProjectId id ->
        { model with
              ProjectInterface = { model.ProjectInterface with Id = id } },
        Cmd.none

    | AddRegularDefinition (id, def) ->
        let project = model.ProjectInterface
        let lexer = project.Lexer
        let regularDefinitions = lexer.RegularDefinitions

        { model with
              ProjectInterface =
                  { project with
                        Lexer =
                            { lexer with
                                  RegularDefinitions = Map.add id def regularDefinitions } } },
        Cmd.none

    | RemoveRegularDefinition id ->
        let project = model.ProjectInterface
        let lexer = project.Lexer
        let regularDefinitions = lexer.RegularDefinitions

        { model with
              ProjectInterface =
                  { project with
                        Lexer =
                            { lexer with
                                  RegularDefinitions = Map.remove id regularDefinitions } } },
        Cmd.none

    | AddSeparator regex ->
        let project = model.ProjectInterface
        let lexer = project.Lexer
        let separators = lexer.Separators

        { model with
              ProjectInterface =
                  { project with
                        Lexer =
                            { lexer with
                                  Separators = Set.add regex separators } } },
        Cmd.none

    | RemoveSeparator regex ->
        let project = model.ProjectInterface
        let lexer = project.Lexer
        let separators = lexer.Separators

        { model with
              ProjectInterface =
                  { project with
                        Lexer =
                            { lexer with
                                  Separators = Set.remove regex separators } } },
        Cmd.none


open Feliz
open Feliz.Bulma

let body model dispatch =
    Bulma.title "test"

let view (model: Model) (dispatch: Msg -> unit) =
    let repoUrl = "https://github.com/baioc/FormallySharp"

    Html.body [
        Bulma.navbar [
            color.isPrimary
            prop.style [ style.padding (length.rem 1) ]
            prop.children [
                Bulma.navbarBrand.div [
                    Bulma.title [
                        prop.text "Formally#"
                        title.is1
                    ]
                ]
                Bulma.navbarMenu [
                    Bulma.navbarEnd.div [
                        Bulma.navbarItem.a [
                            prop.className "fab fa-github"
                            prop.href repoUrl
                            size.isSize3
                        ]
                    ]
                ]
            ]
        ]
        Bulma.section [ body model dispatch ]
        Bulma.footer [
            prop.style [
                style.paddingTop (length.rem 1.5)
                style.paddingBottom (length.rem 1.5)
            ]
            prop.children [
                Html.a [
                    prop.text "Trabalho para a disciplina de Linguagens Formais e Compiladores (INE5421)"
                    prop.href repoUrl
                ]
                Html.p "© 2021 Gabriel B. Sant'Anna, Marcelo Contin, João Vitor"
            ]
        ]
    ]
