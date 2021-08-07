module Index

open System.Text.RegularExpressions
open Elmish

open Formally.Regular


type LexerPart =
    | Token of Regexp
    | Fragment of Regexp

type LexerSpec =
    { RegularDefinitions: Map<string, LexerPart>
      Separators: Set<Regexp> }

type Project = { Id: string; Lexer: LexerSpec }

let isValidId id = Regex.IsMatch(id, "\w+")

module Regexp =
    let tryParse s =
        match s with
        | "" -> None
        | s -> Some <| Regexp.ofSeq s // TODO


type Model =
    { ProjectInterface: Project
      RegularDefinition: string * string }

type Msg =
    | SetRegularDefinition of string * string
    | AddRegularDefinition of string * LexerPart
    | RemoveRegularDefinition of string
    | AddSeparator of Regexp
    | RemoveSeparator of Regexp
    | SetProjectId of string

let init () : Model * Cmd<Msg> =
    let emptyProject =
        { Id = ""
          Lexer =
              { RegularDefinitions = Map.ofSeq [ ("zero", Token Regexp.Zero); ("one", Token Regexp.One) ]
                Separators = Set.empty } }

    let model =
        { ProjectInterface = emptyProject
          RegularDefinition = "", "" }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinition (name, body) ->
        { model with RegularDefinition = name, body },
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

    | SetProjectId id ->
        { model with
              ProjectInterface = { model.ProjectInterface with Id = id } },
        Cmd.none


open Feliz
open Feliz.Bulma

let regularDefinitions (model: Model) (dispatch: Msg -> unit) =
    let name, body = model.RegularDefinition

    Bulma.block [
        Bulma.content [
            Html.ol [
                for def in model.ProjectInterface.Lexer.RegularDefinitions do
                    Html.li [ prop.text (string def) ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.div [
                    Bulma.input.text [
                        prop.value name
                        prop.placeholder "nome"
                        prop.onChange (fun name -> SetRegularDefinition (name, body) |> dispatch)
                    ]
                ]
                Bulma.control.div [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value body
                            prop.placeholder "expressão regular"
                            prop.onChange (fun body -> SetRegularDefinition (name, body) |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.div [
                    Bulma.button.a [
                        prop.text "adicionar"
                        prop.disabled
                            ((not <| isValidId name) || (Regexp.tryParse body |> Option.isNone))
                        prop.onClick
                            (fun _ -> // TODO
                                AddRegularDefinition
                                    (name,
                                     Regexp.tryParse body
                                     |> Option.get
                                     |> Token)
                                |> dispatch)
                        color.isInfo
                    ]
                ]
            ]
        ]
    ]

let body (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            Bulma.card [
                Bulma.cardHeader [
                    Bulma.cardHeaderTitle.p [
                        prop.text "Definições Regulares"
                        cardHeaderTitle.isCentered
                        size.isSize4
                        color.hasBackgroundInfoDark
                        color.hasTextInfoLight
                    ]
                ]
                Bulma.cardContent [ regularDefinitions model dispatch ]
            ]
        ]
        Bulma.column [
            Bulma.columns [
                Bulma.column [
                    Bulma.card [
                        Bulma.cardHeader [
                            Bulma.cardHeaderTitle.p [
                                prop.text "Entrada"
                                cardHeaderTitle.isCentered
                                size.isSize4
                                color.hasBackgroundInfoDark
                                color.hasTextInfoLight
                            ]
                        ]
                        Bulma.cardContent [
                            Bulma.textarea []
                        ]
                    ]
                ]
                Bulma.column [
                    Bulma.card [
                        Bulma.cardHeader [
                            Bulma.cardHeaderTitle.p [
                                prop.text "Tabela de Símbolos"
                                cardHeaderTitle.isCentered
                                size.isSize4
                                color.hasBackgroundInfoDark
                                color.hasTextInfoLight
                            ]
                        ]
                        Bulma.cardContent [
                            Bulma.table []
                        ]
                    ]
                ]
            ]
        ]
    ]

let toolbar (model: Model) (dispatch: Msg -> unit) =
    let idInvalid = not (isValidId model.ProjectInterface.Id)

    Bulma.level [
        level.isMobile
        prop.children [
            Bulma.levelLeft []
            Bulma.levelRight [
                Bulma.levelItem [
                    Bulma.text.p "projeto:"
                    Bulma.input.text [
                        prop.value model.ProjectInterface.Id
                        prop.onTextChange (SetProjectId >> dispatch)
                        prop.placeholder "identificador"
                    ]
                ]
                Bulma.levelItem [
                    Bulma.button.button [
                        prop.text "abrir"
                        prop.disabled idInvalid
                        prop.onClick (fun _ -> SetProjectId "abrir" |> dispatch ) // TODO
                        color.isDanger
                    ]
                ]
                Bulma.levelItem [
                    Bulma.button.button [
                        prop.text "salvar"
                        prop.disabled idInvalid
                        prop.onClick (fun _ -> SetProjectId "salvar" |> dispatch ) // TODO
                        color.isDanger
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    let repoUrl = "https://github.com/baioc/FormallySharp"

    let header =
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

    let footer =
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

    let body =
        Html.div [
            color.hasBackgroundPrimaryLight
            prop.children [
                Bulma.section [
                    prop.style [
                        style.paddingTop (length.rem 1.0)
                        style.paddingBottom (length.rem 1.0)
                    ]
                    prop.children [ toolbar model dispatch ]
                ]
                Bulma.section [
                    prop.style [
                        style.paddingTop (length.rem 1.0)
                        style.paddingBottom (length.rem 2.0)
                    ]
                    prop.children [ body model dispatch ]
                ]
            ]
        ]

    Html.body [
        header
        body
        footer
    ]
