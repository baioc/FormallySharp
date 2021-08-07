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
    { Project: Project
      InputText: string
      RegularDefinition: string * string * string }

type Msg =
    | SetRegularDefinition of string * string * string
    | AddRegularDefinition of string * LexerPart
    | RemoveRegularDefinition of string
    | AddSeparator of Regexp
    | RemoveSeparator of Regexp
    | SetInputText of string
    | SetProjectId of string

let init () : Model * Cmd<Msg> =
    let emptyProject = // TODO
        { Id = ""
          Lexer =
              { RegularDefinitions = Map.ofSeq [ ("zero", Token Regexp.Zero); ("one", Token Regexp.One) ]
                Separators = Set.empty } }

    let model =
        { Project = emptyProject
          InputText = ""
          RegularDefinition = "token", "", "" }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinition (kind, name, body) ->
        { model with RegularDefinition = kind, name, body },
        Cmd.none

    | AddRegularDefinition (id, def) ->
        let project = model.Project
        let lexer = project.Lexer
        let regularDefinitions = lexer.RegularDefinitions

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  RegularDefinitions = Map.add id def regularDefinitions } } },
        Cmd.none

    | RemoveRegularDefinition id ->
        let project = model.Project
        let lexer = project.Lexer
        let regularDefinitions = lexer.RegularDefinitions

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  RegularDefinitions = Map.remove id regularDefinitions } } },
        Cmd.none

    | AddSeparator regex ->
        let project = model.Project
        let lexer = project.Lexer
        let separators = lexer.Separators

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  Separators = Set.add regex separators } } },
        Cmd.none

    | RemoveSeparator regex ->
        let project = model.Project
        let lexer = project.Lexer
        let separators = lexer.Separators

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  Separators = Set.remove regex separators } } },
        Cmd.none

    | SetInputText text ->
        { model with InputText = text },
        Cmd.none

    | SetProjectId id ->
        { model with
              Project = { model.Project with Id = id } },
        Cmd.none


open Feliz
open Feliz.Bulma

let regularDefinitions (model: Model) (dispatch: Msg -> unit) =
    let kind, name, body = model.RegularDefinition
    let isSep = kind = "separador"
    let regexp = Regexp.tryParse body

    let nameIsValid = isSep || isValidId name
    let regexpIsValid = Option.isSome regexp

    let buttonEnabled = nameIsValid && regexpIsValid
    let overwrite =
        (not isSep) && Map.containsKey name model.Project.Lexer.RegularDefinitions

    let addButton =
        Bulma.button.a [
            prop.text (if not overwrite then "adicionar" else $"editar '{name}'")
            prop.disabled (not buttonEnabled)
            if overwrite then color.isWarning else color.isSuccess
            prop.onClick
                (fun _ ->
                    match kind with
                    | "separador" ->
                        Option.get regexp
                        |> AddSeparator
                        |> dispatch
                    | "token" ->
                        (name, Token (Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch
                    | "fragmento" ->
                        (name, Fragment (Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch
                    | _ -> ())
        ]

    let viewRegularDefinition name regDef =
        let name = Option.defaultValue "" name
        let kind, regexp =
            match regDef with
            | Choice2Of2 separatorRegexp -> "separator", separatorRegexp
            | Choice1Of2 (Token regexp) -> "token", regexp
            | Choice1Of2 (Fragment regexp) -> "fragment", regexp
        Bulma.columns [
            columns.isMobile
            columns.isVCentered
            prop.children [
                Bulma.column [
                    Bulma.text.p [
                        prop.text kind
                        text.isFamilyCode
                        color.hasTextPrimary
                    ]
                ]
                Bulma.column [
                    Bulma.text.p [
                        prop.text name
                        text.isFamilyCode
                        color.hasTextInfo
                    ]
                ]
                Bulma.column [
                    column.isTwoFifths
                    prop.children [
                        Bulma.text.p [
                            prop.text (string regexp)
                            text.isFamilyCode
                        ]
                    ]
                ]
                Bulma.column [
                    column.isNarrow
                    prop.children [
                        Bulma.delete [
                            prop.onClick
                                (fun _ ->
                                    match regDef with
                                    | Choice2Of2 separatorRegexp ->
                                        separatorRegexp
                                        |> RemoveSeparator
                                        |> dispatch
                                    | Choice1Of2 _ ->
                                        name
                                        |> RemoveRegularDefinition
                                        |> dispatch)
                        ]
                    ]
                ]
            ]
        ]

    Bulma.block [
        Bulma.content [
            for regDef in model.Project.Lexer.RegularDefinitions do
                viewRegularDefinition (Some regDef.Key) (Choice1Of2 regDef.Value)
            for regexp in model.Project.Lexer.Separators do
                viewRegularDefinition None (Choice2Of2 regexp)
        ]
        Bulma.columns [
            Bulma.column [
                column.isNarrow
                prop.children [
                    Bulma.select [
                        prop.children [
                            Html.option "token"
                            Html.option "fragmento"
                            Html.option "separador"
                        ]
                        prop.value kind
                        prop.onChange
                            (fun kind ->
                                (kind, name, body)
                                |> SetRegularDefinition
                                |> dispatch)
                    ]
                ]
            ]
            Bulma.column [
                column.isOneFifth
                prop.children [
                    Bulma.input.text [
                        prop.value name
                        prop.placeholder "nome"
                        prop.disabled isSep
                        prop.onChange
                            (fun (name: string) ->
                                (kind, name.ToUpperInvariant(), body)
                                |> SetRegularDefinition
                                |> dispatch)
                        if not nameIsValid then color.isDanger
                    ]
                ]
            ]
            Bulma.column [
                prop.children [
                    Bulma.input.text [
                        prop.value body
                        prop.placeholder "expressão regular"
                        prop.onChange
                            (fun body ->
                                (kind, name, body)
                                |> SetRegularDefinition
                                |> dispatch)
                        if not regexpIsValid then color.isDanger
                    ]
                ]
            ]
            Bulma.column [
                column.isNarrow
                prop.children [ addButton ]
            ]
        ]
    ]

let body (model: Model) (dispatch: Msg -> unit) =
    let cardTitle (text: string) =
        Bulma.cardHeaderTitle.p [
            prop.text text
            cardHeaderTitle.isCentered
            size.isSize4
            color.hasBackgroundGreyDark
            color.hasTextGreyLighter
        ]

    Bulma.columns [
        Bulma.column [
            Bulma.card [
                Bulma.cardHeader [ cardTitle "Definições Regulares" ]
                Bulma.cardContent [ regularDefinitions model dispatch ]
            ]
        ]
        Bulma.column [
            Bulma.columns [
                Bulma.column [
                    Bulma.card [
                        Bulma.cardHeader [ cardTitle "Entrada" ]
                        Bulma.cardContent [
                            Bulma.textarea [
                                prop.onChange (SetInputText >> dispatch)
                            ]
                        ]
                    ]
                ]
                Bulma.column [
                    Bulma.card [
                        Bulma.cardHeader [ cardTitle "Tabela de Símbolos" ]
                        Bulma.cardContent [
                            Bulma.table []
                        ]
                    ]
                ]
            ]
        ]
    ]

let toolbar (model: Model) (dispatch: Msg -> unit) =
    let idInvalid = not (isValidId model.Project.Id)

    Bulma.level [
        prop.children [
            Bulma.levelLeft []
            Bulma.levelRight [
                Bulma.levelItem [
                    Bulma.text.p [
                        text.hasTextWeightBold
                        prop.text "projeto:"
                    ]
                    Bulma.input.text [
                        prop.value model.Project.Id
                        prop.onTextChange (SetProjectId >> dispatch)
                        prop.placeholder "identificador"
                    ]
                ]
                Bulma.levelItem [
                    Bulma.columns [
                        columns.isMobile
                        prop.children [
                            Bulma.column [
                                Bulma.button.button [
                                    prop.text "abrir"
                                    prop.disabled idInvalid
                                    prop.onClick
                                        (fun _ -> SetProjectId "abrir" |> dispatch ) // TODO
                                    color.isDanger
                                ]
                            ]
                            Bulma.column [
                                Bulma.button.button [
                                    prop.text "salvar"
                                    prop.disabled idInvalid
                                    prop.onClick
                                        (fun _ -> SetProjectId "salvar" |> dispatch ) // TODO
                                    color.isDanger
                                ]
                            ]
                        ]
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
