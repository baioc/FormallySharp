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
    let token = "token"
    let fragment = "fragmento"
    let separator = "separador"

    let kind, name, body = model.RegularDefinition
    let regexp = Regexp.tryParse body
    let isSep = kind = separator

    let nameIsValid = isSep || isValidId name
    let regexpIsValid = Option.isSome regexp

    let buttonEnabled = nameIsValid && regexpIsValid
    let willOverwrite =
        (not isSep) && Map.containsKey name model.Project.Lexer.RegularDefinitions

    let addButton =
        Bulma.button.a [
            prop.text (if not willOverwrite then "adicionar" else $"editar '{name}'")
            prop.disabled (not buttonEnabled)
            if willOverwrite then color.isWarning else color.isSuccess
            prop.onClick
                (fun _ ->
                    if kind = separator then
                        Option.get regexp
                        |> AddSeparator
                        |> dispatch
                    elif kind = token then
                        (name, Token (Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch
                    elif kind = fragment then
                        (name, Fragment (Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch)
        ]

    let viewRegularDefinition name regDef =
        let name = Option.defaultValue "" name
        let kind, regexp =
            match regDef with
            | Choice1Of2 (Token regexp) -> token, regexp
            | Choice1Of2 (Fragment regexp) -> fragment, regexp
            | Choice2Of2 separatorRegexp -> separator, separatorRegexp
        Bulma.columns [
            columns.isVCentered
            columns.isMobile
            columns.isMultiline
            prop.children [
                Bulma.column [
                    column.isOneFifthTablet
                    prop.children [
                        Bulma.text.p [
                            prop.text kind
                            text.isFamilyCode
                            color.hasTextPrimary
                        ]
                    ]
                ]
                Bulma.column [
                    column.isOneFifthTablet
                    prop.children [
                        Bulma.text.p [
                            prop.text name
                            text.isFamilyCode
                            color.hasTextInfo
                        ]
                    ]
                ]
                Bulma.column [
                    column.isThreeFifthsMobile
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
                                    | Choice1Of2 _ ->
                                        name
                                        |> RemoveRegularDefinition
                                        |> dispatch
                                    | Choice2Of2 sep ->
                                        sep
                                        |> RemoveSeparator
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
            columns.isMobile
            columns.isMultiline
            prop.children [
                Bulma.column [
                    column.isNarrow
                    prop.children [
                        Bulma.select [
                            prop.children [
                                Html.option token
                                Html.option fragment
                                Html.option separator
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
                    column.isOneFifthTablet
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
    ]

let main (model: Model) (dispatch: Msg -> unit) =
    let cardTitle (title: string) =
        Bulma.cardHeaderTitle.p [
            prop.text title
            cardHeaderTitle.isCentered
            size.isSize4
            color.hasBackgroundGreyDark
            color.hasTextGreyLighter
        ]

    let projectInterface =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Definições Regulares" ]
            Bulma.cardContent [ regularDefinitions model dispatch ]
        ]

    let testInput =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Entrada" ]
            Bulma.cardContent [
                Bulma.textarea [
                    prop.onChange (SetInputText >> dispatch)
                ]
            ]
        ]

    let testOutput =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Tabela de Símbolos" ]
            Bulma.cardContent [
                Bulma.table [
                    table.isFullWidth
                    prop.children [
                        // TODO
                        Html.thead [
                            Html.tr [
                                Html.th "Token"
                                Html.th "Lexema"
                                Html.th "Posição"
                            ]
                        ]
                        Html.tbody [
                            Html.tr [
                                Html.th "posição"
                                Html.td "token"
                                Html.td "lexema"
                            ]
                            Html.tr [
                                Html.th "posição"
                                Html.td "token"
                                Html.td "lexema"
                            ]
                        ]
                    ]
                ]
            ]
        ]

    Bulma.tile [
        tile.isAncestor
        prop.children [
            Bulma.tile [
                tile.isParent
                prop.children [
                    Bulma.tile [
                        tile.isChild
                        prop.children [ projectInterface ]
                    ]
                ]
            ]
            Bulma.tile [
                prop.children [
                    Bulma.tile [
                        tile.isParent
                        prop.children [
                            Bulma.tile [
                                tile.isChild
                                prop.children [ testInput ]
                            ]
                        ]
                    ]
                    Bulma.tile [
                        tile.isParent
                        prop.children [
                            Bulma.tile [
                                tile.isChild
                                prop.children [ testOutput ]
                            ]
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
            Bulma.levelLeft [
                Bulma.levelItem [
                    Bulma.tabs [
                        tabs.isBoxed
                        prop.children [
                            Html.ul [
                                Bulma.tab [
                                    tab.isActive
                                    prop.children [
                                        Html.a [
                                            prop.text "léxico"
                                        ]
                                    ]
                                ]
                                Bulma.tab [
                                    prop.children [
                                        Html.a [
                                            prop.text "sintático"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            Bulma.levelRight [
                Bulma.levelItem [
                    Bulma.columns [
                        columns.isVCentered
                        columns.isMobile
                        columns.isMultiline
                        prop.children [
                            Bulma.column [
                                column.isNarrow
                                prop.children [
                                    Bulma.text.p [
                                        text.hasTextWeightBold
                                        prop.text "projeto:"
                                    ]
                                ]
                            ]
                            Bulma.column [
                                prop.children [
                                    Bulma.input.text [
                                        prop.value model.Project.Id
                                        prop.onTextChange (SetProjectId >> dispatch)
                                        prop.placeholder "identificador"
                                    ]
                                ]
                            ]
                            Bulma.column [
                                column.isNarrow
                                prop.children [
                                    Bulma.button.button [
                                        prop.text "abrir"
                                        prop.disabled idInvalid
                                        prop.onClick
                                            (fun _ -> SetProjectId "abrir" |> dispatch ) // TODO
                                        color.isDanger
                                    ]
                                ]
                            ]
                            Bulma.column [
                                column.isNarrow
                                prop.children [
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
                    prop.children [ main model dispatch ]
                ]
            ]
        ]

    Html.body [
        header
        body
        footer
    ]
