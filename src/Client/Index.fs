module Index

open System.Text.RegularExpressions
open Elmish
open Elmish.SweetAlert
open Feliz
open Feliz.Bulma

open Formally.Regular

Fable.Core.JsInterop.importAll "./style.css"


module Regexp =
    let tryParse s =
        match s with
        | "" -> None
        | s -> Some <| Regexp.ofSeq s // TODO

/// Regexp with a user-facing string representation.
type UserRegexp =
    struct
        val Regexp: Regexp
        val private Text: string

        new(text) =
            UserRegexp(text, Regexp.tryParse text |> Option.get)

        new(text, regexp) =
            { Text = text; Regexp = regexp }

        override this.ToString() =
            let text = this.Text
            let text = Regex.Replace(text, "\n", "\\n")
            let text = Regex.Replace(text, "\t", "\\t")
            $"/{text}/"
    end

type LexerPart =
    | Token of UserRegexp
    | Fragment of UserRegexp

type LexerSpec =
    { RegularDefinitions: Map<string, LexerPart>
      Delimiters: Set<UserRegexp> }

type Project = { Id: string; Lexer: LexerSpec }

module Project =
    let isValidId id = Regex.IsMatch(id, "\w+")

type TokenInstance =
    { Token: string
      Lexeme: string
      Position: uint }

type Lexer = obj // TODO

module Lexer =
    let tokenize (lexer: Lexer) (input: char seq) : TokenInstance seq =
        null // TODO


type Model =
    { Project: Project
      RegularDefinition: string * string * string // kind, name, regexp
      InputText: string
      SymbolTable: TokenInstance seq
      Lexer: Lexer option }

type Msg =
    | SetRegularDefinition of string * string * string
    | AddRegularDefinition of string * LexerPart
    | RemoveRegularDefinition of string
    | AddDelimiter of UserRegexp
    | RemoveDelimiter of UserRegexp
    | SetInputText of string
    | GenerateLexer of LexerSpec
    | GeneratedLexer of Lexer
    | SetProjectId of string
    | SaveProject of Project
    | LoadProject of string
    | LoadedProject of Project

// TODO
let init () : Model * Cmd<Msg> =
    let emptyProject =
        { Id = ""
          Lexer =
              { RegularDefinitions = Map.ofSeq [ ("ALPHA", Fragment (UserRegexp "[a-zA-Z_]")); ("ID", Token (UserRegexp "x") ); ]
                Delimiters = set [ UserRegexp " \n\t" ] } }

    let model =
        { Project = emptyProject
          InputText = ""
          RegularDefinition = "token", "", ""
          SymbolTable = [ { Token = "ID"; Lexeme = "x"; Position = 0u }; { Token = "LET"; Lexeme = "let"; Position = 16u } ]
          Lexer = None }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinition (kind, name, body) ->
        { model with
              RegularDefinition = kind, name, body },
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

    | AddDelimiter regexp ->
        let project = model.Project
        let lexer = project.Lexer
        let delimiters = lexer.Delimiters

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  Delimiters = Set.add regexp delimiters } } },
        Cmd.none

    | RemoveDelimiter regexp ->
        let project = model.Project
        let lexer = project.Lexer
        let delimiters = lexer.Delimiters

        { model with
              Project =
                  { project with
                        Lexer =
                            { lexer with
                                  Delimiters = Set.remove regexp delimiters } } },
        Cmd.none

    | SetInputText text ->
        { model with
              InputText = text
              SymbolTable =
                if Option.isNone model.Lexer then model.SymbolTable
                else Lexer.tokenize model.Lexer text },
        Cmd.none

    | GenerateLexer spec -> // TODO
        let toastAlert =
            ToastAlert("gerando lexer...")
                .Position(AlertPosition.Center)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        SweetAlert.Run(toastAlert)

    | GeneratedLexer lexer ->
        let toastAlert =
            ToastAlert("lexer gerado com sucesso!")
                .Position(AlertPosition.Center)
                .ConfirmButton(true)
                .Timeout(3000)
                .Type(AlertType.Success)

        { model with Lexer = Some lexer },
        SweetAlert.Run(toastAlert)

    | SetProjectId id ->
        { model with
              Project = { model.Project with Id = id } },
        Cmd.none

    | SaveProject project -> // TODO
        let toastAlert =
            ToastAlert($"projeto para \"{project.Id}\" foi salvo")
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(false)
                .Timeout(3000)
                .Type(AlertType.Success)

        model,
        SweetAlert.Run(toastAlert)

    | LoadProject id -> // TODO
        let toastAlert =
            ToastAlert($"carregando projeto para \"{id}\"...")
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        SweetAlert.Run(toastAlert)

    | LoadedProject project ->
        let toastAlert =
            ToastAlert($"projeto para \"{project.Id}\" foi carregado")
                .Position(AlertPosition.Top)
                .ConfirmButton(true)
                .Timeout(5000)
                .Type(AlertType.Success)

        { model with Project = project },
        SweetAlert.Run(toastAlert)


let project (model: Model) (dispatch: Msg -> unit) =
    let token = "token"
    let fragment = "fragmento"
    let delimiter = "separador"

    let kind, name, body = model.RegularDefinition
    let regexp = Regexp.tryParse body
    let isDelimiter = kind = delimiter

    let nameIsValid = isDelimiter || Project.isValidId name
    let regexpIsValid = Option.isSome regexp

    let buttonEnabled = nameIsValid && regexpIsValid
    let willOverwrite =
        (not isDelimiter) && Map.containsKey name model.Project.Lexer.RegularDefinitions

    let addButton =
        Bulma.button.a [
            prop.text (if not willOverwrite then "adicionar" else $"editar <{name}>")
            prop.disabled (not buttonEnabled)
            prop.onClick
                (fun _ ->
                    if kind = delimiter then
                        UserRegexp(body, Option.get regexp)
                        |> AddDelimiter
                        |> dispatch
                    elif kind = token then
                        (name, Token <| UserRegexp(body, Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch
                    elif kind = fragment then
                        (name, Fragment <| UserRegexp(body, Option.get regexp))
                        |> AddRegularDefinition
                        |> dispatch)
            if willOverwrite then color.isWarning else color.isSuccess
        ]

    let viewRegularDefinition name regDef =
        let name = Option.defaultValue "" name
        let kind, (regexp: UserRegexp) =
            match regDef with
            | Choice1Of2 (Token regexp) -> token, regexp
            | Choice1Of2 (Fragment regexp) -> fragment, regexp
            | Choice2Of2 delimiterRegexp -> delimiter, delimiterRegexp

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
                                        |> RemoveDelimiter
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
            for regexp in model.Project.Lexer.Delimiters do
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
                                Html.option delimiter
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
                            prop.disabled isDelimiter
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

        let hasToken =
            Map.exists
                (fun name part ->
                    match part with
                    | Token _ -> true
                    | _ -> false)
                model.Project.Lexer.RegularDefinitions
        Bulma.level [
            Bulma.levelItem [
                Bulma.button.button [
                    prop.text (if hasToken then "Gerar Lexer" else "Defina ao menos um token")
                    prop.disabled (not hasToken)
                    prop.onClick (fun _ -> GenerateLexer model.Project.Lexer |> dispatch )
                    button.isLarge
                    if hasToken then color.isPrimary else color.isWarning
                ]
            ]
        ]
    ]

let simulation (model: Model) (dispatch: Msg -> unit) =
    let hasLexer = Option.isSome model.Lexer
    Bulma.columns [
        Bulma.column [
            Bulma.textarea [
                prop.onChange (SetInputText >> dispatch)
                prop.disabled (not hasLexer)
                prop.placeholder  (if hasLexer then "Forneça uma entrada ao lexer."
                                   else "O lexer ainda não foi gerado." )
            ]
        ]
        Bulma.column [
            Bulma.table [
                table.isFullWidth
                table.isHoverable
                table.isBordered
                prop.children [
                    Html.thead [
                        Html.tr [
                            Html.th [
                                prop.text "Token"
                                text.hasTextCentered
                            ]
                            Html.th [
                                prop.text "Lexema"
                                text.hasTextCentered
                            ]
                            Html.th [
                                prop.text "Posição"
                                text.hasTextCentered
                            ]
                        ]
                    ]
                    Html.tbody [
                        for entry in model.SymbolTable do
                            Html.tr [
                                Html.td [
                                    prop.text entry.Token
                                    color.hasTextInfo
                                ]
                                Html.td entry.Lexeme
                                Html.td [
                                    prop.text (string entry.Position)
                                    color.hasTextLink
                                ]
                            ]
                    ]
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
            Bulma.cardHeader [ cardTitle "Especificação Léxica" ]
            Bulma.cardContent [ project model dispatch ]
        ]

    let decisionInterface =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Reconhecimento" ]
            Bulma.cardContent [ simulation model dispatch ]
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
                tile.isParent
                prop.children [
                    Bulma.tile [
                        tile.isChild
                        prop.children [ decisionInterface ]
                    ]
                ]
            ]
        ]
    ]

let toolbar (model: Model) (dispatch: Msg -> unit) =
    let idInvalid = not (Project.isValidId model.Project.Id)

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
                                        Html.a [ prop.text "Lexicon" ]
                                    ]
                                ]
                                Bulma.tab [
                                    prop.children [
                                        Html.a [ prop.text "Sintaxe" ]
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
                                        prop.text "Projeto:"
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
                                            (fun _ -> LoadProject model.Project.Id |> dispatch)
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
                                            (fun _ -> SaveProject model.Project |> dispatch)
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
                        color.hasTextWhite
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

    Html.body [ header; body; footer ]
