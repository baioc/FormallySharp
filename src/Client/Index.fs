module Index

open Elmish
open Fable.Remoting.Client
open Elmish.SweetAlert
open Feliz
open Feliz.Bulma

open Shared

Fable.Core.JsInterop.importAll "./style.css"


type Model =
    { Project: Project
      RegularDefinitionText: string * string * string // kind, name, regexp
      Lexer: Lexer option
      SymbolTable: TokenInstance seq
      InputText: string }

type Msg =
    | SetRegularDefinitionText of string * string * string
    | ChangeRegularDefinitions of LexicalSpecification
    | GenerateLexer of LexicalSpecification
    | GeneratedLexer of Lexer
    | SetInputText of string
    | SetProjectIdText of string
    | SaveProject of Project
    | SavedProject of Identifier
    | LoadProject of Identifier
    | LoadedProject of Project

let api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<FormallySharp>

// TODO
let init () : Model * Cmd<Msg> =
    let emptyProject =
        { Id = ""
          Lexer =
              Map.ofSeq [ "ALPHA", Fragment(UserRegexp "[a-zA-Z_]")
                          "ID", Token(UserRegexp "x", 0)
                          "WS", Separator(UserRegexp " \n\t") ] }

    let model =
        { Project = emptyProject
          Lexer = None
          SymbolTable = [| { Token = "ID"; Lexeme = "x"; Position = 0u }
                           { Token = "LET"; Lexeme = "let"; Position = 16u } |]
          RegularDefinitionText = "token", "", ""
          InputText = "" }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinitionText (kind, name, body) ->
        { model with
              RegularDefinitionText = kind, name, body },
        Cmd.none

    | ChangeRegularDefinitions lexer ->
        { model with
              Project = { model.Project with Lexer = lexer } },
        Cmd.none

    | GenerateLexer spec ->
        let toastAlert =
            ToastAlert("gerando lexer...")
                .Position(AlertPosition.Center)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        Cmd.batch [
            Cmd.OfAsync.perform api.generateLexer spec GeneratedLexer
            SweetAlert.Run(toastAlert)
        ]

    | GeneratedLexer lexer ->
        let toastAlert =
            ToastAlert("lexer gerado com sucesso!")
                .Position(AlertPosition.Center)
                .ConfirmButton(true)
                .Timeout(3000)
                .Type(AlertType.Success)

        { model with Lexer = Some lexer },
        SweetAlert.Run(toastAlert)

    | SetInputText text ->
        { model with
              InputText = text
              SymbolTable =
                  match model.Lexer with
                  | None -> model.SymbolTable
                  | Some lexer -> Lexer.tokenize lexer text },
        Cmd.none

    | SetProjectIdText id ->
        { model with
              Project = { model.Project with Id = id } },
        Cmd.none

    | SaveProject project ->
        let toastAlert =
            ToastAlert(sprintf "salvando projeto para \"%s\"..." project.Id)
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        Cmd.batch [
            Cmd.OfAsync.perform api.saveProject project (fun _ -> SavedProject project.Id)
            SweetAlert.Run(toastAlert)
        ]

    | SavedProject id ->
        let toastAlert =
            ToastAlert(sprintf "projeto para \"%s\" foi salvo" id)
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(false)
                .Timeout(3000)
                .Type(AlertType.Success)

        model,
        SweetAlert.Run(toastAlert)

    | LoadProject id ->
        let toastAlert =
            ToastAlert(sprintf "carregando projeto para \"%s\"..." id)
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        Cmd.batch [
            Cmd.OfAsync.perform api.loadProject id LoadedProject
            SweetAlert.Run(toastAlert)
        ]

    | LoadedProject project ->
        let toastAlert =
            ToastAlert(sprintf "projeto para \"%s\" foi carregado" project.Id)
                .Position(AlertPosition.Top)
                .ConfirmButton(true)
                .Timeout(5000)
                .Type(AlertType.Success)

        { model with Project = project },
        SweetAlert.Run(toastAlert)


let project (spec: LexicalSpecification) (kind, name, body) (dispatch: Msg -> unit) =
    // kinds of regular definitions
    let tokenOption = "token"
    let fragmentOption = "fragmento"
    let separatorOption = "separador"

    let regexp = Regexp.tryParse body
    let nameIsValid = Identifier.isValid name
    let regexpIsValid = Option.isSome regexp

    // normalized priority of each regular definition
    let priorities =
        Map.toSeq spec
        |> Seq.map
            (fun (name, def) ->
                match def with
                | Fragment _ -> -2, (name, def)
                | Separator _ -> -1, (name, def)
                | Token (_, p) -> p, (name, def)) // always >= 0
        |> Seq.sortBy fst
        |> Seq.mapi (fun newPrio (_, (name, def)) -> name, (def, newPrio))
        |> Map.ofSeq

    let maxPriority = Map.count priorities

    // build a new set of orderd regular definitions while moving (or inserting) a token
    let moveToken name regexp delta =
        let priority =
            Map.tryFind name priorities
            |> Option.map snd
            |> Option.defaultValue 0
        let priority = priority + delta
        let lower, higher =
            // remove the token we're going to move
            Map.toSeq priorities
            |> Seq.filter (fun (other, (_, _)) -> other <> name)
            // then partition regular definitions based on priority
            |> Seq.sortBy (fun (_, (_, p)) -> p)
            |> Seq.toArray
            |> Array.partition
                (fun (_, (_, p)) -> if delta > 0 then p <= priority else p < priority)
        // add up the arrays, with the moved token in the middle
        Array.concat [ lower; [| name, (Token(regexp, priority), priority) |]; higher ]
        // re-evaluate priorities based on their resulting index
        |> Seq.mapi
            (fun priority (name, (def, _)) ->
                match def with
                | Token (regexp, _) -> name, Token(regexp, priority)
                | def -> name, def)
        |> Map.ofSeq

    let addRegularDefinitionButton =
        let buttonEnabled = nameIsValid && regexpIsValid
        let willOverwrite = Map.containsKey name spec
        Bulma.button.a [
            prop.text (if not willOverwrite then "adicionar" else sprintf "editar <%s>" name)
            prop.disabled (not buttonEnabled)
            prop.onClick
                (fun _ ->
                    let regexp = UserRegexp(body, Option.get regexp)
                    // when editing a token, we want to maintain existing priority
                    if kind = tokenOption then
                        match Map.tryFind name spec with
                        | Some (Token (_, priority)) ->
                            let token = Token(regexp, priority)
                            Map.add name token spec
                        | _ -> moveToken name regexp 0
                        |> ChangeRegularDefinitions
                        |> dispatch
                    // otherwise, just put the definition in the lex spec
                    elif kind = fragmentOption then
                        Fragment regexp
                        |> fun def -> Map.add name def spec
                        |> ChangeRegularDefinitions
                        |> dispatch
                    elif kind = separatorOption then
                        Separator regexp
                        |> fun def -> Map.add name def spec
                        |> ChangeRegularDefinitions
                        |> dispatch)
            if willOverwrite then color.isWarning else color.isSuccess
        ]

    let viewRegularDefinition name def =
        let kind, (regexp: UserRegexp) =
            match def with
            | Token (regexp, _) -> tokenOption, regexp
            | Fragment regexp -> fragmentOption, regexp
            | Separator regexp -> separatorOption, regexp

        Bulma.columns [
            columns.isVCentered
            columns.isMobile
            columns.isMultiline
            prop.children [
                // type
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
                // name
                Bulma.column [
                    column.isOneFifthTablet
                    prop.children [
                        Bulma.text.p [
                            prop.text (name: string)
                            text.isFamilyCode
                            color.hasTextInfo
                        ]
                    ]
                ]
                // user-facing regex string
                Bulma.column [
                    column.isThreeFifthsMobile
                    prop.children [
                        Bulma.text.p [
                            prop.text (string regexp)
                            text.isFamilyCode
                        ]
                    ]
                ]
                // priority buttons, for tokens only
                match def with
                | Token (regexp, _) ->
                    Bulma.column [
                        column.isNarrow
                        prop.children [
                            Bulma.container [
                                Bulma.icon [
                                    icon.isSmall
                                    prop.children [
                                        Html.a [
                                            prop.className "fas fa-angle-up"
                                            prop.onClick
                                                (fun _ ->
                                                    moveToken name regexp +1
                                                    |> ChangeRegularDefinitions
                                                    |> dispatch)
                                        ]
                                    ]
                                ]
                                Bulma.icon [
                                    icon.isSmall
                                    prop.children [
                                        Html.a [
                                            prop.className "fas fa-angle-down"
                                            prop.onClick
                                                (fun _ ->
                                                    moveToken name regexp -1
                                                    |> ChangeRegularDefinitions
                                                    |> dispatch)
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                | _ -> ()
                // remove button
                Bulma.column [
                    column.isNarrow
                    prop.children [
                        Bulma.delete [
                            prop.onClick
                                (fun _ ->
                                    Map.remove name spec
                                    |> ChangeRegularDefinitions
                                    |> dispatch)
                        ]
                    ]
                ]
            ]
        ]

    // when displaying, we want fragments, separators, then tokens by descending priority
    let displayOrder =
        Map.toSeq priorities
        |> Seq.sortBy
            (fun (_, (def, prio)) ->
                match def with
                | Fragment _ -> -2
                | Separator _ -> -1
                | Token _ -> maxPriority - prio)
        |> Seq.map (fun (name, (def, _)) -> name, def)

    Bulma.block [
        // existing regular definitions
        Bulma.content [
            for name, def in displayOrder do
                viewRegularDefinition name def
        ]
        // partially-filled regular definition fields
        Bulma.columns [
            columns.isMobile
            columns.isMultiline
            prop.children [
                Bulma.column [
                    column.isNarrow
                    prop.children [
                        Bulma.select [
                            prop.children [
                                Html.option tokenOption
                                Html.option fragmentOption
                                Html.option separatorOption
                            ]
                            prop.value kind
                            prop.onChange
                                (fun kind ->
                                    (kind, name, body)
                                    |> SetRegularDefinitionText
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
                            prop.onChange
                                (fun name ->
                                    (kind, name, body)
                                    |> SetRegularDefinitionText
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
                                    |> SetRegularDefinitionText
                                    |> dispatch)
                            if not regexpIsValid then color.isDanger
                        ]
                    ]
                ]
                Bulma.column [
                    column.isNarrow
                    prop.children [addRegularDefinitionButton]
                ]
            ]
        ]
        // lexer generation button
        Bulma.level [
            Bulma.levelItem [
                Bulma.button.button [
                    prop.text "Gerar Analisador Léxico"
                    prop.disabled (Map.isEmpty spec)
                    prop.onClick (fun _ -> GenerateLexer spec |> dispatch)
                    button.isLarge
                    if Map.isEmpty spec then color.isWarning else color.isPrimary
                ]
            ]
        ]
    ]

let recognition (lexer: Lexer option) (symbolTable: TokenInstance seq) (dispatch: Msg -> unit) =
    Bulma.columns [
        // input
        Bulma.column [
            Bulma.textarea [
                prop.onChange (SetInputText >> dispatch)
                prop.disabled (Option.isNone lexer)
                prop.placeholder (if Option.isSome lexer then "Forneça uma entrada ao lexer."
                                  else "O lexer ainda não foi gerado.")
            ]
        ]
        // symbol table
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
                        for entry in symbolTable do
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
            Bulma.cardContent [ project model.Project.Lexer model.RegularDefinitionText dispatch ]
        ]

    let recognitionInterface =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Reconhecimento" ]
            Bulma.cardContent [ recognition model.Lexer model.SymbolTable dispatch ]
        ]

    Bulma.tile [
        tile.isAncestor
        prop.children [
            // project interface
            Bulma.tile [
                tile.isParent
                prop.children [
                    Bulma.tile [
                        tile.isChild
                        prop.children [ projectInterface ]
                    ]
                ]
            ]
            // exection interface
            Bulma.tile [
                tile.isParent
                prop.children [
                    // recognition test
                    Bulma.tile [
                        tile.isChild
                        prop.children [ recognitionInterface ]
                    ]
                ]
            ]
        ]
    ]

let toolbar (project: Project) (dispatch: Msg -> unit) =
    let idInvalid = not (Identifier.isValid project.Id)

    Bulma.level [
        prop.children [
            Bulma.levelLeft [
                Bulma.levelItem [
                    // project mode selector
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
                    // project save/load interface
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
                                        prop.value project.Id
                                        prop.onTextChange (SetProjectIdText >> dispatch)
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
                                        prop.onClick (fun _ -> LoadProject project.Id |> dispatch)
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
                                        prop.onClick (fun _ -> SaveProject project |> dispatch)
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
                    prop.children [ toolbar model.Project dispatch ]
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
