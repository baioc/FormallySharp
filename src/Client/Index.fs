module Index

open System.Text.RegularExpressions

open Elmish
open Fable.Remoting.Client
open Elmish.SweetAlert
open Feliz
open Feliz.Bulma

open Shared
open Formally.Regular
open Formally.ContextFree
open Formally.Converter

Fable.Core.JsInterop.importAll "./style.css"


type Phase =
    | Lexical
    | Syntactical

type Model = {
    // domain-related state, everything else stems from this or is for the UI.
    // in other words: if this was an online shop, this would be the cart
    Project: Project // stores project identifier text

    // shared between analysis phases
    Phase: Phase
    InputText: string

    // lexing-related state
    Lexer: Lexer option
    RegularDefinitionText: string * string * string // inputs: kind, name, regex
    SymbolTable: Result<TokenInstance, LexicalError> seq

    // parsing-related state
    GrammarProductionText: string * string // inputs: head, body
}

type Msg =
    // project-related messages
    | SetProjectIdText of string
    | SaveProject of Project
    | SavedProject of Identifier
    | LoadProject of Identifier
    | LoadedProject of Project
    // messages across phases
    | SetPhase of Phase
    | SetInputText of string
    | GotError of exn
    // regular grammar messages
    | SetRegularDefinitionText of string * string * string
    | ChangeRegularDefinitions of LexicalSpecification
    | GenerateLexer of LexicalSpecification
    | GeneratedLexer of Lexer
    // context-free gramamr messages
    | SetGrammarProductionText of string * string
    | ChangeGrammarProductions of Grammar

let api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<FormallySharp>

let init () : Model * Cmd<Msg> =
    // TODO: clear "empty" project on release
    let emptyProject =
        { Id = ""
          Lexicon = Map.ofSeq [
              "EQUALS", TokenClass(UserRegexp(@"=", Regexp.singleton '='), 8)
              "BINARY_OP", TokenClass(UserRegexp(@"[+-*/^]", Regexp.ofSet "+-*/^"), 8)
              "IDENTIFIER", TokenClass(UserRegexp(@"[a-zA-Z_]+", Regexp.ofSet ([ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '_' ]) |> Regexp.many), 6)
              "LET", TokenClass(UserRegexp(@"let", Regexp.ofSeq "let"), 9)
              "WHITESPACE", Separator <| UserRegexp(@"\s", Regexp.ofSet " \t\n")
              "NUMBER", TokenClass(UserRegexp(@"[0-9]+", Regexp.ofSet [ '0' .. '9' ] |> Regexp.many), 5)
              "SEMICOLON", TokenClass(UserRegexp(@";", Regexp.singleton ';'), 7)
          ]
          Syntax =
              { Initial = "POLY"
                Rules = set [
                    "POLY", [ Terminal "LET"; Terminal "IDENTIFIER"; Terminal "EQUALS"; NonTerminal "EXPR"; Terminal "SEMICOLON" ]
                    "POLY", []
                    "EXPR", [ NonTerminal "NUMBER" ]
                    "EXPR", [ NonTerminal "IDENTIFIER" ]
                    "EXPR", [ NonTerminal "EXPR"; Terminal "BINARY_OP"; NonTerminal "EXPR" ]
                ] } }

    let model =
        { Project = emptyProject
          Phase = Lexical
          InputText = ""
          Lexer = None
          RegularDefinitionText = "token", "", ""
          SymbolTable = []
          GrammarProductionText = "", "" }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
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
            SweetAlert.Run(toastAlert)
            Cmd.OfAsync.either api.saveProject project (fun () -> SavedProject project.Id) GotError
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
            SweetAlert.Run(toastAlert)
            Cmd.OfAsync.either api.loadProject id LoadedProject GotError
        ]

    | LoadedProject project ->
        let toastAlert =
            ToastAlert(sprintf "projeto para \"%s\" foi carregado" project.Id)
                .Position(AlertPosition.TopEnd)
                .ConfirmButton(true)
                .Timeout(5000)
                .Type(AlertType.Success)

        { model with Project = project },
        SweetAlert.Run(toastAlert)

    | SetPhase phase ->
        { model with Phase = phase }, Cmd.none

    | SetInputText text ->
        { model with
              InputText = text
              SymbolTable =
                  match model.Lexer with
                  | None -> model.SymbolTable
                  | Some lexer -> Lexer.tokenize lexer text },
        Cmd.none

    | GotError ex ->
        let toastAlert =
#if DEBUG   // during development, print the specific API call that failed
            ToastAlert(string ex.Message)
#else       // on release mode, we have a generic error message for users
            ToastAlert("erro ao efetuar operação")
#endif
                .Position(AlertPosition.Top)
                .ConfirmButton(true)
                .Timeout(13000)
                .Type(AlertType.Error)

        model,
        SweetAlert.Run(toastAlert)

    | SetRegularDefinitionText (kind, name, body) ->
        { model with
              RegularDefinitionText = kind, name, body },
        Cmd.none

    | ChangeRegularDefinitions lexicon ->
        { model with
              Project = { model.Project with Lexicon = lexicon } },
        Cmd.none

    | GenerateLexer spec ->
        let toastAlert =
            ToastAlert("gerando analisador léxico...")
                .Position(AlertPosition.Center)
                .ConfirmButton(false)
                .Type(AlertType.Info)

        model,
        Cmd.batch [
            SweetAlert.Run(toastAlert)
            Cmd.OfAsync.either api.generateLexer spec GeneratedLexer GotError
        ]

    | GeneratedLexer lexer ->
        let toastAlert =
            ToastAlert("analisador léxico gerado")
                .Position(AlertPosition.Center)
                .ConfirmButton(true)
                .Timeout(3000)
                .Type(AlertType.Success)

        { model with
              Lexer = Some lexer
              SymbolTable = Lexer.tokenize lexer model.InputText },
        SweetAlert.Run(toastAlert)

    | SetGrammarProductionText (head, body) ->
        { model with
              GrammarProductionText = head, body },
        Cmd.none

    | ChangeGrammarProductions syntax ->
        { model with
              Project = { model.Project with Syntax = syntax } },
        Cmd.none


let projectSyntactical grammar lexSpec (head: string, body: string) dispatch =
    let sprintSymbol =
        function
        | Terminal s -> s
        | NonTerminal n -> sprintf "<%s>" n

    let viewProductionRule (head, body) =
        Bulma.columns [
            columns.isVCentered
            columns.isMobile
            columns.isMultiline
            columns.isCentered
            prop.children [
                // head
                Bulma.column [
                    column.isOneFifthTablet
                    prop.style [ style.paddingRight (length.rem 0.5) ]
                    prop.children [
                        Bulma.text.p [
                            prop.text (sprintf "<%s>" head)
                            text.isFamilyCode
                            color.hasTextLink
                        ]
                    ]
                ]
                // body
                Bulma.column [
                    prop.style [ style.padding (length.rem 0.5) ]
                    prop.children [
                        Bulma.text.p [
                            prop.text
                                (match body with
                                | [] -> "\u03B5" // unicode for lowercase epsilon
                                | symbols ->
                                    symbols
                                    |> Seq.map sprintSymbol
                                    |> String.concat " ")
                            text.isFamilyCode
                        ]
                    ]
                ]
                // toggle initial
                Bulma.column [
                    column.isNarrow
                    prop.children [
                        Bulma.input.radio [
                            prop.value "init"
                            prop.isChecked (grammar.Initial = head)
                            prop.onClick
                                (fun _ ->
                                    { grammar with Initial = head }
                                    |> ChangeGrammarProductions
                                    |> dispatch)
                            prop.style [
                                style.paddingRight (length.rem 0)
                                style.paddingLeft (length.rem 0.5)
                            ]
                        ]
                    ]
                ]
                // remove button
                Bulma.column [
                    column.isNarrow
                    prop.style [ style.paddingLeft (length.rem 0) ]
                    prop.children [
                        Bulma.delete [
                            // we can't remove all initial productions
                            prop.disabled
                                (grammar.Initial = head
                                && grammar.Rules
                                   |> Set.filter (fst >> (=) grammar.Initial)
                                   |> Set.count <= 1)
                            prop.onClick
                                (fun _ ->
                                    { grammar with
                                          Rules = Set.remove (head, body) grammar.Rules }
                                    |> ChangeGrammarProductions
                                    |> dispatch)
                        ]
                    ]
                ]
            ]
        ]

    let nonTerminals = grammar.NonTerminals
    let nonTerminalRegex = sprintf "^<%s>$" Identifier.regex

    let productionHead =
        let head = head.Trim()
        if Regex.IsMatch(head, nonTerminalRegex) then
            Some (head.Substring(1, head.Length - 2))
        else
            None

    let productionBody =
        let body = body.Trim()
        if body = "&" then
            Some []
        else
            let parts =
                Regex.Split(body, @"\s+")
                |> Seq.map
                    (fun symbol ->
                        // a terminal symbol should refer to a token in the lexical spec
                        // FIXME: removing a token doesn't invalidate grammar rules
                        match Map.tryFind symbol lexSpec with
                        | Some (TokenClass _) -> Some (Terminal symbol)
                        // non-terminals must already exist or refer to themselves
                        // FIXME: removing a non-terminal doesn't invalidate others
                        | _ ->
                            if not <| Regex.IsMatch(symbol, nonTerminalRegex) then
                                None
                            else
                                let symbol = symbol.Substring(1, symbol.Length - 2)
                                if Set.contains symbol nonTerminals
                                   || (Option.isSome productionHead && symbol = Option.get productionHead) then
                                    Some (NonTerminal symbol)
                                else
                                    None)
            try
                parts
                |> Seq.map Option.get
                |> Seq.toList
                |> Some
            with
                | error -> None

    let addProductionRuleButton =
        let buttonEnabled = Option.isSome productionHead && Option.isSome productionBody
        Bulma.button.a [
            prop.text "adicionar"
            prop.disabled (not buttonEnabled)
            prop.onClick
                (fun _ ->
                    let head, body = Option.get productionHead, Option.get productionBody
                    { grammar with
                          Initial = if grammar.Initial = "" then head else grammar.Initial
                          Rules = Set.add (head, body) grammar.Rules }
                    |> ChangeGrammarProductions
                    |> dispatch)
            color.isSuccess
        ]

    Bulma.block [
        // existing productions
        Html.ol [
            prop.style [ style.paddingLeft (length.rem 1.0) ]
            prop.children [
                for rule in (Seq.sort grammar.Rules) do
                    Html.li [ viewProductionRule rule ]
            ]
        ]
        // partially-filled rule fields
        Bulma.columns [
            columns.isMobile
            columns.isMultiline
            columns.isCentered
            prop.children [
                Bulma.column [
                    column.isOneFifthDesktop
                    prop.style [ style.paddingRight (length.rem 0.5) ]
                    prop.children [
                        Bulma.input.text [
                            prop.value head
                            prop.placeholder "<regra>"
                            prop.onChange
                                (fun head ->
                                    (head, body)
                                    |> SetGrammarProductionText
                                    |> dispatch)
                            if Option.isNone productionHead then color.isDanger
                            text.isFamilyMonospace
                        ]
                    ]
                ]
                Bulma.column [
                    column.isHalfMobile
                    prop.style [ style.paddingLeft (length.rem 0.5) ]
                    prop.children [
                        Bulma.input.text [
                            prop.value body
                            prop.placeholder "corpo da <regra>"
                            prop.onChange
                                (fun body ->
                                    (head, body)
                                    |> SetGrammarProductionText
                                    |> dispatch)
                            if Option.isNone productionBody then color.isDanger
                        ]
                    ]
                ]
                Bulma.column [
                    column.isNarrow
                    prop.style [ style.paddingLeft (length.rem 0) ]
                    prop.children [ addProductionRuleButton ]
                ]
            ]
        ]
        // parser generation button
        Bulma.level [
            Bulma.levelItem [
                Bulma.button.button [
                    prop.text "Gerar Analisador Sintático"
                    // TODO: prop.onClick
                    button.isLarge
                    color.isPrimary
                ]
            ]
        ]
    ]

let projectLexical spec lexer (kind, name, body) dispatch =
    // kinds of regular definitions
    let tokenOption = "token"
    let fragmentOption = "fragmento"
    let separatorOption = "separador"

    let regexp =
        if kind = fragmentOption then
            Regexp.tryParse body
        else
            // make a map of (name -> regex fragment) for inlining in other definitions
            let fragments =
                Map.toSeq spec
                |> Seq.choose
                    (fun (name, def) ->
                        match def with
                        | Fragment r -> Some (name, r.String)
                        | notFragment -> None)
                |> Map.ofSeq
            // inline fragments (if any), then parse the regex
            // FIXME: changing a fragment doesn't change regexps it is included in
            Converter.convertTokenToRegexString(body, fragments)
            |> Regexp.tryParse

    let nameIsValid = Identifier.isValid name
    let regexIsValid = Option.isSome regexp

    // normalized priority of each regular definition
    let priorities =
        Map.toSeq spec
        |> Seq.map
            (fun (name, def) ->
                match def with
                | Fragment r -> -2, (name, def)
                | Separator r -> -1, (name, def)
                | TokenClass (r, prio) -> prio, (name, def)) // always >= 0
        |> Seq.sortBy fst
        |> Seq.mapi (fun newPrio (_, (name, def)) -> name, (def, newPrio))
        |> Map.ofSeq

    let maxPriority = Map.count priorities

    // build a new set of ordered regular definitions while moving (or inserting) a token
    let moveToken name regexp delta =
        let priority =
            Map.tryFind name priorities
            |> Option.map snd
            |> Option.defaultValue 0
        let priority = priority + delta
        let lower, higher =
            // remove the token we're going to move
            Map.toSeq priorities
            |> Seq.filter (fun (other, _) -> other <> name)
            // then partition regular definitions based on priority
            |> Seq.sortBy (fun (_, (def, p)) -> p)
            |> Seq.toArray
            |> Array.partition
                (fun (_, (def, p)) ->
                    if delta > 0 then p <= priority else p < priority)
        // add up the arrays, with the moved token in the middle
        Array.concat [ lower; [| name, (TokenClass (regexp, priority), priority) |]; higher ]
        // re-evaluate priorities based on their resulting index
        |> Seq.mapi
            (fun priority (name, (def, _)) ->
                match def with
                | TokenClass (regexp, _) -> name, TokenClass (regexp, priority)
                | def -> name, def)
        |> Map.ofSeq

    let addRegularDefinitionButton =
        let buttonEnabled = nameIsValid && regexIsValid
        let willOverwrite = Map.containsKey name spec
        Bulma.button.a [
            prop.text (if not willOverwrite then "adicionar" else sprintf "editar %s" name)
            prop.disabled (not buttonEnabled)
            prop.onClick
                (fun _ ->
                    let regexp = UserRegexp(body, Option.get regexp)
                    // when editing a token, we want to maintain existing priority
                    if kind = tokenOption then
                        match Map.tryFind name spec with
                        | Some (TokenClass (previous, priority)) ->
                            let token = TokenClass (regexp, priority)
                            Map.add name token spec
                        | _ ->
                            moveToken name regexp 0 // we're actually inserting a new one
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
        let kind, (userRegexp: UserRegexp) =
            match def with
            | TokenClass (regexp, priority) -> tokenOption, regexp
            | Fragment regexp -> fragmentOption, regexp
            | Separator regexp -> separatorOption, regexp

        Bulma.columns [
            columns.isVCentered
            columns.isMobile
            columns.isMultiline
            columns.isCentered
            prop.children [
                // type
                Bulma.column [
                    column.isOneFifthTablet
                    prop.style [ style.paddingRight (length.rem 0.5) ]
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
                    prop.style [ style.padding (length.rem 0.0) ]
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
                    prop.style [ style.padding (length.rem 0.5) ]
                    prop.children [
                        Bulma.text.p [
#if DEBUG                   // during development, show Regexp tree
                            prop.text (string userRegexp.Regexp)
#else                       // on release, show user-facing regex
                            prop.text (String.visual userRegexp.String)
#endif
                            text.isFamilyCode
                        ]
                    ]
                ]
                // priority buttons, for tokens only
                match def with
                | TokenClass (regexp, priority) ->
                    Bulma.column [
                        column.isNarrow
                        prop.children [
                            Bulma.container [
                                Bulma.icon [
                                    icon.isSmall
                                    prop.style [
                                        style.paddingRight (length.rem 0)
                                        style.paddingLeft (length.rem 0.5)
                                    ]
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
                                    prop.style [ style.padding (length.rem 0) ]
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
                | notToken -> ()
                // remove button
                Bulma.column [
                    column.isNarrow
                    prop.style [ style.paddingLeft (length.rem 0) ]
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

    let viewSpec =
        // when displaying, we want fragments, separators, then tokens by descending priority
        let displayOrder =
            Map.toSeq priorities
            |> Seq.sortBy
                (fun (name, (def, prio)) ->
                    match def with
                    | Fragment _ -> -2
                    | Separator _ -> -1
                    | TokenClass _ -> maxPriority - prio)
            |> Seq.map (fun (name, (def, prio)) -> name, def)

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
                columns.isCentered
                prop.children [
                    Bulma.column [
                        column.isNarrow
                        prop.style [ style.paddingRight (length.rem 0) ]
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
                        column.isOneFifthDesktop
                        prop.style [ style.paddingRight (length.rem 0.5) ]
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
                                text.isFamilyMonospace
                            ]
                        ]
                    ]
                    Bulma.column [
                        column.isHalfMobile
                        prop.style [ style.paddingLeft (length.rem 0.5) ]
                        prop.children [
                            Bulma.input.text [
                                prop.value body
                                prop.placeholder "expressão regular"
                                prop.onChange
                                    (fun body ->
                                        (kind, name, body)
                                        |> SetRegularDefinitionText
                                        |> dispatch)
                                if not regexIsValid then color.isDanger
                                text.isFamilyMonospace
                            ]
                        ]
                    ]
                    Bulma.column [
                        column.isNarrow
                        prop.style [ style.paddingLeft (length.rem 0) ]
                        prop.children [ addRegularDefinitionButton ]
                    ]
                ]
            ]
            // lexer generation button
            Bulma.level [
                Bulma.levelItem [
                    Bulma.button.button [
                        prop.text "Gerar Analisador Léxico"
                        prop.onClick (fun _ -> GenerateLexer spec |> dispatch)
                        button.isLarge
                        color.isPrimary
                    ]
                ]
            ]
        ]

    let viewLexer (lexer: Lexer) =
        Bulma.tableContainer [
            Bulma.table [
                table.isFullWidth
                table.isHoverable
                table.isBordered
                table.isNarrow
                prop.children [
                    Html.thead [
                        Html.tr [
                            Html.th [
                                prop.text "Transições"
                                text.hasTextCentered
                            ]
                            for symbol in lexer.Automaton.Alphabet do
                                Html.th [
                                    prop.text (sprintf "%c" symbol |> String.visual)
                                ]
                        ]
                    ]
                    Html.tbody [
                        // we map states to numbers in order to avoid a huge table
                        let states =
                            lexer.Automaton.States
                            |> Set.remove lexer.Automaton.Dead
                            |> Set.toArray
                            |> Array.sort
                        let indexes =
                            states
                            |> Seq.mapi (fun i x -> (x, i))
                            |> Map.ofSeq
                        for state in states do
                            let prefix = ""
                            let prefix =
                                if Set.contains state lexer.Automaton.Accepting then
                                    "*" + prefix
                                else
                                    " " + prefix
                            let prefix =
                                if state = lexer.Automaton.Current then
                                    "-> " + prefix
                                else
                                    "   " + prefix
                            Html.tr [
                                Html.td [
                                    prop.text (sprintf "%s%d" prefix indexes.[state])
                                ]
                                for symbol in lexer.Automaton.Alphabet do
                                    let next = Map.tryFind (state, symbol) lexer.Automaton.Transitions
                                    Html.td [
                                        match next with
                                        | None ->
                                            prop.text "-"
                                        | Some next ->
                                            if next = lexer.Automaton.Dead then
                                                prop.text "-"
                                            else
                                                prop.text (sprintf "%d" indexes.[next])
                                    ]
                            ]
                    ]
                ]
            ]
        ]

    Bulma.columns [
        columns.isMultiline
        columns.isCentered
        prop.children [
            Bulma.column [
                column.isFull
                prop.children [ viewSpec ]
            ]
            match lexer with
            | None -> ()
            | Some lexer ->
                Bulma.column [
                    column.isFull
                    prop.children [ viewLexer lexer ]
                ]
        ]
    ]

let recognitionLexical lexer symbolTable dispatch =
    Bulma.columns [
        // input
        columns.isMobile
        columns.isMultiline
        columns.isCentered
        prop.children [
            Bulma.column [
                column.isHalfDesktop
                column.isFullTablet
                column.isFullMobile
                prop.children [
                    Bulma.textarea [
                        prop.custom ("rows", 24)
                        prop.onChange (SetInputText >> dispatch)
                        prop.disabled (Option.isNone lexer)
                        prop.placeholder
                            (if Option.isSome lexer then "Forneça uma entrada ao lexer."
                             else "O lexer ainda não foi gerado.")
                    ]
                ]
            ]
            // symbol table
            Bulma.column [
                column.isHalfDesktop
                column.isFullTablet
                column.isFullMobile
                prop.children [
                    Bulma.tableContainer [
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
                                        let kind, lexeme, position, isError =
                                            match entry with
                                            | Ok token ->
                                                token.Token, token.Lexeme, token.Position, false
                                            | Error error ->
                                                let pseudoLexeme =
                                                    error.String
                                                    |> Seq.map (sprintf "%c")
                                                    |> String.concat ""
                                                "ERRO LÉXICO", pseudoLexeme, error.Position, true
                                        Html.tr [
                                            Html.td [
                                                prop.text kind
                                                if isError then color.hasTextDanger
                                                else color.hasTextInfo
                                            ]
                                            Html.td [
                                                prop.text (String.visual lexeme)
                                            ]
                                            Html.td [
                                                prop.text (sprintf "%d" position)
                                                color.hasTextLink
                                            ]
                                        ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let main model dispatch =
    let cardTitle (title: string) =
        Bulma.cardHeaderTitle.p [
            prop.text title
            cardHeaderTitle.isCentered
            size.isSize4
            color.hasBackgroundGreyDark
            color.hasTextGreyLighter
        ]

    let projectInterface =
        match model.Phase with
        | Lexical ->
            Bulma.card [
                Bulma.cardHeader [ cardTitle "Especificação Léxica" ]
                Bulma.cardContent [
                    projectLexical
                        model.Project.Lexicon
                        model.Lexer
                        model.RegularDefinitionText
                        dispatch ]
            ]
        | Syntactical ->
            Bulma.card [
                Bulma.cardHeader [ cardTitle "Gramática" ]
                Bulma.cardContent [
                    projectSyntactical
                        model.Project.Syntax
                        model.Project.Lexicon
                        model.GrammarProductionText
                        dispatch
                    ]
            ]

    let recognitionInterface =
        Bulma.card [
            Bulma.cardHeader [ cardTitle "Reconhecimento" ]
            Bulma.cardContent [
                match model.Phase with
                | Lexical -> recognitionLexical model.Lexer model.SymbolTable dispatch
                | Syntactical -> () // TODO: recognitionSyntactical
            ]
        ]

    Bulma.columns [
        columns.isMultiline
        columns.isCentered
        prop.children [
            Bulma.column [
                column.isFull
                column.isHalfWidescreen
                prop.children [ projectInterface ]
            ]
            Bulma.column [
                column.isFull
                column.isHalfWidescreen
                prop.children [ recognitionInterface ]
            ]
        ]
    ]

let toolbar model dispatch =
    let idInvalid = not (Identifier.isValid model.Project.Id)

    Bulma.level [
        Bulma.levelLeft [
            // project mode selector
            Bulma.tabs [
                tabs.isBoxed
                prop.children [
                    Html.ul [
                        Bulma.tab [
                            match model.Phase with
                            | Lexical -> tab.isActive
                            | Syntactical -> ()
                            prop.children [
                                Html.a [
                                    prop.text "Lexicon"
                                    prop.onClick (fun _ -> SetPhase Lexical |> dispatch)
                                ]
                            ]
                        ]
                        Bulma.tab [
                            match model.Phase with
                            | Lexical -> ()
                            | Syntactical -> tab.isActive
                            prop.children [
                                Html.a [
                                    prop.text "Sintaxe"
                                    prop.onClick (fun _ -> SetPhase Syntactical |> dispatch)
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
                    columns.isCentered
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
                            column.isThreeFifthsMobile
                            prop.children [
                                Bulma.input.text [
                                    prop.value model.Project.Id
                                    prop.onTextChange (SetProjectIdText >> dispatch)
                                    prop.placeholder "identificador"
                                ]
                            ]
                        ]
                        Bulma.column [
                            column.isNarrowTablet
                            column.isOneQuarterMobile
                            prop.children [
                                Bulma.button.button [
                                    prop.text "abrir"
                                    prop.disabled idInvalid
                                    prop.onClick (fun _ -> LoadProject model.Project.Id |> dispatch)
                                    color.isDanger
                                ]
                            ]
                        ]
                        Bulma.column [
                            column.isNarrowTablet
                            column.isOneQuarterMobile
                            prop.children [
                                Bulma.button.button [
                                    prop.text "salvar"
                                    prop.disabled idInvalid
                                    prop.onClick (fun _ -> SaveProject model.Project |> dispatch)
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
                    Bulma.level [
                        Bulma.levelLeft [
                            Bulma.title [
                                prop.text "Formally#"
                                title.is1
                                color.hasTextWhite
                            ]
                        ]
                        Bulma.levelRight [
                            prop.style [ style.paddingLeft (length.rem 2) ]
                            prop.children [
                                Html.a [
                                    prop.text "Atenção! Clique aqui para ver as instruções de uso"
                                    prop.href "https://github.com/baioc/FormallySharp/wiki/Instruções"
                                    size.isSize4
                                ]
                            ]
                        ]
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
                Html.p "© 2021 Gabriel B. Sant'Anna, Marcelo P. G. Contin, João Vitor de S. Costa"
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
