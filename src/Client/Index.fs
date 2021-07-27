module Index

open Elmish
open Fable.Remoting.Client
open Shared

Fable.Core.JsInterop.importAll "/style.css"

type Model = { Todos: Todo list; regularDefinitionString: string; tokenString: string; simulationString: string }

type Msg =
    | GotTodos of Todo list
    | SetRegularDefinitionString of string
    | SetTokenString of string
    | SetSimulationString of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; regularDefinitionString = ""; tokenString = ""; simulationString = ""}

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetRegularDefinitionString value -> { model with regularDefinitionString = value }, Cmd.none
    | SetTokenString value -> { model with tokenString = value }, Cmd.none
    | SetSimulationString value -> { model with simulationString = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.regularDefinitionString

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with regularDefinitionString = "" }, cmd
    | AddedTodo todo ->
        { model with
              Todos = model.Todos @ [ todo ] },
        Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let regularDefinitionBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.regularDefinitionString
            prop.placeholder "Informe as expressões regulares"
            prop.onChange (fun x -> SetRegularDefinitionString x |> dispatch)
        ]
    ]

let tokensBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.tokenString
            prop.placeholder "Informe os tokens"
            prop.onChange (fun x -> SetTokenString x |> dispatch)
        ]
    ]

let simulatorBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.simulationString
            prop.placeholder "Insira o texto para simulação"
            prop.onChange (fun x -> SetSimulationString x |> dispatch)
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.columns[
                        Bulma.column [
                            prop.className "column-odd"
                            prop.children [
                                Bulma.title [
                                    text.hasTextCentered
                                    prop.text "Definição Regular"
                                ]
                                regularDefinitionBox model dispatch
                            ]
                        ]
                        Bulma.column [
                            prop.className "column-odd"
                            prop.children [
                                Bulma.title [
                                    text.hasTextCentered
                                    prop.text "Tokens"
                                ]
                                tokensBox model dispatch
                            ]
                        ]
                        Bulma.column [
                            prop.className "column-odd"
                            prop.children [
                                Bulma.title [
                                    text.hasTextCentered
                                    prop.text "Simulador"
                                ]
                                simulatorBox model dispatch
                            ]
                        ]
                    ]
                    Html.hr[]
                    Bulma.button.a [
                        color.isDark
                        // prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Analisador Léxico"
                    ]
                    Bulma.table [
                        table.isFullWidth
                    ]
                ]
            ]
        ]
    ]
