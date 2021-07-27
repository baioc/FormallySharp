module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

    let cmd =
        Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd =
            Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
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
        // prop.className "columns"
        // prop.children[
        // Html.div[
        //     prop.className "column"
        //     prop.text "1"
        // ]
        // Html.div[
        //     prop.className "column"
        //     prop.text "2"
        // ]
        // Html.div[
        //     prop.className "column"
        //     prop.text "3"
        // ]
        // ]
        Bulma.textarea [
            prop.value model.Input
            prop.placeholder "Informe as expressões regulares"
            prop.onChange (fun x -> SetInput x |> dispatch)
        ]
    ]

let tokensBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.value model.Input
            prop.placeholder "Informe os tokens"
            prop.onChange (fun x -> SetInput x |> dispatch)
        ]
    ]

let simulatorBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.value model.Input
            prop.placeholder "Insira o texto para simulação"
            prop.onChange (fun x -> SetInput x |> dispatch)
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        prop.style [
            style.backgroundColor "#08BDEA"
            style.backgroundSize "cover"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.columns[
                        Bulma.column [
                            prop.style [
                                style.backgroundColor "#1B87A2"
                            ]
                            prop.children [
                                Bulma.title [
                                    text.hasTextCentered
                                    prop.text "Definição Regular"
                                ]
                                regularDefinitionBox model dispatch
                            ]
                        ]
                        Bulma.column [
                            prop.style [
                                    style.backgroundColor "#1B87A2"
                            ]
                            prop.children [
                                Bulma.title [
                                    text.hasTextCentered
                                    prop.text "Tokens"
                                ]
                                tokensBox model dispatch
                            ]
                        ]
                        Bulma.column [
                            prop.style [
                                    style.backgroundColor "#1B87A2"
                            ]
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
                ]
            ]
        ]
    ]
