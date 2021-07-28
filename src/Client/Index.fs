module Index

open Elmish
open Fable.Remoting.Client
open Shared

Fable.Core.JsInterop.importAll "/style.css"

type Model = { RegularDefinitionString: string; TokenString: string; SimulationString: string; SimulatorOutputList: SimulatorOutput list }

type Msg =
    | SetRegularDefinitionString of string
    | SetTokenString of string
    | SetSimulationString of string
    | SetOutputSimulationString of string
    | GetSimulatorOutput of SimulatorOutput list
    | AddSimulatorOutput
    | AddedSimulatorOutput of SimulatorOutput

let simulatorOutputApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ISimulatorOutputListApi>

let init () : Model * Cmd<Msg> =
    let model = { RegularDefinitionString = ""; TokenString = ""; SimulationString = ""; SimulatorOutputList = [] }

    let cmd =
        Cmd.OfAsync.perform simulatorOutputApi.getSimulatorOutputList () GetSimulatorOutput

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinitionString value -> { model with RegularDefinitionString = value }, Cmd.none
    | SetTokenString value -> { model with TokenString = value }, Cmd.none
    | SetSimulationString value -> { model with SimulationString = value }, Cmd.none
    | GetSimulatorOutput simulatorOutputList -> { model with SimulatorOutputList = simulatorOutputList }, Cmd.none
    | AddSimulatorOutput ->
        let simulatorOutput = SimulatorOutput.create("","",1)

        let cmd =
            Cmd.OfAsync.perform simulatorOutputApi.addSimulatorOutput simulatorOutput AddedSimulatorOutput

        { model with RegularDefinitionString = "" }, cmd
    | AddedSimulatorOutput simulatorOutput ->
        { model with
              SimulatorOutputList = model.SimulatorOutputList @ [ simulatorOutput ] },
        Cmd.none

open Feliz
open Feliz.Bulma

let regularDefinitionBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.RegularDefinitionString
            prop.placeholder "Informe as expressões regulares"
            prop.onChange (fun x -> SetRegularDefinitionString x |> dispatch)
        ]
    ]

let tokensBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.TokenString
            prop.placeholder "Informe os tokens"
            prop.onChange (fun x -> SetTokenString x |> dispatch)
        ]
    ]

let simulatorBox (model: Model) (dispatch: Msg -> unit) =
    Html.div[
        Bulma.textarea [
            prop.rows 20
            prop.value model.SimulationString
            prop.placeholder "Insira o texto para simulação"
            prop.onChange (fun x -> SetSimulationString x |> dispatch)
        ]
    ]

let tableOutput (model: Model) (dispatch: Msg -> Unit) =
    Html.table [
        Html.thead [
            Html.tr [
                Html.th "Token"
                Html.th "Lexema"
                Html.th "Posição"
            ]
        ]
        Html.tbody [
            for simulatorOutput in model.SimulatorOutputList do
                Html.tr [
                    Html.td simulatorOutput.Token
                    Html.td simulatorOutput.Lexema
                    Html.td simulatorOutput.Posicao
                ]
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
                    Bulma.container [
                        Bulma.button.a [
                            color.isDark
                            // prop.disabled (Todo.isValid model.Input |> not)
                            // prop.onClick (fun _ -> dispatch AddTodo)
                            prop.text "Analisador Léxico"
                        ]
                        tableOutput model dispatch
                    ]

                ]
            ]
        ]
    ]
