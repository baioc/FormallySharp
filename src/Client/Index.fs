module Index

open Elmish
open Fable.Remoting.Client

open Shared
open Feliz
open Feliz.Bulma

Fable.Core.JsInterop.importAll "./style.css"


type Model =
    { RegularDefinitionText: string
      TokenText: string
      SimulationText: string
      Outputs: Output list }

type Msg =
    | SetRegularDefinitionText of string
    | SetTokenText of string
    | SetSimulationText of string
    | GetOutput of Output list
    | DoLexicalAnalysis
    | FinishedLexicalAnalysis of Output list

let api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IApi>

let init () : Model * Cmd<Msg> =
    let model =
        { RegularDefinitionText = ""
          TokenText = ""
          SimulationText = ""
          Outputs = [] }

    let cmd =
        // Cmd.OfAsync.perform api.getOutputs () GetOutput
        Cmd.none

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetRegularDefinitionText value ->
        { model with 
            RegularDefinitionText = value }, Cmd.none
    | SetTokenText value -> 
        { model with 
            TokenText = value }, Cmd.none
    | SetSimulationText value -> 
        { model with 
            SimulationText = value }, Cmd.none
    | GetOutput outputs ->
        { model with
              Outputs = outputs }, Cmd.none
    | DoLexicalAnalysis ->
        let input = Input.create(model.RegularDefinitionText, model.TokenText, model.SimulationText)
        let cmd = Cmd.OfAsync.perform api.setInput input FinishedLexicalAnalysis
        { model with
            RegularDefinitionText = "Retorno visual pra saber se foi" }, cmd
    | FinishedLexicalAnalysis outputs->
        { model with
              Outputs = outputs }, Cmd.none





// View
let regularDefinitionBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 20
            prop.value model.RegularDefinitionText
            prop.placeholder "Informe as expressões regulares"
            prop.onChange (fun x -> SetRegularDefinitionText x |> dispatch)
        ]
    ]

let tokensBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 20
            prop.value model.TokenText
            prop.placeholder "Informe os tokens"
            prop.onChange (fun x -> SetTokenText x |> dispatch)
        ]
    ]

let simulatorBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 20
            prop.value model.SimulationText
            prop.placeholder "Insira o texto para simulação"
            prop.onChange (fun x -> SetSimulationText x |> dispatch)
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
            for output in model.Outputs do
                Html.tr [
                    Html.td output.Token
                    Html.td output.Lexema
                    Html.td output.Posicao
                ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.columns [
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
                    Html.hr []
                    Bulma.container [
                        Bulma.button.a [
                            color.isDark
                            prop.text "Analisador Léxico"
                            prop.onClick (fun _ -> dispatch DoLexicalAnalysis)
                        ]
                        tableOutput model dispatch
                    ]

                ]

            ]

        ]

    ]
