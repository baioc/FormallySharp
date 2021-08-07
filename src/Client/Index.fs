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
      TokenKeyWord: string
      TokenIgnore: string
      SimulationText: string
      Outputs: Output list }

type Msg =
    | SetRegularDefinitionText of string
    | SetTokenText of string
    | SetTokenKeyWord of string
    | SetTokenIgnore of string
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
        { RegularDefinitionText = "L : [A-Za-z]\nD : [0-9]\nComment : <--[A-Za-z0-9 ]*-->"
          TokenText = "id : {L} ( {L} | {D} )*\nnum : {D}*"
          TokenKeyWord = "reservada : begin\nreservada : end\nreservada : if\nreservada : then\nreservada : while\nreservada : do\nreservada : write"
          TokenIgnore = ": {Comment}"
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
    | SetTokenKeyWord value -> 
        { model with 
            TokenKeyWord = value }, Cmd.none
    | SetTokenIgnore value -> 
        { model with 
            TokenIgnore = value }, Cmd.none
    | SetSimulationText value -> 
        { model with 
            SimulationText = value }, Cmd.none
    | GetOutput outputs ->
        { model with
              Outputs = outputs }, Cmd.none
    | DoLexicalAnalysis ->
        let input = Input.create(model.RegularDefinitionText, model.TokenText, model.TokenKeyWord, model.TokenIgnore, model.SimulationText)
        let cmd = Cmd.OfAsync.perform api.setInput input FinishedLexicalAnalysis
        {model with
              RegularDefinitionText = model.RegularDefinitionText}, cmd
    | FinishedLexicalAnalysis outputs->
        { model with
              Outputs = outputs }, Cmd.none





// View
let regularDefinitionBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 30
            prop.value model.RegularDefinitionText
            prop.placeholder "Informe as expressões regulares"
            prop.onChange (fun x -> SetRegularDefinitionText x |> dispatch)
        ]
    ]

let tokensBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 9
            prop.value model.TokenText
            prop.placeholder "Informe os tokens"
            prop.onChange (fun x -> SetTokenText x |> dispatch)
        ]
        Bulma.textarea [
            prop.className "tokenMiddle"
            prop.rows 9
            prop.value model.TokenKeyWord
            prop.placeholder "Informe as Palavras chave"
            prop.onChange (fun x -> SetTokenKeyWord x |> dispatch)
        ]
        Bulma.textarea [
            prop.rows 9
            prop.value model.TokenIgnore
            prop.placeholder "Informar campos a Ignorar"
            prop.onChange (fun x -> SetTokenIgnore x |> dispatch)
        ]
    ]

let simulatorBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        Bulma.textarea [
            prop.rows 30
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
