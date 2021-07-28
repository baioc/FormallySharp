namespace Shared

open System

type SimulatorOutput =
    { Token: String
      Lexema: String
      Posicao: int }

module SimulatorOutput =
    let create (token: String, lexema: String, posicao: int) =
        { Token = token
          Lexema = lexema
          Posicao = posicao }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ISimulatorOutputListApi =
    { getSimulatorOutputList: unit -> Async<SimulatorOutput list>
      addSimulatorOutput: SimulatorOutput -> Async<SimulatorOutput> }
