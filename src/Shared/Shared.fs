namespace Shared

open System

type Input = 
    { RegularDefinition: String
      Token: String
      Simulation: String }

module Input =
    let create (regularDefinition: String, token: String, simulation: string) = 
        { RegularDefinition = regularDefinition
          Token = token
          Simulation = simulation }

type Output =
    { Token: String
      Lexema: String
      Posicao: int }

module Output =
    let create (token: String, lexema: String, posicao: int) =
        { Token = token
          Lexema = lexema
          Posicao = posicao }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IApi =
    { getOutputs: unit -> Async<Output list>
      addOutput: Output -> Async<Output> 
      setInput: Input -> Async<Output list> }
