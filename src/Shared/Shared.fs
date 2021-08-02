/// Miscellaneous stuff that is used by both Client and Server
namespace Shared


// defines all API endpoints
module Route =
    let builder typeName methodName = $"/api/{typeName}/{methodName}"


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

type IApi =
    { getOutputs: unit -> Async<Output list>
      addOutput: Output -> Async<Output> 
      setInput: Input -> Async<Output list> }
