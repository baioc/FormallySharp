/// Miscellaneous stuff that is used by both Client and Server
namespace Shared


// defines all API endpoints
module Route =
    let builder typeName methodName = $"/api/{typeName}/{methodName}"


open System

type Input = 
    { RegularDefinition: string
      Token: string
      Simulation: string }

module Input =
    let create (regularDefinition: string, token: string, simulation: string) = 
        { RegularDefinition = regularDefinition
          Token = token
          Simulation = simulation }

type Output =
    { Token: string
      Lexema: string
      Posicao: int }

module Output =
    let create (token: string, lexema: string, posicao: int) =
        { Token = token
          Lexema = lexema
          Posicao = posicao }

type IApi =
    { getOutputs: unit -> Async<Output list>
      addOutput: Output -> Async<Output> 
      setInput: Input -> Async<Output list>
      // getRegularDefinitions: unit -> string list
      // addRegularDefinition: string -> string 
    }
