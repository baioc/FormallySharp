namespace Shared

open System

type Todo = { Id: Guid; Description: string }

type SimulatorOutput = { Token : String; Lexema : String; Posicao: int }

module SimulatorOutput =
    let create (token : String, lexema : String, posicao: int) = 
        { Token = token
          Lexema = lexema
          Posicao = posicao}

// module Todo =
//     let isValid (description: string) =
//         String.IsNullOrWhiteSpace description |> not

//     let create (description: string) =
//         { Id = Guid.NewGuid()
//           Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

// type ITodosApi =
//     { getTodos: unit -> Async<Todo list>
//       addTodo: Todo -> Async<Todo> }

type ISimulatorOutputListApi = {
    getSimulatorOutputList: unit -> Async<SimulatorOutput list>
    addSimulatorOutput: SimulatorOutput -> Async<SimulatorOutput>
}
