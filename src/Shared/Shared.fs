/// Miscellaneous stuff that is used by both Client and Server
namespace Shared


// defines all API endpoints
module Route =
    let builder typeName methodName = $"/api/{typeName}/{methodName}"


open System

type Todo = { Id: Guid; Description: string }

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }
