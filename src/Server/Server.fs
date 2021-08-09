module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Giraffe

open Shared


let api =
    { generateLexer =
          fun spec ->
              async {
                  return failwith "TODO"
              }

      saveProject =
          fun project ->
              async {
                  return failwith "TODO"
              }

      loadProject =
          fun id ->
              async {
                  return failwith "TODO"
              }
    }


let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let routes = choose [ webApp ]

let app =
    application {
        url "http://0.0.0.0:8085"
        use_static "public"
        use_router routes
        use_gzip
        memory_cache
    }

run app
