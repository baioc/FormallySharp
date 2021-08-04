namespace Formally.Converter

open Shared
open Formally.Regular

// let api =
//     Remoting.createApi ()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<IApi>

module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinition: string) = 
        Ok()
        // for c in regularDefinition do
        //     if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
        //         api.addRegularDefinition(c)

            