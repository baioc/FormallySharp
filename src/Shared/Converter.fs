namespace Formally.Converter

open Shared
open Formally.Regular

// let api =
//     Remoting.createApi ()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<IApi>

module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinitionsText: string) = 
        let mutable key = ""
        let mutable value = ""
        let mutable insertKey = true
        let regularExpressions = ResizeArray<char>()
        let regularDefinitionsTextWithoutSpaces = System.String.Concat(regularDefinitionsText.Split(' '))
        let regularDefinitions = List.ofArray(regularDefinitionsTextWithoutSpaces.Split('\n'))
        for regularDefinition in regularDefinitions do 
            let text = List.ofArray(regularDefinition.Split(':'))
            key <- text.Head // key = "L"
            for word in text do
                if (word <> text.Head) then
                    let regularExpression = System.String.Concat(System.String.Concat(word.Split('[')).Split(']')) //regularExpression = A-Za-z
                    for i = 0 to regularExpression.Length do 
                        regularExpressions.Add regularExpression.[i]
                        Ok()
                        |> ignore
