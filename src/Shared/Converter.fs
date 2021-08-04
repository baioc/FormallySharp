namespace Formally.Converter

// open Fable.Remoting.Client
open Shared
open Formally.Regular

// let api =
//     Remoting.createApi ()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<IApi>

module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinitionsText: string) = 
        let mutable key = ""
        let mutable value = Regexp.empty
        let regularExpressions = ResizeArray<Regexp>()
        let regularDefinitionsTextWithoutSpaces = System.String.Concat(regularDefinitionsText.Split(' '))
        let regularDefinitions = List.ofArray(regularDefinitionsTextWithoutSpaces.Split('\n'))
        for regularDefinition in regularDefinitions do 
            let text = List.ofArray(regularDefinition.Split(':'))
            key <- text.Head // key = "L"
            for word in text do
                if (word <> text.Head) then
                    let regularExpression = System.String.Concat(System.String.Concat(word.Split('[')).Split(']')) //regularExpression = A-Za-z
                    while (regularExpression.Contains('-')) do
                        for i = 0 to regularExpression.Length do 
                            if (regularExpression.[i].Equals('-')) then
                                let temp = [regularExpression.[i-1] .. regularExpression.[i+1]]
                                regularExpressions.Add(Regexp.ofSet(temp))
                                regularExpression.Remove(i-1, 2) |> ignore
                    for c in regularExpression do 
                        regularExpressions.Add(Regexp.ofChar(c))
                    for regex in regularExpressions do
                        if (value.Equals(Regexp.empty)) then
                            value <- regex
                        else
                            value <- value * regex
            // IApi.putRegularDefinition(key, value)