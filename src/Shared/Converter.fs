namespace Formally.Converter

open Shared
open Formally.Regular


module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinitionsText: string) = 
        let mutable key = ""
        let mutable value = Regexp.empty // L: [A-Z] \n D: [0-9]
        let regularExpressions = ResizeArray<Regexp>() // {L}|{D}|a
        let regularDefinitionsTextWithoutSpaces = System.String.Concat(regularDefinitionsText.Split(' '))
        let regularDefinitions = List.ofArray(regularDefinitionsTextWithoutSpaces.Split('\n'))
        for regularDefinition in regularDefinitions do 
            let text = List.ofArray(regularDefinition.Split(':'))
            key <- text.Head // key = "L"
            for regularExpression in text do
                if (regularExpression <> text.Head) then
                    for i = 0 to regularExpression.Length do //regularExpression = ab[A-Za-z]ba
                        if (regularExpression.[i] <> '[') then
                            if (regularExpression.[i].Equals('|'))
                            regularExpressions.Add(Regexp.ofChar(regularExpression.[i]))
                        else
                            

                    // while (regularExpression.Contains('-')) do
                    let regularSet = System.String.Concat(System.String.Concat(word.Split('[')).Split(']')) //regularExpression = A-Za-z
                        if (regularSet.[i].Equals('-')) then
                            let temp = [regularSet.[i-1] .. regularSet.[i+1]]
                            regularExpressions.Add(Regexp.ofSet(temp))
                            // regularSet.Remove(i-1, 2) |> ignore
                    for c in regularExpression do 
                        regularExpressions.Add(Regexp.ofChar(c))
                    for regex in regularExpressions do
                        if (value.Equals(Regexp.empty)) then
                            value <- regex
                        else
                            value <- value * regex // a((ba|c)|z)
            // IApi.putRegularDefinition(key, value)]
        return regularExpressions, value