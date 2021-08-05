namespace Formally.Converter

open Shared
open Formally.Regular


module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinition: string) = // L : a[A-Za-z]b
        let mutable key = ""
        let mutable value = Regexp.empty
        let expressions = ResizeArray<Regexp>()
        let mutable start = 0
        let mutable finish = 0
        let mutable setDetected = false

        let text = List.ofArray(regularDefinition.Split(':'))
        key <- text.Head // key = "L"
        let regularExpression = text.Item(1) // regularExpression = "a[A-Za-z]b"
        for i = 0 to (regularExpression.Length - 1) do
            if (regularExpression.[i] <> '[' && regularExpression.[i] <> ']' && not(setDetected)) then
                expressions.Add(Regexp.ofChar(regularExpression.[i]))
            elif (regularExpression.[i].Equals('[')) then
                setDetected <- true
                start <- i
            elif (regularExpression.[i].Equals(']')) then
                finish <- i
                let regularSet = regularExpression.Substring(start,(finish - start + 1)) // regularSet = "[A-Za-z]"
                for i = 0 to (regularSet.Length - 1) do
                    if (regularSet.[i].Equals('-')) then
                        expressions.Add(Regexp.ofSet([regularSet.[i-1] .. regularSet.[i+1]]))
                setDetected <- false
        setDetected <- false

        for i = 0 to (expressions.Count - 1) do
            let regex = expressions.[i]
            if (value.Equals(Regexp.empty)) then
                value <- regex
            // TODO (atribuições estão erradas)
            elif (regex.Equals(Regexp.ofChar('('))) then
                setDetected <- true
                start <- i
            elif (regex.Equals(Regexp.ofChar(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(...)"
                for i = 0 to (inside.Count - 1) do
                    if (inside.[i].Equals('|')) then
                        value <- value + regex
                if (expressions.[i].Equals(Regexp.ofChar('|'))) then
                    value <- value * (!* regex)
                setDetected <- false
            else
                value <- value * regex
                
        key, value
                
