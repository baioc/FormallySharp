namespace Formally.Converter

open Shared
open Formally.Regular


module Converter = 
    let convertRegularDefinitionTextToRegexp(regularDefinition: string) = // L : a[A-Za-z]b(aba|c)*c
        let mutable key = ""
        let mutable value = Regexp.empty
        let expressions = ResizeArray<Regexp>()
        let mutable start = 0
        let mutable finish = 0
        let mutable setDetected = false
        let mutable bracketDetected = false

        let text = List.ofArray(regularDefinition.Split(':'))
        key <- text.Head // key = "L"
        let regularExpression = text.Item(1) // regularExpression = "a[A-Za-z]b(aba|c)*c"
        for i = 0 to (regularExpression.Length - 1) do
            if (regularExpression.[i] <> '[' && regularExpression.[i] <> ']' && not(setDetected)) then
                expressions.Add(Regexp.ofChar(regularExpression.[i]))
            elif (regularExpression.[i].Equals('[')) then
                setDetected <- true
                start <- i
            elif (regularExpression.[i].Equals(']')) then
                finish <- i
                let regularSet = regularExpression.Substring(start,(finish - start + 1)) // regularSet = "[A-Za-z]"
                let temp = ResizeArray<Regexp>()
                let mutable tempRegex = Regexp.empty
                for i = 0 to (regularSet.Length - 1) do
                    if (regularSet.[i].Equals('-')) then
                        temp.Add(Regexp.ofSet([regularSet.[i-1] .. regularSet.[i+1]]))
                for item in temp do
                    if (item.Equals(Regexp.empty)) then
                        tempRegex <- item
                    else 
                        tempRegex <- tempRegex + item
                expressions.Add(tempRegex)
                setDetected <- false
        
        for i = 0 to (expressions.Count - 1) do
            if (value.Equals(Regexp.empty)) then
                value <- expressions.[i]
            elif (expressions.[i] <> Regexp.ofChar('(') && expressions.[i] <> Regexp.ofChar(')') && expressions.[i] <> Regexp.ofChar('*') && expressions.[i] <> Regexp.ofChar('|') && not(bracketDetected)) then
                value <- value * expressions.[i]
            elif (expressions.[i].Equals(Regexp.ofChar('('))) then
                bracketDetected <- true
                start <- i
            elif (expressions.[i].Equals(Regexp.ofChar(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(aba|c)"
                let mutable pipeDetected = false
                let mutable insideRegex = Regexp.empty
                for j = 0 to (inside.Count - 1) do
                    if (inside.[j].Equals(Regexp.ofChar('|'))) then
                        pipeDetected <- true
                        let leftList = inside.GetRange(1, j-1)
                        let mutable left = Regexp.empty
                        for item in leftList do
                            if (left.Equals(Regexp.empty)) then
                                left <- item
                            else 
                                left <- left * item
                        let rightList = inside.GetRange(j+1, inside.Count-1-(j+1))
                        let mutable right = Regexp.empty
                        for item in rightList do
                            if (right.Equals(Regexp.empty)) then
                                right <- item
                            else 
                                right <- left * item
                        insideRegex <- left + right
                if (not(pipeDetected)) then
                    for item in inside do
                        if (insideRegex.Equals(Regexp.empty)) then
                            insideRegex <- item
                        else
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (expressions.[i+1].Equals(Regexp.ofChar('*'))) then
                    value <- value * (!* insideRegex)
                else
                    value <- value * insideRegex
                bracketDetected <- false
        key, value
                



    let getRegexFromBracketString(expressions: ResizeArray<Regexp>) = 
        let mutable value = Regexp.empty
        let expressions = ResizeArray<Regexp>()
        let mutable start = 0
        let mutable finish = 0
        let mutable bracketDetected = false
        for i = 0 to (expressions.Count - 1) do
            if (value.Equals(Regexp.empty)) then
                value <- expressions.[i]
            elif (expressions.[i] <> Regexp.ofChar('(') && expressions.[i] <> Regexp.ofChar(')') && expressions.[i] <> Regexp.ofChar('*') && expressions.[i] <> Regexp.ofChar('|') && not(bracketDetected)) then
                value <- value * expressions.[i]
            elif (expressions.[i].Equals(Regexp.ofChar('('))) then
                bracketDetected <- true
                start <- i
            elif (expressions.[i].Equals(Regexp.ofChar(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(aba|c)"
                let mutable pipeDetected = false
                let mutable insideRegex = Regexp.empty
                for j = 0 to (inside.Count - 1) do
                    if (inside.[j].Equals(Regexp.ofChar('|'))) then
                        pipeDetected <- true
                        let leftList = inside.GetRange(1, j-1)
                        let mutable left = Regexp.empty
                        for item in leftList do
                            if (left.Equals(Regexp.empty)) then
                                left <- item
                            else 
                                left <- left * item
                        let rightList = inside.GetRange(j+1, inside.Count-1-(j+1))
                        let mutable right = Regexp.empty
                        for item in rightList do
                            if (right.Equals(Regexp.empty)) then
                                right <- item
                            else 
                                right <- left * item
                        insideRegex <- left + right
                if (not(pipeDetected)) then
                    for item in inside do
                        if (insideRegex.Equals(Regexp.empty)) then
                            insideRegex <- item
                        else
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (expressions.[i+1].Equals(Regexp.ofChar('*'))) then
                    value <- value * (!* insideRegex)
                else
                    value <- value * insideRegex
                bracketDetected <- false
        value
        