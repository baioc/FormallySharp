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
            elif (regularExpression.[i]=('[')) then
                setDetected <- true
                start <- i
            elif (regularExpression.[i]=(']')) then
                finish <- i
                let regularSet = regularExpression.Substring(start,(finish - start + 1)) // regularSet = "[A-Za-z]"
                let temp = ResizeArray<Regexp>()
                let mutable tempRegex = Regexp.empty
                for i = 0 to (regularSet.Length - 1) do
                    if (regularSet.[i]=('-')) then
                        temp.Add(Regexp.ofSet([regularSet.[i-1] .. regularSet.[i+1]]))
                for item in temp do
                    if (item=(Regexp.empty)) then
                        tempRegex <- item
                    else 
                        tempRegex <- tempRegex + item
                expressions.Add(tempRegex)
                setDetected <- false
        // TODO fazer a recursão pra adicionar mais parenteses dentro de parenteses
        for i = 0 to (expressions.Count - 1) do
            if (expressions.[i] <> Regexp.ofChar('(') && expressions.[i] <> Regexp.ofChar(')') && expressions.[i] <> Regexp.ofChar('*') && expressions.[i] <> Regexp.ofChar('|') && not(bracketDetected)) then
                if (value=(Regexp.empty)) then
                    value <- expressions.[i]
                else
                    value <- value * expressions.[i]
            elif (expressions.[i]=(Regexp.ofChar('('))) then
                bracketDetected <- true
                start <- i
            elif (expressions.[i]=(Regexp.ofChar(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(aba|c)"
                let mutable pipeDetected = false
                let mutable insideRegex = Regexp.empty
                for j = 0 to (inside.Count - 1) do
                    if (inside.[j]=(Regexp.ofChar('|'))) then
                        pipeDetected <- true
                        let leftList = inside.GetRange(1, j-1)
                        let mutable left = Regexp.empty
                        for item in leftList do
                            if (left=(Regexp.empty)) then
                                left <- item
                            else 
                                left <- left * item
                        let rightList = inside.GetRange(j+1, inside.Count-1-(j+1))
                        let mutable right = Regexp.empty
                        for item in rightList do
                            if (right=(Regexp.empty)) then
                                right <- item
                            else 
                                right <- left * item
                        insideRegex <- left + right
                if (not(pipeDetected)) then
                    for item in inside do
                        if (insideRegex=(Regexp.empty)) then
                            insideRegex <- item
                        else
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (expressions.[i+1]=(Regexp.ofChar('*'))) then
                    value <- value * (!* insideRegex)
                else
                    value <- value * insideRegex
                bracketDetected <- false
        key, value
                


    //TODO método recursivo para tratar múltiplos parenteses (falta ajustar pra ser)
    let getRegexFromBracketString(expressions: ResizeArray<Regexp>) = 
        let mutable value = Regexp.empty
        let expressions = ResizeArray<Regexp>()
        let mutable start = 0
        let mutable finish = 0
        let mutable bracketDetected = false
        for i = 0 to (expressions.Count - 1) do
            if (value=(Regexp.empty)) then
                value <- expressions.[i]
            elif (expressions.[i] <> Regexp.ofChar('(') && expressions.[i] <> Regexp.ofChar(')') && expressions.[i] <> Regexp.ofChar('*') && expressions.[i] <> Regexp.ofChar('|') && not(bracketDetected)) then
                value <- value * expressions.[i]
            elif (expressions.[i]=(Regexp.ofChar('('))) then
                bracketDetected <- true
                start <- i
            elif (expressions.[i]=(Regexp.ofChar(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(aba|c)"
                let mutable pipeDetected = false
                let mutable insideRegex = Regexp.empty
                for j = 0 to (inside.Count - 1) do
                    if (inside.[j]=(Regexp.ofChar('|'))) then
                        pipeDetected <- true
                        let leftList = inside.GetRange(1, j-1)
                        let mutable left = Regexp.empty
                        for item in leftList do
                            if (left=(Regexp.empty)) then
                                left <- item
                            else 
                                left <- left * item
                        let rightList = inside.GetRange(j+1, inside.Count-1-(j+1))
                        let mutable right = Regexp.empty
                        for item in rightList do
                            if (right=(Regexp.empty)) then
                                right <- item
                            else 
                                right <- left * item
                        insideRegex <- left + right
                if (not(pipeDetected)) then
                    for item in inside do
                        if (insideRegex=(Regexp.empty)) then
                            insideRegex <- item
                        else
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (expressions.[i+1]=(Regexp.ofChar('*'))) then
                    value <- value * (!* insideRegex)
                else
                    value <- value * insideRegex
                bracketDetected <- false
        value
        


    let convertTokenToRegexp(tokenText: string, regularDefinitionsMap: Map<string, string>) = // id: {L} ({L} | {D})*
        let mutable start = 0
        let mutable finish = 0
        let mutable keyDetected = false

        let text = List.ofArray(tokenText.Split(':'))
        let mutable regularExpression = text.Head + ":"
        let token = text.Item(1)

        for i = 0 to token.Length - 1 do //{L} ({L} | {D})*
            if (token.[i] <> '{' && token.[i] <> '}' && not(keyDetected)) then
                regularExpression <- regularExpression + token.[i].ToString()
            if (token.[i]='{') then
                keyDetected <- true
                start <- i
            elif (token.[i]='}') then
                finish <- i
                let inside = token.Substring((start+1), (finish-start-1))
                let mutable tokenId = ""
                for character in inside do
                    tokenId <- tokenId + character.ToString()
                let regex = regularDefinitionsMap.Item(tokenId)
                regularExpression <- regularExpression + regex
                keyDetected <- false
        
        let key, value = convertRegularDefinitionTextToRegexp(regularExpression)
        key, value
