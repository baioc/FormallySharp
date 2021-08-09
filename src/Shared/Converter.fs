namespace Formally.Converter

open Shared
open Formally.Regular


module Converter = 

    let getRegexFromParenthesesString(expressions: ResizeArray<Regexp>, isKleeneClosure: bool) = 
        let mutable value = Regexp.empty
        let mutable start = 0
        let mutable finish = 0
        let mutable parenthesesDetected = false
        for i = 0 to (expressions.Count - 1) do
            if (expressions.[i] <> Regexp.ofChar('(') && expressions.[i] <> Regexp.ofChar(')') && expressions.[i] <> Regexp.ofChar('*') && expressions.[i] <> Regexp.ofChar('|') && not(parenthesesDetected)) then
                value <- value * expressions.[i]
            elif (expressions.[i]=(Regexp.ofChar('('))) then
                parenthesesDetected <- true
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
                        if (insideRegex=(Regexp.empty) && item <> Regexp.ofChar('(') && item <> Regexp.ofChar(')')) then
                            insideRegex <- item
                        elif (item <> Regexp.ofChar('(') && item <> Regexp.ofChar(')')) then
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (isKleeneClosure && expressions.[i+1]=(Regexp.ofChar('*'))) then
                    value <- value * (!* insideRegex)
                else
                    value <- value * insideRegex
                parenthesesDetected <- false
        value

    let convertRegularDefinitionTextToRegexp(regularDefinition: string) = // L : a[A-Za-z]b(aba|c)*c
        let mutable key = ""
        let mutable value = Regexp.empty
        let mutable expressions = ResizeArray<Regexp>()
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

        let mutable positionsStartParentheses = ResizeArray<int>()
        let mutable positionsEndParentheses = ResizeArray<int>()
        let mutable isParentheses = true
        for i = 0 to (expressions.Count - 1) do
            if (expressions.[i]=(Regexp.ofChar('('))) then
                positionsStartParentheses.Add(i)
            if (expressions.[i]=(Regexp.ofChar(')'))) then
                positionsEndParentheses.Add(i) 
        if (positionsStartParentheses.Count = 0) then
            isParentheses <- false
        let mutable newExpressions = ResizeArray<Regexp>()
        let mutable isKleeneClosure = false
        let mutable positionKleeneToRemove = 99999999
        while isParentheses do   
            positionKleeneToRemove <- 99999999
            isKleeneClosure <- false
            newExpressions <- ResizeArray<Regexp>()
            for i=positionsStartParentheses.[positionsStartParentheses.Count - 1] to positionsEndParentheses.[0] do
                newExpressions.Add(expressions.[i])
                if (i+1 <= expressions.Count-1 && expressions.[i+1] = Regexp.ofChar('*')) then
                    newExpressions.Add(expressions.[i+1])
                    isKleeneClosure <- true
                    positionKleeneToRemove <- i+1
            let mutable value2 = getRegexFromParenthesesString(newExpressions, isKleeneClosure)
            newExpressions <- expressions
            expressions <- ResizeArray<Regexp>()
            for i=0 to positionsStartParentheses.[positionsStartParentheses.Count - 1]-1 do
                expressions.Add(newExpressions.[i])
            expressions.Add(value2)
            for i=positionsEndParentheses.[0]+1 to newExpressions.Count - 1 do
                if (i <> positionKleeneToRemove) then
                    expressions.Add(newExpressions.[i])
            positionsStartParentheses <- ResizeArray<int>()
            positionsEndParentheses <- ResizeArray<int>()
            for i = 0 to (expressions.Count - 1) do
                if (expressions.[i]=(Regexp.ofChar('('))) then
                    positionsStartParentheses.Add(i)
                if (expressions.[i]=(Regexp.ofChar(')'))) then
                    positionsEndParentheses.Add(i) 
            if (positionsStartParentheses.Count = 0) then
                isParentheses <- false
        if (expressions.Count = 1) then
            value <- expressions.[0]
        else
            // TODO: passar expression para o value. Considerar concatenação com algo fora do parenteses a direita ainda
            // como para o caso teste : ( {L} ( {L} | {D} ) )* | {D}
            value <- expressions.[0]
        key, value
                



    
        


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
