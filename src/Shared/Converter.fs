namespace Formally.Converter

open Formally.Regular


module Converter = 

    let getRegexFromParenthesesString(expressions: ResizeArray<Regexp>, isKleeneClosure: bool) = 
        let mutable regex = Regexp.empty
        let mutable start = 0
        let mutable finish = 0
        let mutable parenthesesDetected = false
        for i = 0 to (expressions.Count - 1) do
            if (expressions.[i] <> Regexp.singleton('(') && expressions.[i] <> Regexp.singleton(')') && expressions.[i] <> Regexp.singleton('*') && expressions.[i] <> Regexp.singleton('|') && not(parenthesesDetected)) then
                if (regex = Regexp.empty) then
                    regex <- expressions.[i]
                else 
                    regex <- regex * expressions.[i]
            elif (expressions.[i]=(Regexp.singleton('('))) then
                parenthesesDetected <- true
                start <- i
            elif (expressions.[i]=(Regexp.singleton(')'))) then
                finish <- i
                let inside = expressions.GetRange(start,(finish - start + 1)) // inside = "(aba|c)"
                let mutable pipeDetected = false
                let mutable insideRegex = Regexp.empty
                for j = 0 to (inside.Count - 1) do
                    if (inside.[j]=(Regexp.singleton('|'))) then
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
                        if (insideRegex=(Regexp.empty) && item <> Regexp.singleton('(') && item <> Regexp.singleton(')')) then
                            insideRegex <- item
                        elif (item <> Regexp.singleton('(') && item <> Regexp.singleton(')')) then
                            insideRegex <- insideRegex * item
                pipeDetected <- false
                if (isKleeneClosure && expressions.[i+1]=(Regexp.singleton('*'))) then
                    if (regex = Regexp.empty) then
                        regex <- (!* insideRegex)
                    else
                        regex <- regex * (!* insideRegex)
                else
                    if (regex = Regexp.empty) then
                        regex <- insideRegex
                    else
                        regex <- regex * insideRegex
                parenthesesDetected <- false
        regex

    let convertRegularDefinitionTextToRegexp(regularDefinition: string) =
        let mutable regex = Regexp.empty
        let mutable expressions = ResizeArray<Regexp>()
        let mutable start = 0
        let mutable finish = 0
        let mutable setDetected = false
        let regularExpression = "(" + regularDefinition + ")"
        for i = 0 to (regularExpression.Length - 1) do
            if (regularExpression.[i] <> '[' && regularExpression.[i] <> ']' && not(setDetected)) then
                expressions.Add(Regexp.singleton(regularExpression.[i]))
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
                    if (tempRegex=(Regexp.empty)) then
                        tempRegex <- item
                    else 
                        tempRegex <- tempRegex + item
                expressions.Add(tempRegex)
                setDetected <- false
        let mutable positionsStartParentheses = ResizeArray<int>()
        let mutable positionsEndParentheses = ResizeArray<int>()
        let mutable isParentheses = true
        for i = 0 to (expressions.Count - 1) do
            if (expressions.[i]=(Regexp.singleton('('))) then
                positionsStartParentheses.Add(i)
            if (expressions.[i]=(Regexp.singleton(')'))) then
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
            let mutable startFor = positionsStartParentheses.[positionsStartParentheses.Count - 1]
            let mutable endFor = 0
            for i = 0 to (positionsEndParentheses.Count - 1) do
                if (positionsEndParentheses.[i] > startFor && endFor = 0) then
                    endFor <- positionsEndParentheses.[i]
            for i = startFor to endFor do
                newExpressions.Add(expressions.[i])
                if (i+1 <= expressions.Count-1 && expressions.[i+1] = Regexp.singleton('*')) then
                    newExpressions.Add(expressions.[i+1])
                    isKleeneClosure <- true
                    positionKleeneToRemove <- i+1
            let mutable value2 = getRegexFromParenthesesString(newExpressions, isKleeneClosure)
            newExpressions <- expressions
            expressions <- ResizeArray<Regexp>()
            for i=0 to startFor-1 do
                expressions.Add(newExpressions.[i])
            expressions.Add(value2)
            for i=endFor+1 to newExpressions.Count - 1 do 
                if (i <> positionKleeneToRemove) then
                    expressions.Add(newExpressions.[i])
            positionsStartParentheses <- ResizeArray<int>()
            positionsEndParentheses <- ResizeArray<int>()
            for i = 0 to (expressions.Count - 1) do
                if (expressions.[i]=(Regexp.singleton('('))) then
                    positionsStartParentheses.Add(i)
                if (expressions.[i]=(Regexp.singleton(')'))) then
                    positionsEndParentheses.Add(i) 
            if (positionsStartParentheses.Count = 0) then
                isParentheses <- false
        if (expressions.Count = 1) then
            regex <- regex * expressions.[0]
        regex

    let convertTokenToRegexString(token: string, regularDefinitionsMap: Map<string, string>) =
        let mutable start = 0
        let mutable finish = 0
        let mutable keyDetected = false

        let mutable regularExpression = ""

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
                if (regularDefinitionsMap.ContainsKey(tokenId)) then
                    let regexText = regularDefinitionsMap.Item(tokenId)
                    regularExpression <- regularExpression + regexText
                else 
                    regularExpression <- regularExpression + tokenId
                keyDetected <- false
        
        regularExpression
