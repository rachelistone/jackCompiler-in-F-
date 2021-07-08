//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

namespace JackCompiler

open System
open System.IO
open System.Collections.Generic

module Parsing =

        type Record = { TypeOf:string 
                        Kind:string
                        Number:int }

        let parsing(path: String) =

            // finds all the files in path lasts in "MyT.xml"
            let TxmlFilesInDirectory = Directory.GetFiles(path, "*MyT.xml") 

            for pathOfFile in TxmlFilesInDirectory do

                let mutable peekedLine = ""

                let symbolTable = new Dictionary<string, Record>()
                let funcTable = new Dictionary<string, Record>()

                let fieldCounter = ref 0
                let staticCounter = ref 0
                let argumentCounter = ref 0
                let localCounter = ref 0
                let className = ref ""
                let funcType = ref ""
                let whatToReturn = ref ""
                let funcName = ref ""
                let numOfArgsInCall = ref 0
                let funcOrClassName = ref ""
                let ifLabelNum = ref 0
                let whileLabelNum = ref 0

                // create and open output file .xml
                let TxmlFileName = Path.GetFileNameWithoutExtension pathOfFile
                let fileName = sprintf "%s.vm" (TxmlFileName.Remove(TxmlFileName.Length-3))
                let outputFilePath = path + "\\" + fileName 
                let outputFile = new StreamWriter(outputFilePath, false)

                // open jack file to read from
                use stream = new StreamReader(pathOfFile)

                let getNextToken() = 
                    if peekedLine.Length = 0 then
                        let line = stream.ReadLine()
                        line.Split ' '
                    else
                        let line = peekedLine
                        peekedLine <- ""
                        line.Split ' '

                // peek - without consuming from stream, returns the only the token without the in - <>
                let checkNextToken() = 
                    if peekedLine.Length = 0 then
                        peekedLine <- stream.ReadLine()
                    let words = peekedLine.Split ' '
                    words

        // -------------------------------------------------- Expressions: -------------------------------

                let rec term() = 
                    if checkNextToken().[1] = "~" || checkNextToken().[1] = "-" then
                        let op = getNextToken().[1] // <symbol> unaryOp </symbol>
                        term()
                        match op with
                        |"~" -> outputFile.WriteLine(sprintf "not")
                        |"-" -> outputFile.WriteLine(sprintf "neg")
                        
                    elif checkNextToken().[1] = "(" then
                        getNextToken() |> ignore // <symbol> '(' </symbol>
                        expression()
                        getNextToken() |> ignore // <symbol> ')' </symbol>
                    else
                        let words = getNextToken()

                        // <integerConstant> number </integerConstant>
                        if words.[0] = "<integerConstant>" then 
                            outputFile.WriteLine(sprintf "push constant %s" words.[1])

                        // <stringConstant> string </stringConstant>
                        elif words.[0] = "<stringConstant>" then
                            let theString = String.Join(' ', words.[1..(words.Length - 2)])
                            outputFile.WriteLine(sprintf "push constant %i" theString.Length)
                            outputFile.WriteLine(sprintf "call String.new 1")
                            for i in 0..theString.Length-1 do
                                outputFile.WriteLine(sprintf "push constant %i" ((int)theString.[i]))
                                outputFile.WriteLine(sprintf "call String.appendChar 2")

                        // <keywordConstant> 'true' | 'false' | 'null' | 'this' </keywordConstant> or
                        elif words.[1] = "this" then
                            outputFile.WriteLine(sprintf "push pointer 0")
                        elif words.[1] = "null" || words.[1] = "false" then // vm: false = 0, true = 1
                            outputFile.WriteLine(sprintf "push constant 0")
                        elif words.[1] = "true" then // true is -1
                            outputFile.WriteLine(sprintf "push constant 0")
                            outputFile.WriteLine(sprintf "not")

                        // array [] 
                        elif checkNextToken().[1] = "[" then // if it was varName

                            getNextToken() |> ignore // <symbol> '[' </symbol>
                            expression()
                            getNextToken() |> ignore // <symbol> ']' </symbol>

                            let record = funcTable.[words.[1]]
                            if record.Kind = "var" then
                                outputFile.WriteLine(sprintf "push local %i" record.Number)
                            if record.Kind = "argument" then
                                outputFile.WriteLine(sprintf "push argument %i" record.Number)

                            outputFile.WriteLine(sprintf "add") // name of the array + index
                            outputFile.WriteLine(sprintf "pop pointer 1") // x+i is the desired address
                            outputFile.WriteLine(sprintf "push that 0") // push the value of the calculated address to the stack

                        // subroutineName | className 
                        elif checkNextToken().[1] = "(" || checkNextToken().[1] = "." then // if it was subroutineName - "(" or className - "."
                            funcOrClassName := words.[1]
                            subroutineCall()

                        // <identifier> varName </identifier>
                        elif symbolTable.ContainsKey(words.[1]) then
                            let record = symbolTable.[words.[1]]
                            if record.Kind = "field" then
                                outputFile.WriteLine(sprintf "push this %i" record.Number)
                            if record.Kind = "static" then
                                outputFile.WriteLine(sprintf "push static %i" record.Number)
                        elif funcTable.ContainsKey(words.[1]) then
                            let record = funcTable.[words.[1]]
                            if record.Kind = "var" then
                                outputFile.WriteLine(sprintf "push local %i" record.Number)
                            if record.Kind = "argument" then
                                outputFile.WriteLine(sprintf "push argument %i" record.Number)
        
                and expression() = 
                    term()
                    while checkNextToken().[1] = "+" || checkNextToken().[1] = "-" || checkNextToken().[1] = "*" || 
                    checkNextToken().[1] = "/" || checkNextToken().[1] = "&amp;" || checkNextToken().[1] = "|" || checkNextToken().[1] = "&lt;" ||
                    checkNextToken().[1] = "&gt;" || checkNextToken().[1] = "=" do
                        let op = getNextToken().[1] // <symbol> op </symbol>
                        term()
                        match op with
                        | "+" -> outputFile.WriteLine(sprintf "add")
                        | "-" -> outputFile.WriteLine(sprintf "sub")
                        | "*" -> outputFile.WriteLine(sprintf "call Math.multiply 2")
                        | "/" -> outputFile.WriteLine(sprintf  "call Math.divide 2")
                        | "&amp;" -> outputFile.WriteLine(sprintf "and")
                        | "|" -> outputFile.WriteLine(sprintf "or")
                        | "&lt;" -> outputFile.WriteLine(sprintf "lt")
                        | "&gt;" -> outputFile.WriteLine(sprintf  "gt")
                        | "=" -> outputFile.WriteLine( "eq")


                and expressionList() = 
                    numOfArgsInCall := 0
                    if not(checkNextToken().[1] = ")") then // if there is an expression list, then next token is not ")", 
                    // this function called only from subroutineCall - which has "(" and ")" 
                         expression()
                         numOfArgsInCall := 1
                         while checkNextToken().[1] = "," do
                            getNextToken() |> ignore // <symbol> ',' </symbol>
                            expression()
                            numOfArgsInCall := !numOfArgsInCall + 1

                and subroutineCall() = // this function assume that the first(subroutineCall) was consumed

                    // if the call was without class name or object, so this function is a method of the current class
                    if checkNextToken().[1] = "(" then // funcOrClassName is subroutineName
                        getNextToken() |> ignore // <symbol> '(' </symbol>
                        outputFile.WriteLine(sprintf "push pointer 0")
                        expressionList()
                        outputFile.WriteLine(sprintf "call %s.%s %i " !className !funcOrClassName (!numOfArgsInCall+1))

                    // if the function is with object class name, and this name is saved in table, so this is a method, push the object to the arg list,
                    // then call the method with the class name in left side
                    elif checkNextToken().[1] = "." then // 'funcOrClassName' is className or varName
                        getNextToken() |> ignore // <symbol> '.' </symbol> 
                        let subroutineName = getNextToken().[1] // <identifier> subroutineName </identifier>
                        getNextToken() |> ignore // <symbol> '(' </symbol>
                        let mutable record = {
                                                Kind=""
                                                TypeOf=""
                                                Number=0 }
                        
                        // object of the current class that calls method
                        if symbolTable.ContainsKey(!funcOrClassName) then
                            record <- symbolTable.[!funcOrClassName]
                            if record.Kind = "field" then
                                outputFile.WriteLine(sprintf "push this %i" record.Number)
                                expressionList()
                            //if record.Kind = "static" then
                             //   outputFile.WriteLine(sprintf "push static %i" record.Number)
                            outputFile.WriteLine(sprintf "call %s.%s %i " record.TypeOf subroutineName (!numOfArgsInCall+1)) 
                        
                        // local or argument object of the current function that calls method
                        elif funcTable.ContainsKey(!funcOrClassName) then
                            record <- funcTable.[!funcOrClassName]
                            if record.Kind = "var" then
                                outputFile.WriteLine(sprintf "push local %i" record.Number)
                                expressionList()
                            if record.Kind = "argument" then
                                outputFile.WriteLine(sprintf "push argument %i" record.Number)
                                expressionList()

                            outputFile.WriteLine(sprintf "call %s.%s %i " record.TypeOf subroutineName (!numOfArgsInCall + 1)) 

                        // if the left side was class name - static function of a class
                        else
                            expressionList()
                            outputFile.WriteLine(sprintf "call %s.%s %i " !funcOrClassName subroutineName !numOfArgsInCall) 
                    

                    getNextToken() |> ignore// <symbol> ')' </symbol>

        // -------------------------------------------------- Statements: -------------------------------

                let rec statements() =
                    while checkNextToken().[1] = "let" || checkNextToken().[1] = "if" || 
                    checkNextToken().[1] = "while"|| checkNextToken().[1] = "do" || checkNextToken().[1] = "return" do
                        statement()

                and statement() = 
                    let currentToken = checkNextToken().[1]
                    match currentToken with
                    | "let" -> letStatement()
                    | "if" -> ifStatement()
                    | "while" -> whileStatement()
                    | "do" -> doStatement()
                    | "return" -> returnStatement()

                and letStatement() =
                    getNextToken() |> ignore // <keyword> 'let' </keyword>
                    let varName = getNextToken().[1] // <identifier> varName </identifier>
                    let mutable isArr = false
                    if checkNextToken().[1] = "[" then
                        isArr <- true
                        getNextToken() |> ignore // <symbol> '[' </symbol>
                        expression()
                        let record = funcTable.[varName]
                        if record.Kind = "var" then
                            outputFile.WriteLine(sprintf "push local %i" record.Number)
                        if record.Kind = "argument" then
                            outputFile.WriteLine(sprintf "push argument %i" record.Number)
                        outputFile.WriteLine(sprintf "add") // e.g. x[i] x was pushed as varName, i was pushed in expression()
                        
                        getNextToken() |> ignore // <symbol> ']' </symbol>
                    getNextToken() |> ignore // <symbol> '=' </symbol>

                    expression() // push right side of the assignment

                    // pop it to the left side parameter of assignment
                    if isArr then
                        outputFile.WriteLine(sprintf "pop temp 0")
                        outputFile.WriteLine(sprintf "pop pointer 1")
                        outputFile.WriteLine(sprintf "push temp 0")
                        outputFile.WriteLine(sprintf "pop that 0")

                    elif symbolTable.ContainsKey(varName) then
                        let record = symbolTable.[varName]
                        if record.Kind = "field" then
                            outputFile.WriteLine(sprintf "pop this %i" record.Number)
                        if record.Kind = "static" then
                            outputFile.WriteLine(sprintf "pop static %i" record.Number)

                    elif funcTable.ContainsKey(varName) then
                        let record = funcTable.Item(varName)
                        if record.Kind = "var" then
                            outputFile.WriteLine(sprintf "pop local %i" record.Number)
                        if record.Kind = "argument" then
                            outputFile.WriteLine(sprintf "pop argument %i" record.Number)

                    getNextToken() |> ignore // <symbol> ';' </symbol>

                and ifStatement() =
                    getNextToken() |> ignore // <keyword> 'if' </keyword>
                    getNextToken() |> ignore // <symbol> '(' </symbol>
                    expression()
                    getNextToken() |> ignore // <symbol> ')' </symbol>
                    getNextToken() |> ignore // <symbol> '{' </symbol>

                    outputFile.WriteLine(sprintf "if-goto IF_TRUE%i" !ifLabelNum)
                    outputFile.WriteLine(sprintf "goto IF_FALSE%i" !ifLabelNum)

                    outputFile.WriteLine(sprintf "label IF_TRUE%i" !ifLabelNum)
                    let localNumIf = !ifLabelNum
                    ifLabelNum:= !ifLabelNum + 1
                    statements() 

                    getNextToken() |> ignore // <symbol> '}' </symbol>
                    if checkNextToken().[1] = "else" then

                        outputFile.WriteLine(sprintf "goto IF_END%i" localNumIf)
                        getNextToken() |> ignore // <keyword> 'else' </keyword>
                        getNextToken() |> ignore // <symbol> '{' </symbol>

                        outputFile.WriteLine(sprintf "label IF_FALSE%i" localNumIf)
                        statements() 
                        
                        outputFile.WriteLine(sprintf "label IF_END%i" localNumIf)
                        getNextToken() |> ignore // <symbol> '}' </symbol>
                    else  
                        outputFile.WriteLine(sprintf "label IF_FALSE%i" localNumIf)
                        //if !ifLabelNum = 9 && localNumIf = 4 then
                         //  outputFile.Close()

                and whileStatement() =
                    getNextToken() |> ignore // <keyword> 'while' </keyword>
                    outputFile.WriteLine(sprintf "label WHILE_EXP%i" !whileLabelNum)
                    getNextToken() |> ignore // <symbol> '(' </symbol>
                    expression() // push the calculation of the condition
                    getNextToken() |> ignore // <symbol> ')' </symbol>
                    outputFile.WriteLine(sprintf "not")
                    outputFile.WriteLine(sprintf "if-goto WHILE_END%i" !whileLabelNum)
                    let localLabelNum = !whileLabelNum 
                    // if it will call another while in statement(), save this level of recursion, then promote to the next level.
                    whileLabelNum:= !whileLabelNum + 1 
                    getNextToken() |> ignore // <symbol> '{' </symbol>
                    statements() 
                    getNextToken() |> ignore // <symbol> '}' </symbol>
                    // outputFile.Close()
                    outputFile.WriteLine(sprintf "goto WHILE_EXP%i" localLabelNum)
                    outputFile.WriteLine(sprintf "label WHILE_END%i" localLabelNum)

                and doStatement() =
                    getNextToken() |> ignore // <keyword> 'do' </keyword>
                    funcOrClassName := getNextToken().[1] // <identifier> varName | subroutineName | className </identifier>
                    subroutineCall()
                    outputFile.WriteLine(sprintf "pop temp 0") // we dont need the constant 0 from void functions
                    getNextToken() |> ignore // <symbol> ';' </symbol>

                and returnStatement() =
                    getNextToken() |> ignore // <keyword> 'return' </keyword>
                    if not (checkNextToken().[1] = ";") then  // if there is an expression after the 'return', then next token is not ";" 
                        expression()
                    else
                        outputFile.WriteLine(sprintf "push constant 0")
                    outputFile.WriteLine(sprintf "return") // vm always need 'return' in the end of function
                    getNextToken() |> ignore // <symbol> ';' </symbol>

        // ---------------------------------------------Program stracture: -------------------------------

                let varDec() =
                    let kind = getNextToken().[1] // <keyword> 'var' </keyword>
                    let typeOf = getNextToken().[1] // <keyword> type </keyword>
                    let mutable name = getNextToken().[1] // <identifier> varName </identifier>
                    let mutable record = { TypeOf = typeOf 
                                           Kind = kind 
                                           Number = !localCounter }
                    funcTable.Add(name, record) 
                    localCounter := !localCounter + 1
                    while checkNextToken().[1] = "," do
                        getNextToken() |> ignore // <symbol> ',' </symbol>
                        name <- getNextToken().[1] // <identifier> varName </identifier>
                        record <- { TypeOf = typeOf 
                                    Kind = kind 
                                    Number = !localCounter }
                        funcTable.Add(name, record) 
                        localCounter := !localCounter + 1
                    getNextToken() |> ignore // <symbol> ';' </symbol>

                let parameterList() =
                    if not(checkNextToken().[1] = ")") then // if there is a parameter list, then next token is not ")" 
                        let typeOf = getNextToken().[1] // <keyword> type </keyword>
                        let name = getNextToken().[1] // <identifier> varName </identifier>
                        let mutable record = { Kind=""
                                               TypeOf=""
                                               Number=0 }

                        if !funcType = "method" then // method has 'this' as first arg so the next argument's index is +1
                            record <- { TypeOf = typeOf 
                                        Kind = "argument" 
                                        Number = !argumentCounter + 1}
                        else // static or constructor doesn't have to get 'this' as first argument
                            record <- { TypeOf = typeOf 
                                        Kind = "argument" 
                                        Number = !argumentCounter }
                        funcTable.Add(name, record)
                        argumentCounter := !argumentCounter + 1
                        while checkNextToken().[1] = "," do
                            getNextToken() |> ignore // <symbol> ',' </symbol>
                            let typeOf = getNextToken().[1] // <keyword> type </keyword>
                            let name = getNextToken().[1] // <identifier> varName </identifier>
                            if !funcType = "method" then
                                record <- { TypeOf = typeOf 
                                            Kind = "argument" 
                                            Number = !argumentCounter + 1}
                            else
                                record <- { TypeOf = typeOf 
                                            Kind = "argument" 
                                            Number = !argumentCounter }
                            funcTable.Add(name, record) 
                            argumentCounter := !argumentCounter + 1
        
                let subroutineBody() =
                    getNextToken() |> ignore // <symbol> '{' </symbol>
                    while checkNextToken().[1] = "var" do
                        varDec()
                    outputFile.WriteLine(sprintf "function %s.%s %i" !className !funcName !localCounter)
                    if !funcType = "constructor" then
                        outputFile.WriteLine(sprintf "push constant %i" !fieldCounter)
                        outputFile.WriteLine(sprintf "call Memory.alloc 1")
                        outputFile.WriteLine(sprintf "pop pointer 0")
                    elif !funcType = "method" then
                        outputFile.WriteLine(sprintf "push argument 0")
                        outputFile.WriteLine(sprintf "pop pointer 0")
                    statements()
                    getNextToken() // <symbol> '}' </symbol>

                let ClassVarDec() = 
                    let kind = getNextToken().[1] // <keyword> static | field </keyword>
                    let typeOf = getNextToken().[1] // <keyword> type </keyword>
                    let name = getNextToken().[1] // <identifier> varName </identifier>
                    if kind = "static" then
                        let record = { TypeOf = typeOf 
                                       Kind = kind 
                                       Number = !staticCounter }
                        symbolTable.Add(name, record) 
                        staticCounter := !staticCounter + 1

                    elif kind = "field" then
                        let record = { TypeOf = typeOf 
                                       Kind = kind 
                                       Number = !fieldCounter }
                        symbolTable.Add(name, record) 
                        fieldCounter := !fieldCounter + 1

                    while checkNextToken().[1] = "," do
                        getNextToken() |> ignore // <symbol> ',' </symbol>
                        let name = getNextToken().[1] // <identifier> varName </identifier>
                        if kind = "static" then
                            let record = { TypeOf = typeOf 
                                           Kind = kind 
                                           Number = !staticCounter }
                            symbolTable.Add(name, record) 
                            staticCounter := !staticCounter + 1
                        elif kind = "field" then
                            let record = { TypeOf = typeOf 
                                           Kind = kind 
                                           Number = !fieldCounter }
                            symbolTable.Add(name, record) 
                            fieldCounter := !fieldCounter + 1
                    getNextToken() // <symbol> ';' </symbol>

                let subroutineDec() =
                    funcTable.Clear()
                    localCounter:= 0
                    argumentCounter:= 0
                    funcType := getNextToken().[1] // <keyword> 'constructor' | 'function' | 'method' </keyword>
                    whatToReturn := getNextToken().[1] // <keyword> 'void' | type </keyword>
                    funcName := getNextToken().[1] // <identifier> subroutineName </identifier>
                    getNextToken() |> ignore // <symbol> '(' </symbol>
                    parameterList()
                    getNextToken() |> ignore // <symbol> ')' </symbol>
                    subroutineBody()

                let classParse() =
                    symbolTable.Clear()
                    fieldCounter:=0
                    staticCounter:=0
                    getNextToken() |> ignore// <keyword> class </keyword>
                    className := getNextToken().[1] // <identifier> className </identifier>
                    getNextToken() |> ignore // <symbol> '{' </symbol>
                    while checkNextToken().[1] = "static" || checkNextToken().[1] = "field" do
                        ClassVarDec() |> ignore
                    try
                        while checkNextToken().[1] = "constructor" || checkNextToken().[1] = "function" || checkNextToken().[1] = "method" do
                            subroutineDec() |> ignore
                    with
                    | :? Exception as ex ->
                        0 |> ignore
                    getNextToken() // <symbol> '}' </symbol>

                getNextToken() |> ignore // <tokens>
                classParse() |> ignore

                stream.Close() 
                outputFile.Close()

