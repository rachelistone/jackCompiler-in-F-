//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

namespace JackCompiler

open System
open System.IO

module Parsing =

        let parsing(path: String) =

            // finds all the files in path lasts in "MyT.xml"
            let TxmlFilesInDirectory = Directory.GetFiles(path, "*MyT.xml") 

            for pathOfFile in TxmlFilesInDirectory do

                let spacesNum = ref -1

                let printSpaces() =
                    let mutable spaces = ""
                    for i in 0..!spacesNum do
                        spaces <- sprintf "%s  " spaces
                    spaces

                let mutable peekedLine = ""

                // create and open output file .xml
                let TxmlFileName = Path.GetFileNameWithoutExtension pathOfFile
                let fileName = sprintf "%s.xml" (TxmlFileName.Remove(TxmlFileName.Length-1))
                let outputFilePath = path + "\\" + fileName 
                let outputFile = new StreamWriter(outputFilePath, false)

                // open jack file to read from
                use stream = new StreamReader(pathOfFile)

                let getNextToken() = 
                    if peekedLine.Length = 0 then
                        let line = stream.ReadLine()
                        line
                    else
                        let line = peekedLine
                        peekedLine <- ""
                        line

                // peek - without consuming from stream
                let checkNextToken() = 
                    if peekedLine.Length = 0 then
                        peekedLine <- stream.ReadLine()
                    let words = peekedLine.Split ' '
                    words.[1]

        // -------------------------------------------------- Expressions: -------------------------------

                let rec term() = 
                    outputFile.WriteLine(printSpaces() + "<term>")
                    spacesNum := !spacesNum + 1
                    if checkNextToken() = "~" || checkNextToken() = "-" then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> unaryOp </symbol>
                        term()
                    elif checkNextToken() = "(" then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                        expression()
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>
                    else
                        outputFile.WriteLine(printSpaces() + getNextToken()) 
                    // <integerConstant> number </integerConstant> or
                    // <stringConstant> string </stringConstant> or
                    // <keywordConstant> 'true' | 'false' | 'null' | 'this' </keywordConstant> or
                    // <identifier> varName | subroutineName | className </identifier>
                        if checkNextToken() = "[" then
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '[' </symbol>
                            expression()
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ']' </symbol>
                        elif checkNextToken() = "(" || checkNextToken() = "." then
                            subroutineCall()
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</term>")
        
                and expression() = 
                    outputFile.WriteLine(printSpaces() + "<expression>")
                    spacesNum := !spacesNum + 1
                    term()
                    while checkNextToken() = "+" || checkNextToken() = "-" || checkNextToken() = "*" || 
                    checkNextToken() = "/" || checkNextToken() = "&amp;" || checkNextToken() = "|" || checkNextToken() = "&lt;" ||
                    checkNextToken() = "&gt;" || checkNextToken() = "=" do
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> op </symbol>
                        term()
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</expression>")

                and expressionList() = 
                    outputFile.WriteLine(printSpaces() + "<expressionList>")
                    spacesNum := !spacesNum + 1
                    if not(checkNextToken() = ")") then // if there is an expression list, then next token is not ")" 
                         expression()
                         while checkNextToken() = "," do
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                            expression()
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</expressionList>")

                and subroutineCall() = // this function assume that the first(subroutineCall) was consumed
                    if checkNextToken() = "(" then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                        expressionList()
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>
                    elif checkNextToken() = "." then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '.' </symbol>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> subroutineName </identifier>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                        expressionList()
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>

        // -------------------------------------------------- Statements: -------------------------------

                let rec statements() =
                    outputFile.WriteLine(printSpaces() + "<statements>")
                    spacesNum := !spacesNum + 1
                    while checkNextToken() = "let" || checkNextToken() = "if" || 
                    checkNextToken() = "while"|| checkNextToken() = "do" ||checkNextToken() = "return" do
                        statement()
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</statements>")

                and statement() = 
                    let currentToken = checkNextToken()
                    match currentToken with
                    | "let" -> letStatement()
                    | "if" -> ifStatement()
                    | "while" -> whileStatement()
                    | "do" -> doStatement()
                    | "return" -> returnStatement()

                and letStatement() =
                    outputFile.WriteLine(printSpaces() + "<letStatement>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'let' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    if checkNextToken() = "[" then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '[' </symbol>
                        expression()
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ']' </symbol>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '=' </symbol>
                    expression()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ';' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</letStatement>")

                and ifStatement() =
                    outputFile.WriteLine(printSpaces() + "<ifStatement>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'if' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                    expression()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '{' </symbol>
                    statements() 
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '}' </symbol>
                    if checkNextToken() = "else" then
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'else' </keyword>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '{' </symbol>
                        statements() 
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '}' </symbol> 
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</ifStatement>")

                and whileStatement() =
                    outputFile.WriteLine(printSpaces() + "<whileStatement>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'while' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                    expression()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '{' </symbol>
                    statements() 
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '}' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</whileStatement>")

                and doStatement() =
                    outputFile.WriteLine(printSpaces() + "<doStatement>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'do' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName | subroutineName | className </identifier>
                    subroutineCall()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ';' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</doStatement>")

                and returnStatement() =
                    outputFile.WriteLine(printSpaces() + "<returnStatement>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'return' </keyword>
                    if not (checkNextToken() = ";") then  // if there is an expression after the 'return', then next token is not ";" 
                        expression()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ';' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</returnStatement>")

        // ---------------------------------------------Program stracture: -------------------------------

                let varDec() =
                    outputFile.WriteLine(printSpaces() + "<varDec>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'var' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> type </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    while checkNextToken() = "," do
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ',' </symbol>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ';' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</varDec>")

                let parameterList() =
                    outputFile.WriteLine(printSpaces() + "<parameterList>")
                    spacesNum := !spacesNum + 1
                    if not(checkNextToken() = ")") then // if there is a parameter list, then next token is not ")" 
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> type </keyword>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                        while checkNextToken() = "," do
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ',' </symbol>
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> type </keyword>
                            outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</parameterList>")
        
                let subroutineBody() =
                    outputFile.WriteLine(printSpaces() + "<subroutineBody>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '{' </symbol>
                    while checkNextToken() = "var" do
                        varDec()
                    statements()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '}' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</subroutineBody>")

                let ClassVarDec() = 
                    outputFile.WriteLine(printSpaces() + "<classVarDec>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> static | field </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> type </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    while checkNextToken() = "," do
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ',' </symbol>
                        outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> varName </identifier>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ';' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</classVarDec>")

                let subroutineDec() = 
                    outputFile.WriteLine(printSpaces() + "<subroutineDec>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'constructor' | 'function' | 'method' </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> 'void' | type </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> subroutineName </identifier>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '(' </symbol>
                    parameterList()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> ')' </symbol>
                    subroutineBody()
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</subroutineDec>")

                let classParse() =
                    outputFile.WriteLine(printSpaces() + "<class>")
                    spacesNum := !spacesNum + 1
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <keyword> class </keyword>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <identifier> className </identifier>
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '{' </symbol>
                    while checkNextToken() = "static" || checkNextToken() = "field" do
                        ClassVarDec()
                    while checkNextToken() = "constructor" || checkNextToken() = "function" || checkNextToken() = "method" do
                        subroutineDec()
                    outputFile.WriteLine(printSpaces() + getNextToken()) // <symbol> '}' </symbol>
                    spacesNum := !spacesNum - 1
                    outputFile.WriteLine(printSpaces() + "</class>")

                getNextToken() // <tokens>
                classParse()

                stream.Close() 
                outputFile.Close()

