//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

namespace JackCompiler

open System
open System.IO


module Tokening =

        let tokening(path: String) =

            let keywords = ["class"; "constructor"; "function"; "method"; "field";
                "static"; "var"; "int";"char"; "boolean"; "void"; "null"; "true";
                "false"; "this"; "let"; "do"; "if"; "else"; "while"; "return"]

            let isKeyword (word:string) =
                let mutable result = false
                for w in keywords do 
                    if w = word then
                        result <- true
                result

            let symbols = ['{';'}';'(';')';'[';']';'.';',';';';'+';'-';'*';'/';'&';'|';'~';'>';'<';'=']

            let isSymbol (character:char) =
                let mutable result = false
                for c in symbols do 
                    if c = character then
                        result <- true
                result
    
        // --------------------------------------- tokening ----------------------------------------------

            // finds all the files in path lasts in ".jack"
            let JackFilesInDirectory = Directory.GetFiles(path, "*.jack")    

            let currentChar = ref ' '

            for pathOfFile in JackFilesInDirectory do

                // create and open output file .xml
                let fileName = sprintf "%sMyT.xml" (Path.GetFileNameWithoutExtension pathOfFile)
                let outputFilePath = path + "\\" + fileName 
                let outputFile = new StreamWriter(outputFilePath, false)

                Console.WriteLine(sprintf "path:%s\nfilename:%s\noutputpath:%s"pathOfFile fileName outputFilePath)

                // open jack file to read from
                use stream = new StreamReader(pathOfFile)

                let readnext() = 
                    let characterInt = stream.Read()
                    (char)characterInt

                outputFile.WriteLine("<tokens>")

                // read the char
                currentChar := readnext()

                // loop all the characters in the jack file
                while (not stream.EndOfStream) do

                    // buffer to keep the chars of the token
                    let mutable currentWord = ""

                    let isletter = Char.IsLetter(!currentChar) || !currentChar = '_'
                    let isdigit =  Char.IsDigit(!currentChar)
                    let issymbol = isSymbol(!currentChar)

                    // handle identifier or keyword
                    if isletter  then
                        while not(!currentChar = ' ') && not(!currentChar = '\n')  && not(isSymbol(!currentChar)) do
                            //read the char
                            currentWord <- sprintf "%s%c" currentWord !currentChar
                            currentChar := readnext()
                        if isKeyword(currentWord) then
                            outputFile.WriteLine(sprintf "<keyword> %s </keyword>" currentWord)
                        else 
                            outputFile.WriteLine(sprintf "<identifier> %s </identifier>" currentWord)
            
                    // handle integer contant
                    elif isdigit then 
                        while not(!currentChar = ' ') && not(!currentChar = '\n')  && not(isSymbol(!currentChar))  do 
                        // && not(isletter(currentWord))
                            //read the char
                            currentWord <- sprintf "%s%c" currentWord !currentChar
                            currentChar := readnext()
                        outputFile.WriteLine(sprintf "<integerConstant> %s </integerConstant>" currentWord)

                    // handle symbol and comments
                    elif issymbol then
                        if !currentChar = '<' then
                            currentWord <- (sprintf "&lt;")
                        elif !currentChar = '>' then
                            currentWord <- (sprintf "&gt;")
                        elif !currentChar = '&' then
                            currentWord <- (sprintf "&amp;")
                        else
                            currentWord <- (sprintf "%c" !currentChar)
                        currentChar := readnext()

                        if !currentChar = '/' then
                            while not(!currentChar = '\n') do
                                currentChar := readnext()

                        elif !currentChar = '*' then
                            let mutable endOfComment = false
                            while not(endOfComment) do
                                currentChar := readnext()
                                if !currentChar = '*' then
                                    currentChar := readnext()
                                    if !currentChar = '/' then
                                        endOfComment <- true
                            currentChar := readnext()
                        else        
                            outputFile.WriteLine(sprintf "<symbol> %s </symbol>" currentWord)

                    // handle string
                    elif !currentChar = '"' then 
                        currentChar := readnext()
                        while not(!currentChar = '"') do
                            currentWord <- sprintf "%s%c" currentWord !currentChar
                            currentChar := readnext()
                        currentChar := readnext()
                        outputFile.WriteLine(sprintf "<stringConstant> %s </stringConstant>" currentWord)

                    // handle space or new line or slash
                    else 
                        currentChar := readnext()

                outputFile.WriteLine("</tokens>")
                stream.Close() 
                outputFile.Close()
        