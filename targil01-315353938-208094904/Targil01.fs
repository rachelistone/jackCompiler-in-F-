//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

open System
open System.IO

//F:\Program Files\nand2tetris\projects\07\MemoryAccess\BasicTest
//F:\Program Files\nand2tetris\projects\07\MemoryAccess\PointerTest
//F:\Program Files\nand2tetris\projects\07\MemoryAccess\StaticTest
//F:\Program Files\nand2tetris\projects\07\StackArithmetic\SimpleAdd
//F:\Program Files\nand2tetris\projects\07\StackArithmetic\StackTest

[<EntryPoint>]
let main argv =
    let path = Console.ReadLine()

    //find the last directory of the path
    let arrOfPath = path.Split @"\" 
    let lastDirectory = arrOfPath.[arrOfPath.Length - 1]
    let fileName = lastDirectory + ".asm"

    //create outputFile 
    let outputFilePath = path + "\\" + fileName 
    let outputFile = new StreamWriter(outputFilePath, true)

    //finds all the files in path lasts in ".vm"
    let filesInDirectory = Directory.GetFiles(path, "*.vm") 

    //number of label in compare operation
    let numOfLabel = ref 0

//------------------------------------------ function that handle push commands -------------------------------------------

    let handlePush(segment : String, index : int, fileName : String) =
        let mutable segmentName = ""

        // function that translates command with the segments this, that, local, argument
        let handleSegmentDynamicAddress() = 
            outputFile.WriteLine(sprintf "@%i" index)
            outputFile.WriteLine("D=A")
            outputFile.WriteLine(sprintf "@%s" segmentName)
            outputFile.WriteLine("A=M+D")
            outputFile.WriteLine("D=M")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("A=M")
            outputFile.WriteLine("M=D")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("M=M+1")

        // function that translates command with the segments static, temp
        let handleSegmentStaticAddress(segAddress : String) = 
            outputFile.WriteLine(sprintf "@%s" segAddress)
            outputFile.WriteLine("D=M")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("A=M")
            outputFile.WriteLine("M=D")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("M=M+1")

        // function that translates command with the segment constant
        let handleVirtualSegment() =
            outputFile.WriteLine(sprintf "@%i" index)
            outputFile.WriteLine("D=A")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("A=M")
            outputFile.WriteLine("M=D")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("M=M+1") 

        let handlePointer() =
            if index = 0 then 
                outputFile.WriteLine("@THIS") 
            elif index = 1 then
                outputFile.WriteLine("@THAT") 
            outputFile.WriteLine("D=M")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("A=M")
            outputFile.WriteLine("M=D")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("M=M+1")

        match segment with
        | "argument" -> segmentName <- "ARG"
                        handleSegmentDynamicAddress()

        | "local" -> segmentName <- "LCL"
                     handleSegmentDynamicAddress()

        | "static" -> handleSegmentStaticAddress(sprintf "%s.%i" fileName index)

        | "constant" -> handleVirtualSegment()

        | "this" -> segmentName <- "THIS"
                    handleSegmentDynamicAddress()

        | "that" -> segmentName <- "THAT"
                    handleSegmentDynamicAddress()

        | "pointer" -> handlePointer()
                           
        | "temp" -> handleSegmentStaticAddress(sprintf "%i" (5 + index))

//----------------------------------------- function that handle pop commands --------------------------------------

    let handlePop(segment : String, index : int, fileName : String) =
         let mutable segmentName = ""

         // function that translates command with the segments this, that, local, argument
         let handleSegmentDynamic() =
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("A=M-1")
             outputFile.WriteLine("D=M")
             outputFile.WriteLine(sprintf "@%s" segmentName)
             outputFile.WriteLine("A=M")
             for i=1 to index do
                outputFile.WriteLine("A=A+1")
             outputFile.WriteLine("M=D")
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("M=M-1")

         // function that translates command with the segments static, temp
         let handleSegmentStatic(segAddress : String) = 
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("A=M-1")
             outputFile.WriteLine("D=M")
             outputFile.WriteLine(sprintf "@%s" segAddress)
             outputFile.WriteLine("M=D")
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("M=M-1")

         let handlePointer() = 
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("A=M-1")
             outputFile.WriteLine("D=M")
             if index = 0 then 
                 outputFile.WriteLine("@THIS")
             elif index = 1 then
                 outputFile.WriteLine("@THAT")
             outputFile.WriteLine("M=D")
             outputFile.WriteLine("@SP")
             outputFile.WriteLine("M=M-1")

         match segment with
         | "argument" -> segmentName <- "ARG"
                         handleSegmentDynamic()

         | "local" -> segmentName <- "LCL"
                      handleSegmentDynamic()

         | "static" -> handleSegmentStatic(sprintf "%s.%i" fileName index)

         | "this" -> segmentName <- "THIS"
                     handleSegmentDynamic()

         | "that" -> segmentName <- "THAT"
                     handleSegmentDynamic()

         | "pointer" -> handlePointer()
                        
         | "temp" -> handleSegmentStatic(sprintf "%i" (5 + index))

 //------------------------------------ function that handles commands add, sub, and, or --------------------------------
 
    let handleBinaryOp(op : String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine("D=M")
        outputFile.WriteLine("A=A-1")
        outputFile.WriteLine(sprintf "M=M%sD" op)
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=M-1")

 //---------------------------------------- function that handles commands neg, not --------------------------------------
 
    let handleUnaryOp(op : String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine(sprintf "M=%sM" op)

 //------------------------------------------function that handles commands eq, lt, gt ------------------------------------

    let handleCompare(op : String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine("D=M")
        outputFile.WriteLine("A=A-1")
        outputFile.WriteLine("D=M-D")
        outputFile.WriteLine(sprintf "@IF_TRUE%i" !numOfLabel)
        outputFile.WriteLine(sprintf "D;%s" op)
        outputFile.WriteLine("D=0")
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine("A=A-1")
        outputFile.WriteLine("M=D")
        outputFile.WriteLine(sprintf "@IF_FALSE%i" !numOfLabel)
        outputFile.WriteLine("0;JMP")
        outputFile.WriteLine(sprintf "(IF_TRUE%i)" !numOfLabel)
        outputFile.WriteLine("D=-1")
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine("A=A-1")
        outputFile.WriteLine("M=D")
        outputFile.WriteLine(sprintf "(IF_FALSE%i)" !numOfLabel)
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=M-1")
        numOfLabel := !numOfLabel + 1
  
 //-------------------------------------------------- Function ReadFile -------------------------------------------------------

    let ReadFile (filePath : String) =
        let currentFileName = Path.GetFileNameWithoutExtension filePath
        use stream = new StreamReader(new FileStream(filePath, FileMode.Open))
        while (not stream.EndOfStream) do
            let line = stream.ReadLine()
            let words = line.Split ' '
            if not(words.[0] = "//") then
                outputFile.WriteLine(sprintf "//*******************%s********************" line)
            match words.[0] with
            | "//" -> printfn "comment found"
            | "push" -> handlePush(words.[1], int words.[2], currentFileName)
            | "pop" -> handlePop(words.[1], int words.[2], currentFileName)
            | "add" -> handleBinaryOp("+")
            | "sub" -> handleBinaryOp("-")
            | "and" -> handleBinaryOp("&")
            | "or" -> handleBinaryOp("|")
            | "not" -> handleUnaryOp("!")
            | "neg" -> handleUnaryOp("-")
            | "eq" -> handleCompare("JEQ")
            | "gt" -> handleCompare("JGT")
            | "lt" -> handleCompare("JLT")
            | "" -> printfn "empty line"
        stream.Close()  

    //Loop over all files in path and do some actions
    for pathOfFile in filesInDirectory do
        outputFile.WriteLine(sprintf "//%s" (Path.GetFileNameWithoutExtension pathOfFile))
        ReadFile(pathOfFile)

    outputFile.Close()

    0 // return an integer exit code
