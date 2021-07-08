//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

open System
open System.IO

//F:\Program Files\nand2tetris\projects\08\ProgramFlow\FibonacciSeries
//F:\Program Files\nand2tetris\projects\08\ProgramFlow\BasicLoop
//F:\Program Files\nand2tetris\projects\08\FunctionCalls\SimpleFunction
//F:\Program Files\nand2tetris\projects\08\FunctionCalls\StaticsTest
//F:\Program Files\nand2tetris\projects\08\FunctionCalls\NestedCall
//F:\Program Files\nand2tetris\projects\08\FunctionCalls\FibonacciElement

[<EntryPoint>]
let main argv =
    let path = Console.ReadLine()

    //find the last directory of the path
    let arrOfPath = path.Split @"\" 
    let lastDirectory = arrOfPath.[arrOfPath.Length - 1]
    let fileName = lastDirectory + ".asm"

    //create outputFile 
    let outputFilePath = path + "\\" + fileName 
    let outputFile = new StreamWriter(outputFilePath, false)

    //inds all the files in path lasts in ".vm"
    let filesInDirectory = Directory.GetFiles(path, "*.vm") 

    //number of label in compare operation
    let numOfLabel = ref 0

    //global counter to distinguish between multipile calls to the same function
    let numOfFunctionCalls = ref 0
    

//------------------------------------------ function that handle push commands ------------------------------------

    // push value to the stack from the given "segment" in offset of the given "index" in that segment.
    // the file name is for the static segment, to put it in the area of this file's statics.
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

    // pop value from the stack and puts it in the given "segment" with offset of the given "index" in that segment.
    // the file name is for the static segment, to put it in the area of this file's statics.
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

 //------------------------------------ function that handles commands add, sub, and, or ---------------------------
 
    // make the "op" operation between two values from the head of stack and put the result at the head.
    let handleBinaryOp(op : String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine("D=M")
        outputFile.WriteLine("A=A-1")
        outputFile.WriteLine(sprintf "M=M%sD" op)
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=M-1")

 //---------------------------------------- function that handles commands neg, not --------------------------------
 
    // make the "op" operation on the value at the head of the stack.
    let handleUnaryOp(op : String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("A=M-1")
        outputFile.WriteLine(sprintf "M=%sM" op)

 //------------------------------------------function that handles commands eq, lt, gt -----------------------------

    // if the two values at the head trued the "op" relation then put -1 at the head, else put 0.
    // it subsuructs at the hed from the value before and compare it according yo op with 0.
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

// -------------------------------------------------- functions that handles jump ----------------------------------

    // create label
    let handleLabel( labelName: String, currentFileName: String) = 
        outputFile.WriteLine(sprintf "(%s.%s)" currentFileName labelName)

    //goto labelName - unconditional jump
    let handleJump(labelName: String, currentFileName: String) =
        outputFile.WriteLine(sprintf "@%s.%s" currentFileName labelName)
        outputFile.WriteLine("0;JMP")

    //if-goto labelName - if the head of stack doesn't contain 0, than jump to label
    let handleJumpCondition(labelName: String, currentFileName: String) =
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=M-1") //pop the value 
        outputFile.WriteLine("A=M")
        outputFile.WriteLine("D=M")
        outputFile.WriteLine(sprintf "@%s.%s" currentFileName labelName)
        outputFile.WriteLine("D;JNE")
       
// --------------------------------- functions that handles function definition, call and return -----------------

    // call funcName, numOfArgs - save the state of previous function at the stack before jumping to the function definition,
    // update the addresses of the segments to the caled's use, and jump there.
    let handleCall(funcName: String, numOfArgs:int) =

        // push the address to return in the calling function, to the stack, so that after the jump to the callee, it will come back to after the calling. 
        // if there is two calls for same func - the global counter distinguishes them
        outputFile.WriteLine(sprintf "@%s.ReturnAddress%i" funcName !numOfFunctionCalls)
        outputFile.WriteLine("D=A")
        outputFile.WriteLine("@SP") 
        outputFile.WriteLine("A=M")
        outputFile.WriteLine("M=D")
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=M+1")

        // push segments (lcl, arg..) state (addresses) of the calling function
        let saveSegmentAddress(segName:String) = 
            outputFile.WriteLine(sprintf "@%s" segName)
            outputFile.WriteLine("D=M")
            outputFile.WriteLine("@SP") 
            outputFile.WriteLine("A=M")
            outputFile.WriteLine("M=D")
            outputFile.WriteLine("@SP")
            outputFile.WriteLine("M=M+1")

        saveSegmentAddress("LCL")
        saveSegmentAddress("ARG")
        saveSegmentAddress("THIS")
        saveSegmentAddress("THAT")

        // update LCL of the called function to @SP, to allocate the locals after the saved states of calling function
        outputFile.WriteLine("@SP") 
        outputFile.WriteLine("D=M")
        outputFile.WriteLine("@LCL")
        outputFile.WriteLine("M=D")

        // update ARG of the called function, where all the arguments the calling function pushed.  @SP-5-numOfArgs
        outputFile.WriteLine("@SP") 
        outputFile.WriteLine("D=M")
        outputFile.WriteLine(sprintf "@%i" (numOfArgs + 5))
        outputFile.WriteLine("D=D-A")
        outputFile.WriteLine("@ARG")
        outputFile.WriteLine("M=D")

        // jump to execute called function code
        outputFile.WriteLine(sprintf "@%s" funcName)
        outputFile.WriteLine("0;JMP")

        // label of the return address (after the "call" comand)
        outputFile.WriteLine(sprintf "(%s.ReturnAddress%i)" funcName !numOfFunctionCalls)
        numOfFunctionCalls := !numOfFunctionCalls + 1
    
    // function funcName, numOfLocals - allocate in stack numOfLocals places, and initalize it to zero.
    // it creates an entry label before the init. and the def. of the function, to jump here when this function is called
    let handleFunction(funcName: String, numOfLocals:int) =
        outputFile.WriteLine(sprintf "(%s)" funcName) 
        for _ in 1..numOfLocals do                   
            handlePush("constant", 0, "")
    
    // return - restore the calling function's states and jump to the return address in the caller
    // set the stack pointer to the returned value from that call, where the caller inserted the args of the called
    let handleReturn() = 

         //save current LCL address in temp variable in order to restore the segments of the calling function
         outputFile.WriteLine("@LCL")
         outputFile.WriteLine("D=M")
         outputFile.WriteLine("@13")
         outputFile.WriteLine("M=D")

         // function that restore segments of the calling function and save return address in temp, to jump there after all.
         // offset is how much cells in stack to move back in order to get the appropriate segmentName addres
         let copyAddressToSegment(offset: int, segmentName:String) =
             outputFile.WriteLine("@13")
             outputFile.WriteLine("D=M")
             outputFile.WriteLine(sprintf "@%i" offset)
             outputFile.WriteLine("A=D-A" )
             outputFile.WriteLine("D=M")
             outputFile.WriteLine(sprintf "@%s" segmentName)
             outputFile.WriteLine("M=D")

         copyAddressToSegment(5, "14") // save return address in temp variable (the gloobal area in addres 14 at RAM.

         // move the returned value to the begining of ARG segment (we've done with that function's argument) - returned value is now in head of stack
         outputFile.WriteLine("@SP")
         outputFile.WriteLine("A=M-1")
         outputFile.WriteLine("D=M")
         outputFile.WriteLine("@ARG")
         outputFile.WriteLine("A=M")
         outputFile.WriteLine("M=D")

         // restore previous (caller's) SP - move address of SP to ARG+1 (where the returned value located)
         outputFile.WriteLine("@ARG")
         outputFile.WriteLine("D=M+1")
         outputFile.WriteLine("@SP")
         outputFile.WriteLine("M=D")

         copyAddressToSegment(1, "THAT") // restore THAT of the caller
         copyAddressToSegment(2, "THIS") // restore THIS of the caller
         copyAddressToSegment(3, "ARG")  // restore ARG of the caller
         copyAddressToSegment(4, "LCL")  // restore LCL of the caller

         // jump to return address - jump to the caller code (saved in the globals from the stack)
         outputFile.WriteLine("@14")
         outputFile.WriteLine("A=M")
         outputFile.WriteLine("0;JMP")

 //-------------------------------------------------- Function ReadFile -------------------------------------------------------

    // read from the opened vm file and route each line to the appropriate function according to the first word.
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
            | "label" -> handleLabel(words.[1], currentFileName)             //handleLabel(labelName, currentFileName)
            | "goto" -> handleJump(words.[1], currentFileName)               //handleJump(labelName, currentFileName)
            | "if-goto" -> handleJumpCondition(words.[1], currentFileName)  // handleJumpCondition(labelName, currentFileName)  
            | "call" -> handleCall(words.[1], int words.[2])                //handleCall(funcName, numOfArgs)
            | "function" -> handleFunction(words.[1], int words.[2])        //handleFunction(funcName, numOfLocals)
            | "return" -> handleReturn() 
            | "" -> printfn "empty line"
        stream.Close()  
    
    // if there are multipile vm files in that library, then initialize the stack address to the init function and call it at the begining.
    if (filesInDirectory.Length > 1) then
        outputFile.WriteLine("@256")
        outputFile.WriteLine("D=A")
        outputFile.WriteLine("@SP")
        outputFile.WriteLine("M=D")
        handleCall("Sys.init", 0)

    // Loop over all files in path, writes its name at top and then the translated commends.
    for pathOfFile in filesInDirectory do
        outputFile.WriteLine(sprintf "//%s" (Path.GetFileNameWithoutExtension pathOfFile))
        ReadFile(pathOfFile)

    outputFile.Close()

    0 // return an integer exit code
