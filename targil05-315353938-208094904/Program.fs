//group 43
//Michal Yona 208094904
//Rachel Stone 315353938

namespace JackCompiler

open System
open System.IO
open Tokening
open Parsing

//F:\Program Files\nand2tetris\projects\10\ExpressionLessSquare
//F:\Program Files\nand2tetris\projects\10\Square
//F:\Program Files\nand2tetris\projects\10\ArrayTest

module Program = 

    [<EntryPoint>]
    let main argv =

        let path = Console.ReadLine()

        tokening(path)

        parsing(path)

        0 // return an integer exit code
