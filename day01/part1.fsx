open System
open System.IO

let readInput (path: string) =
    File.ReadAllLines path |> Seq.ofArray

let charToDigit (c: char) =
    int c - 48

let findNumber (line: string) =
    let digits = line.ToCharArray() |> Seq.where Char.IsDigit
    let ten = Seq.head digits |> charToDigit |> (*) 10
    let one = Seq.last digits |> charToDigit
    ten + one

readInput "input" |> Seq.map findNumber |> Seq.sum |> printfn "%d"
