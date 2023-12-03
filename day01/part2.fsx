open System
open System.IO

let readInput (path: string) = File.ReadAllLines path |> Seq.ofArray

let charToDigit (c: char) = int c - 48

let lookup =
    [ ("1", 1)
      ("2", 2)
      ("3", 3)
      ("4", 4)
      ("5", 5)
      ("6", 6)
      ("7", 7)
      ("8", 8)
      ("9", 9)
      ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]
    |> Map.ofList

let windowed (line: string) =
    seq {
        for i in 0 .. line.Length - 1 do
            yield line.Substring(i)
    }

let getDigit (line: string) : int Option =
    if line.Length = 1 then
        lookup.TryFind line
    else
        match lookup.Keys |> Seq.tryFind line.StartsWith with
        | Some key -> lookup.TryFind key
        | None -> None

let findNumber (line: string) =
    let digits = windowed line |> Seq.choose getDigit
    let ten = Seq.head digits |> (*) 10
    let one = Seq.last digits
    ten + one

readInput "input" |> Seq.map findNumber |> Seq.sum |> printfn "%d"
