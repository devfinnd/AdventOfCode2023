open System.IO

type Set = { Red: int; Green: int; Blue: int }

type Game = { Id: int; Sets: Set seq }

let readInput (path: string) = File.ReadAllLines path |> Seq.ofArray

let trim (input: string) = input.Trim()

let parseId (header: string) =
    header.Substring("Game ".Length - 1) |> int

let parseCube (cube: string) =
    let parts = cube.Split ' ' |> Seq.map trim
    let color = Seq.last parts
    let amount = Seq.head parts |> int
    color, amount

let parseSet (set: string) =
    let cubes =
        set.Split ','
        |> Seq.map trim
        |> Seq.map parseCube
        |> Seq.groupBy fst
        |> Seq.map (fun (color, cubes) -> color, cubes |> Seq.maxBy snd |> snd)
        |> Map.ofSeq

    let filter color =
        cubes.TryFind color |> Option.defaultValue 0

    { Red = filter "red"
      Green = filter "green"
      Blue = filter "blue" }

let parseGame (input: string) =
    let parts = input.Split ':' |> Array.map trim
    let gameId = parts[0] |> parseId
    let sets = parts[1].Split ';' |> Seq.map trim |> Seq.map parseSet
    { Id = gameId; Sets = sets }

let isPossible (game: Game) =
    let maxRed = 12
    let maxGreen = 13
    let maxBlue = 14

    let isRedPossible (set: Set) = set.Red <= maxRed
    let isGreenPossible (set: Set) = set.Green <= maxGreen
    let isBluePossible (set: Set) = set.Blue <= maxBlue

    let isPossible (set: Set) =
        isRedPossible set && isGreenPossible set && isBluePossible set

    game.Sets |> Seq.forall isPossible


let solve (input: string seq) =
    input |> Seq.map parseGame |> Seq.filter isPossible |> Seq.sumBy (fun x -> x.Id)

readInput "input" |> solve |> printfn "Solution: %d"
