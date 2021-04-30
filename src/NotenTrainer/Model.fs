module NotenTrainer.Model

open System

type Model =
    {
        W:  float
        H:  float
        X0: float
        Y0: float
        MaxIter : int

        StartTime : DateTime
        Current : (int*int)[]
        Guess :   int list
        Iter  : int
        NrCorrect : int
        LastGuess : bool
    }

type Message =
| NewGame
| NextTest
| NextGuess of note:string

let RND = Random()
let inline nextRnd lb ub = RND.Next(lb,ub)
let inline nextRandomNote () = (nextRnd 0 6), (nextRnd -1 2)

type Settings = {
    NrNotes:int
    MaxIter:int
    Language : string
}

let settings = {
    NrNotes = 2
    MaxIter = 10
    Language = "de"
}

let initModel () =
    let now = DateTime.Now
    {
        W = 400.
        H = 300.
        X0 = 10.
        Y0 = 70.

        StartTime = now
        Current = Array.init settings.NrNotes <| fun _ -> nextRandomNote()
        Guess = []
        MaxIter = settings.MaxIter
        Iter  = 0
        NrCorrect = 0
        LastGuess = false
    }

let checkGuess model =
    let guess = model.Guess |> List.rev |> List.toArray
    let test = model.Current |> Array.map fst
    let lastGuess = (guess=test)
    { model with
        Iter = model.Iter+1
        NrCorrect = model.NrCorrect + (if lastGuess then 1 else 0)
        LastGuess = lastGuess
    }

let noteNames_german = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "H"
|]
let noteNames_english = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "B"
|]


let noteNames_italian = [|
    "Do"; "Re"; "Mi"; "Fa"; "Sol"; "La"; "Si"
|]


let rec update (message:Message) (model:Model) =
    match message with
    | NewGame -> initModel()
    | NextTest -> 
        {model with 
            Current = Array.init settings.NrNotes <| fun _ -> nextRandomNote()
            Guess = []
        }
    | NextGuess(c) ->
        let i = noteNames_german |> Array.findIndex ((=) c)
        let newModel = { model with Guess = i::model.Guess }
        if newModel.Guess.Length = newModel.Current.Length then
            checkGuess newModel |> update NextTest
        else newModel


let noteNames = noteNames_german
