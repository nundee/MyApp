module NotenTrainer.Model

open System

type EKey=
    | Violin = 0
    | Bass   = 1
type Model =
    {
        W:  float
        H:  float
        X0: float
        Y0: float
        MaxIter : int

        StartTime : DateTime
        CurrentKey : EKey
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
let inline nextRandomNote () = (nextRnd 0 7), (nextRnd -1 2)
let inline nextNotes n = 
    Array.init (2*n) <| fun _ -> nextRandomNote()
    |> Array.distinct
    |> Array.take n
let inline nextKey () = nextRnd 0 2 |> enum<EKey>

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
        CurrentKey = nextKey()
        Current = nextNotes settings.NrNotes
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
            CurrentKey = nextKey()
            Current = nextNotes settings.NrNotes
            Guess = []
        }
    | NextGuess(c) ->
        let i = noteNames_german |> Array.findIndex ((=) c)
        let newModel = { model with Guess = i::model.Guess }
        if newModel.Guess.Length = newModel.Current.Length then
            checkGuess newModel |> update NextTest
        else newModel


let noteNames = noteNames_german
