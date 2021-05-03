module NotenTrainer.Main

open System
open Elmish
open Bolero
open Bolero.Html


open NotenTrainer.Model
open NotenTrainer.Partitur
open NotenTrainer.Keyboard
open NotenTrainer.Stats

let view (model:Model) dispatch =
    concat [
        h1 [] [text "Notentrainer"]
        ecomp<Partitur,_,_> [] model <| dispatch
        ecomp<Keyboard,_,_> [] model <| dispatch 
        ecomp<Stats,_,_>    [] model <| dispatch 
    ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel ()) update view
        |> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%A: %A" msg exn)
