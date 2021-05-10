module App
   
open Elmish
open Elmish.React

let init () =
    NotenTrainer.Model.initModel(), Cmd.none

let update message model =
    let newModel = NotenTrainer.Model.update message model
    newModel, Cmd.none

Main.loadSettings()
Program.mkProgram init update Main.view
|> Program.withReactSynchronous "main"
|> Program.run