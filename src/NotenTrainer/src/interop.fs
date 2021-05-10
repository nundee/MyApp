module MyInterop
open Fable.Core
open Browser


let saveSettings x =
    localStorage.setItem ("NotenTrainer", JS.JSON.stringify x)

let loadSettings () =
    localStorage.getItem ("NotenTrainer") |> JS.JSON.parse
    