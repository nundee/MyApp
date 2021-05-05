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
    let flags = [
        "albania.png","shqip"
        "united-kingdom.png","english"
        "italy.png","italiano"
        "germany.png","deutsch"
    ]
    concat [
        div [attr.style "display: flex; justify-content: flex-end; margin-right: 20px;"] [
            button ["class"=>"btn"; attr.style "margin-right: 20px;"] [
                img [attr.src "reload_icon.png"; attr.width "32px"; attr.height "32px"]
            ]
            forEach flags <| fun (f,t)->
                button ["class"=>"btn"; attr.title t] [img [attr.src f; attr.width "32px"; attr.height "32px"]]
        ]
        h1 [] [text "Notentrainer"]
        ecomp<Partitur,_,_> [] model <| dispatch
        ecomp<Keyboard,_,_> [] model <| dispatch 
        ecomp<Stats,_,_>    [] model <| dispatch
        cond (gameOver model) <| function
        | true -> 
            div [attr.style @"
                position:absolute;
                left: 0px;top: 50px;
                width: 100%;height: 100%;
                background: rgba(0,0,0,0.5);
                z-index:10;
                display: flex;justify-content: center;align-items: center;"
            ] [
                h1 [attr.style "color: orange;background: rgb(0,0,0,0.5);"] [
                    text "GAME OVER"
                ]
            ]
        | false -> empty
    ]


type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel ()) update view
        |> Program.withConsoleTrace
        |> Program.withErrorHandler (fun (msg, exn) -> printfn "%A: %A" msg exn)
