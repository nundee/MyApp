module NotenTrainer.Stats

open System
open Bolero
open Bolero.Html

open NotenTrainer.Model

type Stats()=
    inherit ElmishComponent<Model,Message>()

    override _.ShouldRender(oldModel, newModel) =
        oldModel <> newModel
       
    override _.View model dispatch =
        p [] [
            ul [
                attr.style @"
                    list-style: none;
                    font-size: medium;
                    font-family: cursive;"
            ] [ 
                let first_item = li [] [text $"left {model.MaxIter - model.Iter}"]
                cond (model.Iter>0) <| function
                | false -> first_item
                | true -> 
                    concat [
                        li [] [text $"elapsed time: {(DateTime.Now-model.StartTime).Seconds} seconds"]
                        first_item
                        li [] [text $"correct {System.Math.Round((float model.NrCorrect)/(float model.Iter)*100.0,1)} %%"]
                        li [ attr.style (if model.LastGuess then "color:green;" else "color:red;")] [
                                text <| sprintf "last guess was %s" (if model.LastGuess then "correct" else "wrong")
                           ]
                    ]
            ]
        ]
