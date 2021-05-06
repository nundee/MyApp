module NotenTrainer.Stats

open System
open Bolero
open Bolero.Html

open NotenTrainer.Model
open NotenTrainer.Lang

type Stats()=
    inherit ElmishComponent<Model,Message>()

    override _.ShouldRender(oldModel, newModel) =
        oldModel.Language <> newModel.Language || 
        newModel.Guess.Length < 1
       
    override _.View model dispatch =
        let msg  = getStatMessage model.Language
        p [] [
            ul [
                attr.style @"
                    list-style: none;
                    font-size: medium;
                    font-family: cursive;"
            ] [ 
                let first_item = li [] [text $"{msg EStatMsg.IterLeft} {model.MaxIter - model.Iter}"]
                cond (model.Iter>0) <| function
                | false -> first_item
                | true -> 
                    concat [
                        li [] [text $"{msg EStatMsg.ElapsedTime}: {(DateTime.Now-model.StartTime).Seconds}s"]
                        first_item
                        li [] [text $"{msg EStatMsg.Correct} {System.Math.Round((float model.NrCorrect)/(float model.Iter)*100.0,1)} %%"]
                        li [ attr.style (if model.LastGuess then "color:green;" else "color:red;")] [
                                text <| if model.LastGuess then msg EStatMsg.LastGuessOK else msg EStatMsg.LastGuessWrong
                           ]
                    ]
            ]
        ]
