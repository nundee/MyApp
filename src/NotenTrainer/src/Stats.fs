module Stats

open System
open Fable.React
open Fable.React.Props
open Elmish.React

open NotenTrainer.Model
open NotenTrainer.Lang


let shouldRender (oldModel:Model) (newModel:Model) =
    oldModel.Language <> newModel.Language || 
    newModel.Guess.Length < 1

let view (model:Model) dispatch =
    let msg  = getStatMessage model.Language
    let listItems = 
        let first_item = li [] [str $"{msg EStatMsg.IterLeft} {model.MaxIter - model.Iter}"]
        if (model.Iter<1) then [first_item] else
        [
            li [] [str $"{msg EStatMsg.ElapsedTime}: {elapsedTime model}s"]
            first_item
            li [] [str $"{msg EStatMsg.Correct} {System.Math.Round((float model.NrCorrect)/(float model.Iter)*100.0,1)} %%"]
            li [ Style [CSSProp.Color (if model.LastGuess then "green" else "red")] ] [
                    str <| if model.LastGuess then msg EStatMsg.LastGuessOK else msg EStatMsg.LastGuessWrong
               ]
        ]
    div [Style [CSSProp.MarginTop 20]] [
        ul [
            Style [
                CSSProp.ListStyle  "none"
                CSSProp.FontSize   "medium"
                CSSProp.FontFamily "cursive"
            ]
        ]  listItems        
    ]

let makeView model dispatch =
    lazyView2With (fun m1 m2 -> shouldRender m1 m2 |> not) view model dispatch