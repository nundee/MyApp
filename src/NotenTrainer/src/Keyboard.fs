module Keyboard

open NotenTrainer.Model
open NotenTrainer.Lang
open Elmish.React

open Fable.Core
open Fable.React.Standard
open Fable.React.Props

let shouldRender oldModel newModel =
        oldModel.W <> newModel.W ||
        oldModel.Language <> newModel.Language

let view model dispatch =
    let KW = int model.W
    let KH_w = 120
    let KW_w = min 45 (KW / 7)
    let KH_b = 80
    let KW_b = 20

    let style1 = [
        CSSProp.Cursor "default"
        CSSProp.FontSize "0px" 
        CSSProp.Height $"{KH_w}px"
        CSSProp.Padding "0px"
        CSSProp.Position PositionOptions.Relative
        CSSProp.ListStyle "none"
        CSSProp.Margin "0px"
        CSSProp.Width $"{KW}px"
        CSSProp.Custom("userSelect","none")
    ]

    let style_w is_last = [
        let right_border_width = if is_last then 1 else 0
        let right_border_style = if is_last then "solid" else "initial"
        let right_border_color = if is_last then "rgb(0, 0, 0)" else "initial"
        CSSProp.Display DisplayOptions.InlineBlock
        CSSProp.Cursor  "pointer"
        CSSProp.Custom("userSelect","none")
        CSSProp.BackgroundColor "rgb(255, 255, 255)"
        CSSProp.BorderWidth $"1px {right_border_width}px 1px 1px" 
        CSSProp.BorderTopStyle "solid"
        CSSProp.BorderRightStyle $"{right_border_style}" 
        CSSProp.BorderBottomStyle "solid"
        CSSProp.BorderLeftStyle "solid" 
        CSSProp.BorderTopColor "rgb(0, 0, 0)"
        CSSProp.BorderRightColor $"{right_border_color}" 
        CSSProp.BorderBottomColor "rgb(0, 0, 0)"
        CSSProp.BorderLeftColor "rgb(0, 0, 0)" 
        CSSProp.BorderImage "initial" 
        CSSProp.Height $"{KH_w}px" 
        CSSProp.Width $"{KW_w}px" 
        CSSProp.BorderRadius "0px 0px 5px 5px" 
    ]


    let style_b (left_pos:int) = [
        CSSProp.Display DisplayOptions.InlineBlock
        CSSProp.Custom("userSelect","none") 
        CSSProp.BackgroundColor "rgb(0, 0, 0)" 
        CSSProp.Border "1px solid rgb(0, 0, 0)" 
        CSSProp.Position PositionOptions.Absolute 
        CSSProp.Left $"{left_pos}px" 
        CSSProp.Width $"{KW_b}px" 
        CSSProp.Height $"{KH_b}px" 
        CSSProp.BorderRadius "0px 0px 3px 3px"
    ]

    let noteNames = getNoteNames model.Language
    let lastNote = Array.last noteNames
    printfn "notes: %A" noteNames

    div [Id "keyboard"; Style style1] [
        ul [Style style1] [
            for c in noteNames do
                li [
                    Id c 
                    Title c 
                    Style <| style_w (c = lastNote)
                    OnClick (fun _ -> NextGuess(c) |> dispatch)
                ] []
                
            for (c,i) in [("C#",1);("D#",2); ("F#",4); ("G#",5); ("A#",6)] do
                li [
                    Id c 
                    Title c 
                    Style <| style_b ((KW_w+1)*i - KW_b/2)
                ] []
        ]
    ]


let makeView model dispatch =
    lazyView2With (fun m1 m2 -> shouldRender m1 m2 |> not) view model dispatch