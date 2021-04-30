module NotenTrainer.Keyboard

open Bolero
open Bolero.Html

open NotenTrainer.Model

type Keyboard()=
    inherit ElmishComponent<Model,Message>()

    override _.ShouldRender(oldModel, newModel) =
        oldModel.W <> newModel.W
       
    override _.View model dispatch =
        let KW = int model.W
        let KH_w = 120
        let KW_w = min 45 (KW / 7)
        let KH_b = 80
        let KW_b = 20

        let style1 = sprintf @"cursor: default;
        font-size: 0px; height: %dpx;
        padding: 0px;
        position: relative;
        list-style: none;
        margin: 0px;
        width: %dpx;
        user-select: none;" KH_w KW

        let style_w is_last =  
            let right_border_width = if is_last then 1 else 0
            let right_border_style = if is_last then "solid" else "initial"
            let right_border_color = if is_last then "rgb(0, 0, 0)" else "initial"
            $@"display: inline-block;
            cursor:pointer; 
            user-select: none; 
            background-color: rgb(255, 255, 255); 
            border-width: 1px {right_border_width}px 1px 1px; 
            border-top-style: solid;
            border-right-style: {right_border_style}; 
            border-bottom-style: solid; 
            border-left-style: solid; 
            border-top-color: rgb(0, 0, 0); 
            border-right-color: {right_border_color}; 
            border-bottom-color: rgb(0, 0, 0); 
            border-left-color: rgb(0, 0, 0); 
            border-image: initial; 
            height: {KH_w}px; 
            width: {KW_w}px; 
            border-radius: 0px 0px 5px 5px;" 

        let style_b (left_pos:int) = 
            $@"display: inline-block; 
            user-select: none; 
            background-color: rgb(0, 0, 0); 
            border: 1px solid rgb(0, 0, 0); 
            position: absolute; 
            left: {left_pos}px; 
            width: {KW_b}px; 
            height: {KH_b}px; 
            border-radius: 0px 0px 3px 3px;"

        div[ 
            attr.id "keyboard" 
            attr.style style1 
        ] [
            ul [
                attr.style style1
            ] [
                forEach noteNames_german <| fun c ->
                    li [
                        attr.id c 
                        attr.title c 
                        "data-note-type" => "white" 
                        attr.style <| style_w (c="H")
                        on.click (fun _ -> NextGuess(c) |> dispatch)
                    ] []
                    
                forEach [("C#",1);("D#",2); ("F#",4); ("G#",5); ("A#",6)] <| fun (c, i) ->
                    li [
                        attr.id c 
                        attr.title c 
                        "data-note-type" => "black" 
                        attr.style <| style_b ((KW_w+1)*i - KW_b/2)
                    ] []
            ]
        ]
