module NotenTrainer.Main

open Elmish
open Bolero
open Bolero.Html


type Model =
    {
        W:  float
        H:  float
        X0: float
        Y0: float
        Current : (int*int)[]
        Guess :   int[]
        NIter : int
        Iter  : int
    }


let initModel =
    {
        W = 400.
        H = 300.
        X0 = 10.
        Y0 = 70.
        Current = [| 2,0; 3,0; 6,0 |]//array.Empty<_>()
        Guess = array.Empty<int>()
        NIter = 10
        Iter  = 1
    }

let noteNames_english = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "B"
|]

let noteNames_german = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "H"
|]

let noteNames_italian = [|
    "Do"; "Re"; "Mi"; "Fa"; "Sol"; "La"; "Si"
|]


let noteNames = noteNames_german
//let noteNames = noteNames_italian
let yc (m:Model) (i:float) =
    let h = (m.H- 2.0*m.Y0)/2.0
    let gap = h/4.0
    m.Y0 + i*gap


let nodeCoord_Oct0 = [
    5.; 4.5; 4.; 3.5; 3.; 2.5; 2. 
]


let yi (m:Model) (i:int) = yc m (float i)

let update message model = 
    model


let inline hline (x:float) (y:float) len = 
    elt "line" ["x1" => x; "y1" => y; "x2" => x+len; "y2" => y; "stroke" => "black"; "stroke-width" => 2] []

let drawNote (model:Model) (nx:float) (nw:float) (nh:float) (inote:int) (ioctave:int) =
    let y = yc model nodeCoord_Oct0.[inote]
    elt "g" [] [
        elt "ellipse" [
            "cx"=> nx
            "cy"=> y
            "rx" => nw
            "ry" => nh
            "fill" => "black"
            "stroke" => "black"
            "stroke-width" => 2
        ][]
        elt "ellipse" [
            "cx"=> nx
            "cy"=> y
            "rx" => nh
            "ry" => nh
            "fill" => "white"
            "stroke" => "black"
            "stroke-width" => 2
        ][]
        cond (inote % 2 = 0) <| function
        | true -> hline (nx-nh) y (2.0*nh)
        | false -> empty
    ] 

let view (model:Model) dispatch =
    let vh,vw = model.H/2.0*0.9, model.H/3.0*0.9
    let yim = yi model
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

    concat [
        h1 [] [text "Notentrainer"]
        div [] [
            svg [
                attr.height model.H
                attr.width model.W
            ] [
                elt "image" [
                    attr.id "violin"
                    attr.href "violin.svg" 
                    "x"=> model.X0+10.; "y" => model.Y0 + ((yim 4) - (yim 0) - vh)/2.0
                    attr.height vh
                    attr.width  vw
                ][]
                forEach [0..4] <| fun i ->
                    hline model.X0 (yi model i) (model.W-2.0*model.X0)
                
                // draw note
                let nx, nh = model.X0 + model.W/2.0-20.0, (yim 1 - yim 0)*0.4
                let nw = nh*1.5
                forEach [0..model.Current.Length-1] <| fun k ->
                    let i,j = model.Current.[k]
                    drawNote model (nx+(float k)*nw*2.0) nw nh i j
            ]
        ]
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
                        on.click (fun _ -> printfn "clicked %s" c)
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
    ]


type MyApp() =
    inherit ProgramComponent<Model, string>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
