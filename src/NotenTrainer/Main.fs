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
                
                let x0,y0, rw, rh  = model.X0+20. , yim 8, 45.0, 60.0
                forEach [0..6] <| fun i->
                    elt "rect" [
                        "x"=> x0 + (float i)*rw
                        "y"=> y0
                        "rx" => 3
                        "ry" => 3
                        "width" => rw
                        "height" => rh
                        "fill" => "white"
                        "stroke" => "black"
                        "stroke-width" => 1
                        attr.style "cursor:pointer"
                        on.click (fun _ -> printfn "clicked %d" i)
                    ][]
                forEach [0..6] <| fun i->
                    elt "text" [
                        "x"=> x0 + (float i)*rw + rw/2.0
                        "y"=> y0 + rh/2.0
                        "fill" => "black"
                        "text-anchor" => "middle" 
                        "alignment-baseline" => "central"
                        "font-family" => "Courier-New"
                        "font-size" => 12
                        attr.style "cursor:pointer"
                    ][ text noteNames.[i]]
                let brw, brh = 16.0, 30 in
                forEach [1;2;4;5;6] <| fun i->
                    elt "rect" [
                        "x"=> x0 + (i |> float)*rw - (brw/2.0)
                        "y"=> y0
                        "rx" => 3
                        "ry" => 3
                        "width" => brw
                        "height" => brh
                        "fill" => "black"
                        "stroke" => "none"
                    ][]

                // draw note
                let nx, nh = model.X0 + model.W/2.0-20.0, (yim 1 - yim 0)*0.4
                let nw = nh*1.5
                forEach [0..model.Current.Length-1] <| fun k ->
                    let i,j = model.Current.[k]
                    drawNote model (nx+(float k)*nw*2.0) nw nh i j
            ]
        ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, string>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
