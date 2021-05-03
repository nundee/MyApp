module NotenTrainer.Partitur

open Bolero
open Bolero.Html

open NotenTrainer.Model

let yc (m:Model) (i:float) =
    let h = (m.H- 2.0*m.Y0)/2.0
    let gap = h/4.0
    m.Y0 + i*gap


let node_iy (inote:int) (ioctave:int)=
    let i = inote + ioctave*7 |> float
    5.0 - i/2.0

let yi (m:Model) (i:int) = yc m (float i)



let inline hline (x:float) (y:float) len =
    elt "line" ["x1" => x; "y1" => y; "x2" => x+len; "y2" => y; "stroke" => "black"; "stroke-width" => 2] []

let drawNote (model:Model) (nx:float) (nw:float) (nh:float) (inote:int) (ioctave:int) =
    let iy = node_iy inote ioctave
    let y = yc model iy
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
        cond (abs (iy - (round iy)) < 0.1) <| function
        | true -> hline (nx-nh) y (2.0*nh)
        | false -> empty

        if iy >= 5.0 then
            let mutable yy = floor iy
            concat [
                while yy >= 5.0 do
                    yield hline (nx-nw-3.0) (yc model yy) (2.0*nw+6.0)
                    yy <- yy-1.0
            ]
        elif iy <= -1.0 then
            let mutable yy = ceil iy
            concat [
                while yy <= -1.0 do
                    yield hline (nx-nw-3.0) (yc model yy) (2.0*nw+6.0)
                    yy <- yy+1.0
            ]
        else empty

    ] 

type Partitur()=
    inherit ElmishComponent<Model,Message>()

    override _.View model dispatch =
        let vh,vw = model.H/2.0*0.9, model.H/3.0*0.9
        let yim = yi model
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
