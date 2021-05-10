module Partitur

open Fable.React
open Fable.React.Props
open Elmish.React

open NotenTrainer.Model

let yc (m:Model) (i:float) =
    let h = (m.H- 2.0*m.Y0)/2.0
    let gap = h/4.0
    m.Y0 + i*gap


let node_iy (inote:int) (ioctave:int) off =
    let i = inote + ioctave*7 |> float
    off - i/2.0

let yi (m:Model) (i:int) = yc m (float i)


let inline hline (x:float) (y:float) len =
    line [X1  x; Y1 y; X2 (x+len); Y2 y; SVGAttr.Stroke "black"; SVGAttr.StrokeWidth  2] []

let inline empty ()= div [][]

let drawNote (model:Model) (nx:float) (nw:float) (nh:float) (inote:int) (ioctave:int) =
    let offset = 
        match model.CurrentKey with 
        | EKey.Violin -> 5.0
        | _ -> 2.5

    let iy = node_iy inote ioctave offset
    let y = yc model iy 
    g [] [
        yield ellipse [
            Cx nx
            Cy y
            Rx nw
            Ry nh
            SVGAttr.Fill "black"
            SVGAttr.Stroke "black"
            SVGAttr.StrokeWidth  2
        ][]
        yield ellipse [
            SVGAttr.Cx nx
            SVGAttr.Cy y
            SVGAttr.Rx nh
            SVGAttr.Ry nh
            SVGAttr.Fill "white"
            SVGAttr.Stroke "black"
            SVGAttr.StrokeWidth 2
        ][]
        if (abs (iy - (round iy)) < 0.1) then
            yield hline (nx-nh) y (2.0*nh) 
        else yield empty()

        if iy >= 5.0 then
            let mutable yy = floor iy
            while yy >= 5.0 do
                yield hline (nx-nw-3.0) (yc model yy) (2.0*nw+6.0)
                yy <- yy-1.0
        elif iy <= -1.0 then
            let mutable yy = ceil iy
            while yy <= -1.0 do
                yield hline (nx-nw-3.0) (yc model yy) (2.0*nw+6.0)
                yy <- yy+1.0
        else yield empty()
    ] 

let view model dispatch =
    let vh,vw = model.H/2.0*0.9, model.H/3.0*0.9
    let yim = yi model
    let divStyle = 
        if gameOver model then
            [Style [CSSProp.Background "radial-gradient(black, transparent)"] :> IHTMLProp]
        else []
    div divStyle [
        svg [
            SVGAttr.Height model.H
            SVGAttr.Width model.W
        ] [
            image [
                Id "violin"
                Href <| match model.CurrentKey with EKey.Violin -> "violin.svg" | _ -> "bass.svg"
                X  (model.X0+10.)
                Y  (model.Y0 + ((yim 4) - (yim 0) - vh)/2.0)
                SVGAttr.Height vh
                SVGAttr.Width  vw
            ][]
            for i=0 to 4 do hline model.X0 (yi model i) (model.W-2.0*model.X0)
            
            // draw note
            let nx, nh = model.X0 + model.W/2.0-20.0, (yim 1 - yim 0)*0.4
            let nw = nh*1.5
            for k=0 to model.Current.Length-1 do
                let i,j = model.Current.[k]
                drawNote model (nx+(float k)*nw*2.0) nw nh i j
        ]
    ]

let makeView model dispatch =
    lazyView2 view model dispatch