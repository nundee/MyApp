module NotenTrainer.Main

open Elmish
open Bolero
open Bolero.Html


type Model =
    {
        greeting: string
    }


let initModel =
    {
        greeting = "Notentrainer"
    }


let update message model = 
    model


let inline hline (x:float) (y:float) len = 
    elt "line" ["x1" => x; "y1" => y; "x2" => x+len; "y2" => y; "stroke" => "black"; "stroke-width" => 2] []

let view (model:Model) dispatch =
    let W,H = 400., 300.
    let X0,Y0 = 10., 10.
    let h = H/2.0
    let gap = h/4.0
    concat [
        h1 [] [text model.greeting]
        div [] [
            svg [
                attr.height H
                attr.width W

            ] [
                elt "image" [
                    attr.id "violin" 
                    attr.href "violin.svg" 
                    "x"=> X0+10.; "y" => Y0+10.
                    attr.height (H/2.0*0.9)
                    attr.width  (H/3.0*0.9)
                ][]
                forEach [0..4] <| fun i ->
                    hline X0 (Y0+(float i)*gap) (H-2.0*X0) 
            ]
        ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, string>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
