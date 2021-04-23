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
        Current : string[]
        Guess :   string[]
        NIter : int
        Iter  : int
    }


let initModel =
    {
        W = 400.
        H = 300.
        X0 = 10.
        Y0 = 10.
        Current = array.Empty<string>()
        Guess = array.Empty<string>()
        NIter = 10
        Iter  = 1
    }

    //    let h = H/2.0
    //    let gap = h/4.0
        
    //}

let yi (m:Model) (i:int) =
    let h = m.H/2.0
    let gap = h/4.0
    m.Y0 + (float i)*gap

let update message model = 
    model


let inline hline (x:float) (y:float) len = 
    elt "line" ["x1" => x; "y1" => y; "x2" => x+len; "y2" => y; "stroke" => "black"; "stroke-width" => 2] []

let view (model:Model) dispatch =

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
                    "x"=> model.X0+10.; "y" => model.Y0+10.
                    attr.height (model.H/2.0*0.9)
                    attr.width  (model.H/3.0*0.9)
                ][]
                forEach [0..4] <| fun i ->
                    hline model.X0 (yi model i) (model.H-2.0*model.X0) 
            ]
        ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, string>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
