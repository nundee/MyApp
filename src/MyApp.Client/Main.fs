module MyApp.Client.Main

open Elmish
open Bolero
open Bolero.Html


type Model =
    {
        greeting: string
        dg:DataGrid.GridModel
    }

open DataGrid

let initModel =
    {
        greeting = "Allo ales"
        dg={
            Title = "Table caption gaga"
            TitleEditing=false
            Columns = RowModel.New 
                        [|  "Nr."
                            "c1"
                            "c2"
                            "c3"
                            "c4"
                            "c5"
                        |]
            Rows = Array.init 10 (fun i-> RowModel.New [| i.ToString(); "a1"; "a2"; "a3"; "a4"; "a5"|]) |> ResizeArray
        }
    }


let update message model = 
    { model with dg=DataGrid.update message model.dg}

let view (model:Model) dispatch =
    concat [
        h1 [] [text model.greeting]
        DataGrid.view model.dg dispatch
        ecomp<Plotly.Chart,_,_> [
            "DivStyle" => "width:600px;height:500px;"
        ] {
            Data= [|
                {|
                    Type = "scatter"
                    x = [|0; 1; 2; 3; 4; 5|]
                    y =  [|1.5; 1.0; 1.3; 0.7; 0.8; 0.9|]
                    //name = @"$\alpha_{1c} = 352 \pm 11 \text{ km s}^{-1}$"
                    name = "koqe"
                |}
                {|
                    Type = "bar"
                    x = [|0; 1; 2; 3; 4; 5|]
                    y = [|1.0; 0.5; 0.7; -1.2; 0.3; 0.4|]
                |}
            |]
            Layout={| 
                    title = "Qofte"
                    xaxis = {| title = @"zis is X" |}
                    //showlegend= false
            |}
          } 
          (fun x->())
        
        //    "layout" => layout 
        //    attr.style "height: 60vh; width=600px; min-height: 350px"
            
        //] [
        //]
    ]


type MyApp() =
    inherit ProgramComponent<Model, DataGrid.Message>()

    override this.Program =
        MyInterop.JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
