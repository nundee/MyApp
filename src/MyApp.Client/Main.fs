module MyApp.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Plotly.Blazor
open Plotly.Blazor.LayoutLib
open Plotly.Blazor.Traces.ScatterLib

open Microsoft.JSInterop

type JS() =    
    static member val Runtime = Unchecked.defaultof<IJSRuntime>  with get, set
    static member log([<System.ParamArray>] args:obj[]) =
        JS.Runtime.InvokeVoidAsync("console.log",args) |> ignore
    static member exec(func:string, [<System.ParamArray>] args:obj[])=
        JS.Runtime.InvokeVoidAsync(func, args) |> ignore
    static member eval(func:string, [<System.ParamArray>] args:obj[])=
        let task=JS.Runtime.InvokeAsync(func, args).AsTask()
        task.RunSynchronously()
        task.Result


module DataGrid =
    type RowModel = {
        Cells: string[]
        CellEditingIndex: int
        Changed : bool
    }
    with
        static member New(cells) = {Cells=cells; CellEditingIndex= -1; Changed=false}
        member this.setEditingIndex(i) = {this with CellEditingIndex=i; Changed=true}
        member this.setCell(i, s) =  
            this.Cells.[i] <- s
            {this with CellEditingIndex = -1; Changed = true }

    type GridModel = { 
        Title: string
        TitleEditing: bool
        Columns: RowModel
        Rows: ResizeArray<RowModel>
    } 
    with 
        member this.setTitle(t) = {this with Title=t; TitleEditing=false}
        member this.editTitle(b) = {this with TitleEditing=b}

    type Message =
        | Ping
        | EditTitle of bool
        | SetTitle of string
        | SetRow of RowModel*int
        //| EditCell of RowModel * int
        //| SetCell of RowModel * int * string
       
    type MyInput()=
        inherit ElmishComponent<string,string>()
        let inputRef = HtmlRef()
        let mutable changed=false

        //override this.ShouldRender(oldModel, newModel) =
        //    oldModel <> newModel

        override this.View model dispatch =
            input [
                attr.value model
                attr.ref inputRef
                bind.change.string model (fun t-> changed <- true; dispatch t)
                on.focusout (fun _ -> if (not changed) then dispatch model)
            ]
        override this.OnAfterRenderAsync(firstRender:bool)=
            JS.exec("MyJsLib.focus",inputRef.Value)
            base.OnAfterRenderAsync(firstRender)

    let rnd=System.Random()

    type Row()=
        inherit ElmishComponent<RowModel, RowModel>()

        override this.ShouldRender(oldModel, newModel) =
            //JS.log("should render", "old", oldModel.Changed, "new", newModel.Changed)
            newModel.Changed

        override this.View model dispatch =
            //JS.log("render row", model)
            tr [attr.style <| sprintf "background-color:#77%06x" (rnd.Next(0xFFFFFE))] [                
                forEach [|0..model.Cells.Length-1|] <| fun i -> 
                    td [                        
                        on.dblclick (fun _-> 
                            if i <> model.CellEditingIndex then 
                                //JS.log("dblclick",i)
                                printfn "dblclick %d" i
                                let m=model.setEditingIndex(i)
                                JS.log(m) |> ignore
                                dispatch m
                        )
                    ] [
                        cond (i=model.CellEditingIndex) <| function
                        | true ->  ecomp<MyInput,_,_> [] 
                                    model.Cells.[i] (fun s -> 
                                        let m=model.setCell(i,s)
                                        JS.log("done editing",m)
                                        dispatch m
                                    )
                        | false -> text model.Cells.[i]
                    ]
            ]


    let view model dispatch =        
        div [attr.style "width:80%; height:500px; overflow-y: auto; overflow-x: auto;"] [
           
            table [attr.style "width:100%"] [
                caption [
                    on.dblclick (fun _ -> if (not model.TitleEditing) then dispatch (EditTitle true))
                ] [
                    cond model.TitleEditing <| function
                    | true -> ecomp<MyInput,_,_> [] model.Title (dispatch << SetTitle)
                    | false -> text model.Title
                ]
                
                thead [] [
                    tr [] [
                        forEach model.Columns.Cells <| fun c -> th [attr.style "color:red; background-color:yellow"] [text c]
                    ]
                ]
                tbody [] [
                    forEach [|0..model.Rows.Count-1|] <| fun i ->
                        let row = model.Rows.[i]
                        model.Rows.[i] <- {row with Changed = false}
                        ecomp<Row,_,_> [] row (fun r-> 
                            dispatch (SetRow (r,i))
                        )
                ]
            ]            
        ]


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
            Title = "Table caption"
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
    //JS.log("update", message)
    match message with
    | Ping -> model
    | SetTitle(t)  -> {model with dg = model.dg.setTitle(t) }
    | EditTitle(b) -> {model with dg = model.dg.editTitle(b) }
    | SetRow(r,i)  -> model.dg.Rows.[i] <- r; {model with dg = model.dg }
    //| EditCell(r,i) -> r.setEditingIndex(i) |> ignore; model

let view (model:Model) dispatch =
    concat [
        h1 [] [text model.greeting]
        DataGrid.view model.dg dispatch
        let layout = 
            new Layout(
                Title= Title(Text="Scatter"),
                YAxis= ResizeArray<YAxis> ([
                    YAxis(Title= YAxisLib.Title(Text="Scatter Unit"))
                ])
            )
        in
        comp<PlotlyChart> [
            "layout" => layout 
            attr.style "height: 60vh; width=600px; min-height: 350px"
            
        ] [
        ]
    ]


type MyApp() =
    inherit ProgramComponent<Model, DataGrid.Message>()

    override this.Program =
        JS.Runtime <- this.JSRuntime
        Program.mkSimple (fun _ -> initModel) update view
