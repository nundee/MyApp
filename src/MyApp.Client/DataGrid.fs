
module DataGrid 

open Elmish
open Bolero
open Bolero.Html

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
    let mutable value = ""
    let mutable changed=false

    //override this.ShouldRender(oldModel, newModel) =
    //    oldModel <> newModel

    override this.View model dispatch =
        input [
            attr.style "width:80px;"
            attr.value model
            attr.ref inputRef
            bind.change.string model (fun t-> changed <- true; value <- t; MyInterop.unsetFocus(inputRef) |> ignore)//dispatch t)
            on.blur (fun _ -> 
                //printfn "on.blur"
                dispatch (if changed then value else model)
            ) 
        ]
    override this.OnAfterRenderAsync(firstRender:bool)=
        base.OnAfterRenderAsync(firstRender).ContinueWith(
            fun _ -> MyInterop.setFocus inputRef |> ignore
        )

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
                    if i=model.CellEditingIndex then attr.contenteditable true else                       
                    on.dblclick (fun _-> 
                        if i <> model.CellEditingIndex then 
                            //JS.log("dblclick",i)
                            printfn "dblclick %d" i
                            let m=model.setEditingIndex(i)
                            dispatch m
                    )
                ] [
                    text model.Cells.[i]
                    // cond (i=model.CellEditingIndex) <| function
                    // | true ->  ecomp<MyInput,_,_> [] 
                    //             model.Cells.[i] (fun s -> 
                    //                 let m=model.setCell(i,s)
                    //                 dispatch m
                    //             )
                    // | false -> text model.Cells.[i]
                ]
        ]

let update message (model:GridModel) =
    //JS.log("update", message)
    match message with
    | Ping -> model
    | SetTitle(t)  -> model.setTitle(t)
    | EditTitle(b) -> model.editTitle(b)
    | SetRow(r,i)  -> model.Rows.[i] <- r; {model with Rows=model.Rows} //{model with dg = model.dg }

let view model dispatch =        
    div [attr.style "width:80%; height:500px; overflow-y: auto; overflow-x: auto;"] [
       
        table [attr.style "width:100%"
               "cellpadding" => 0
               "cellspacing" => 0
        ] [
            caption [
                on.dblclick (fun _ -> if (not model.TitleEditing) then dispatch (EditTitle true))
            ] [
                cond model.TitleEditing <| function
                | true -> ecomp<MyInput,_,_> [] model.Title (dispatch << SetTitle)
                | false -> text model.Title
            ]
            colgroup [] [
                forEach model.Columns.Cells <| fun _ -> col [attr.style "width:80px;"]
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
