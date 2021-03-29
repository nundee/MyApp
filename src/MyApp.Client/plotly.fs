module Plotly 

open Elmish
open Bolero
open Bolero.Html

open MyInterop

type Layout = {
    Title:string   
}

type Trace =   {
    X : float[]
    Y: float[]
    Type:string 
    Name:string 
}

type PlotModel = {
    Layout:obj
    Data: obj[]
}

type Chart()=
    inherit ElmishComponent<PlotModel,string>()
    let divRef = HtmlRef()

    [<Microsoft.AspNetCore.Components.Parameter>]
    member val DivStyle = "" with get,set

    override this.View model dispatch =
        div [
            attr.ref divRef
            attr.style this.DivStyle
        ] [
        ]

    override this.OnAfterRenderAsync(firstRender:bool)=
        JS.execFunc(
            "MyJsLib.newPlot",
            divRef.Value, 
            this.Model.Data,
            this.Model.Layout
        )
//let inline Plotly (elt:Bolero.HtmlRef) =
//   JS.execFunc("MyJsLib.focus",elt.Value)
