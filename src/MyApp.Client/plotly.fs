module Plotly 

open Elmish
open Bolero
open Bolero.Html

open MyInterop

type Chart()=
    inherit ElmishComponent<string,string>()
    let divRef = HtmlRef()

    override this.View model dispatch =
        div [
            attr.ref divRef
        ] [
        ]

    override this.OnAfterRenderAsync(firstRender:bool)=
        JS.execFunc("MyJsLib.newPlot",divRef.Value, Array.empty<obj>, null)
//let inline Plotly (elt:Bolero.HtmlRef) =
//   JS.execFunc("MyJsLib.focus",elt.Value)
