module MyInterop

open Microsoft.JSInterop

type JS() =    
    static member val Runtime = Unchecked.defaultof<IJSRuntime>  with get, set
    static member log([<System.ParamArray>] args:obj[]) =
        JS.Runtime.InvokeVoidAsync("console.log",args) |> ignore
    static member execFunc(func:string, [<System.ParamArray>] args:obj[])=
        JS.Runtime.InvokeVoidAsync(func, args).AsTask()
        // |> Run
    static member evalFunc(func:string, [<System.ParamArray>] args:obj[])=
        let x = JS.Runtime.InvokeAsync(func, args).AsTask() |> Async.AwaitTask
        x


let inline setFocus (elt:Bolero.HtmlRef) =
    JS.execFunc("MyJsLib.focus",elt.Value)
let inline unsetFocus (elt:Bolero.HtmlRef) =
        JS.execFunc("MyJsLib.unfocus",elt.Value)