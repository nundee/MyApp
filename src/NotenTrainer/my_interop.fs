module MyInterop

open Microsoft.JSInterop
open System.Text.Json
type JS() =    
    static member val Runtime = Unchecked.defaultof<IJSRuntime>  with get, set
    static member log([<System.ParamArray>] args:obj[]) =
        JS.Runtime.InvokeVoidAsync("console.log",args) |> ignore
    static member execFunc(func:string, [<System.ParamArray>] args:obj[])=
        JS.Runtime.InvokeVoidAsync(func, args).AsTask()
        // |> Run
    static member evalFunc(func:string, [<System.ParamArray>] args:obj[])=
        let x = JS.Runtime.InvokeAsync(func, args).AsTask()
        x


let saveSettings<'T> s =
    JS.execFunc("MyJsLib.saveSettings", JsonSerializer.Serialize<'T>(s)) 

let loadSettings<'T> fallback =
    async {
        let! ss = JS.evalFunc<string>("MyJsLib.loadSettings") |> Async.AwaitTask
        if ss.Length<1 then return fallback else
        return JsonSerializer.Deserialize<'T>(ss)
    }
