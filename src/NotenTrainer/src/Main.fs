module Main

open System
open Elmish
open Fable.React
open Fable.React.Props
open Elmish.React
open Fable.Core.DynamicExtensions

open NotenTrainer.Model
open NotenTrainer.Lang

let loadSettings() = 
    let jobj = MyInterop.loadSettings()
    if isNull jobj then settings <- defaultSettings else
    settings <- {
        NrNotes = System.Convert.ToInt32(jobj.["NrNotes"])
        MaxIter = System.Convert.ToInt32(jobj.["MaxIter"])
        Language =jobj.["Language"].ToString()
    }
    printfn "settings= %A" settings


let view (model:Model) dispatch =
        let flags = [
            "albania.png","shqip"
            "united-kingdom.png","english"
            //"italy.png","italiano"
            "germany.png","deutsch"
        ]
        div[] [
            div [Style [
                        CSSProp.Display DisplayOptions.Flex
                        CSSProp.JustifyContent "flex-end"
                        CSSProp.MarginRight "20px"
                ]
            ] [
                button [
                    Class "btn"
                    Title "New game"
                    Style [CSSProp.MarginRight "20px"]
                    OnClick (fun _ -> dispatch NewGame)
                ] [img [Src "reload_icon.png"; HTMLAttr.Width "32px"; HTMLAttr.Height "32px"]]
                for (f,t) in flags do
                    button [
                        Class "btn"
                        Title t
                        OnClick (fun _ -> ChangeLanguage(t) |> dispatch)
                    ] [img [Src f; HTMLAttr.Width "32px"; HTMLAttr.Height "32px"]]
            ]
            h1 [] [str <| getStatMessage model.Language EStatMsg.Trainer]
            Partitur.makeView model dispatch
            Keyboard.makeView model dispatch
            Stats.makeView model dispatch
            if gameOver model then 
                div [ Style [
                        CSSProp.Position PositionOptions.Absolute
                        CSSProp.Left "0px"
                        CSSProp.Top "50px"
                        CSSProp.Width "100%"
                        CSSProp.Height "100%"
                        CSSProp.Background "rgba(0,0,0,0.5)"
                        CSSProp.ZIndex 10
                        CSSProp.Display DisplayOptions.Flex
                        CSSProp.JustifyContent "center"
                        CSSProp.AlignItems AlignItemsOptions.Center
                       ]
                ] [
                    h1 [Style [
                        CSSProp.Color "orange"
                        CSSProp.Background "rgb(0,0,0,0.5)"]] 
                        [str "GAME OVER"]
                ]
            else div[][]
        ]

let makeView model dispatch =
    lazyView2 view model dispatch