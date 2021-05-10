module NotenTrainer.Lang

let noteNames_german = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "H"
|]
let noteNames_english = [|
    "C"; "D"; "E"; "F"; "G"; "A"; "B"
|]


let noteNames_italian = [|
    "Do"; "Re"; "Mi"; "Fa"; "Sol"; "La"; "Si"
|]

let getNoteNames (lang:string) =
    match lang.[0] with
    | 's'  -> noteNames_italian
    | 'e' -> noteNames_english
    | _ -> noteNames_german 

type EStatMsg=
| IterLeft=0
| ElapsedTime=1
| Correct=2
| LastGuessWrong=3
| LastGuessOK=4
| Trainer = 5

let StatMsgEnglish=[|
    "Remaining"
    "Elapsed time"
    "Correct"
    "Last guess was wrong"
    "Last guess was OK"
    "Note trainer"
|]

let StatMsgDeutsch=[|
    "Noch"
    "Zeit"
    "Richtig"
    "Letzter Tipp war falsch"
    "Letzter Tipp war richtig"
    "Notentrainer"
|]

let StatMsgShqip=[|
    "Akoma"
    "Kohëzgjatja"
    "Korrekt"
    "Gjuajtja e fundit ishte gabim"
    "Gjuajtja e fundit ishte në rregull"
    "Traineri i notave"
|]

let extraSharje=[|
    "ia mbyte kot"
    "nuk qenke në terezi"
    "le kokrrën e namit"
    "mblidh veten"
    "nuk paske pas pikën e turpit"
|]

let extraPergezime=[|
    "i modh je"
    "mjaft shite men, ec!"
    "hajde njoni"
    "ku ma paske fshehur tërë kete zotësi?"
    "të lumshin doçkat e vogla"
|]

let getStatMsgs (lang:string) = 
    match lang.[0] with
    | 's'  -> StatMsgShqip
    | 'e' -> StatMsgEnglish
    | _ -> StatMsgDeutsch 

let rnd=System.Random()
let getStatMessage lang (key:EStatMsg) =
    let msgs = getStatMsgs lang
    let msg = msgs.[int key]
    if lang.[0]<> 's' then msg else
    if key=EStatMsg.LastGuessOK then
        let i = rnd.Next(extraPergezime.Length)
        $"{msg} ({extraPergezime.[i]})"
    elif key=EStatMsg.LastGuessWrong then
        let i = rnd.Next(extraSharje.Length)
        $"{msg} ({extraSharje.[i]})"
    else msg
   
