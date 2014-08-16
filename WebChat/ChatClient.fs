namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.JQuery

[<JavaScript>]
module ChatClient =
    let renderMessage (msg : string) =
        Div [Text msg]

    let sendMessage (msgBox : Element) =
        let msgElm = renderMessage msgBox.Value
        ById("chat-box").AppendChild(msgElm.Dom) |> ignore
        msgBox.Value <- ""
       
    let renderMain =
        let msgBox = Input [ Text ""; Attr.Id "message-box"; Attr.Type "text" ] 
        Div [
            Div [ Attr.Id "chat-box" ]
            msgBox
            |>! OnKeyPress (fun input char -> if char.CharacterCode = 13 then sendMessage msgBox)
        ]

type ChatControl () =
    inherit Web.Control()

    [<JavaScript>]
    override __.Body =
        ChatClient.renderMain :> _