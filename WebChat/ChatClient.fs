namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery

[<JavaScript>]
module ChatClient =
    let renderMessage (msg : string) =
        Div [Text msg]

    let appendMessage (msg : string) =
        let msgElm = renderMessage msg
        ById("chat-box").AppendChild(msgElm.Dom) |> ignore

    let appendNotification (msg : string) =
        let msgElm = renderMessage msg
        ById("chat-box").AppendChild(msgElm.Dom) |> ignore

    let sendMessage (ws:WebSocket) (msgBox : Element) =
        ws.Send(Speak msgBox.Value |> Json.Stringify)
        appendMessage msgBox.Value
        msgBox.Value <- ""

    let openChatWebSocket () =
        let ws = WebSocket("ws://" + Window.Self.Location.Host + "/ChatWebSocket")
        ws.Onmessage <- fun ev -> 
            match Json.Parse(ev.Data.ToString()) |> As<ServerProtocol> with
            | ServerProtocol.Join -> 
                appendNotification "connect"
                JQuery.Of("#message-box").RemoveAttr("disabled") |> ignore
            | ServerProtocol.Listen msg -> appendMessage msg
        ws.Onclose <- fun () ->
            JQuery.Of("#message-box").Attr("disabled", "disabled") |> ignore
            appendNotification "disconnect"
        ws
       
    let renderMain =
        let ws = openChatWebSocket ()
        let msgBox = Input [ Text ""; Attr.Id "message-box"; Attr.Disabled "disabled"; Attr.Type "text" ] 
        Div [
            Div [ Attr.Id "chat-box" ]
            msgBox
            |>! OnKeyPress (fun input char -> if char.CharacterCode = 13 then sendMessage ws msgBox)
        ]

type ChatControl () =
    inherit Web.Control()

    [<JavaScript>]
    override __.Body =
        ChatClient.renderMain :> _