namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery

open Protocol

[<JavaScript>]
module ChatClient =

    type Orientation = System | Me | Other

    let renderMessage (orientation : Orientation) (msg : string) =
        Div [ Div [Text msg] -< [Attr.Class "padding10"]]
        -<
        match orientation with 
        | System -> [Attr.Class "balloon"]
        | Me -> [Attr.Class "balloon right"]
        | Other -> [Attr.Class "balloon left"]

    let appendMessage (orientation : Orientation) (msg : string) =
        let msgElm = renderMessage orientation msg
        ById("history").AppendChild(msgElm.Dom) |> ignore

    let appendNotification (msg : string) =
        let msgElm = renderMessage System msg
        ById("history").AppendChild(msgElm.Dom) |> ignore

    let mutable currentWebSocket = Unchecked.defaultof<WebSocket>

    let sendMessage (msgBox : Element) =
        currentWebSocket.Send(Speak msgBox.Value |> Json.Stringify)
        appendMessage Me msgBox.Value
        msgBox.Value <- ""

    let openChatWebSocket () =
        let ws = WebSocket("ws://" + Window.Self.Location.Host + "/ChatWebSocket")
        ws.Onmessage <- fun ev -> 
            match Json.Parse(ev.Data.ToString()) |> As<ServerProtocol> with
            | ServerProtocol.Join -> 
                JQuery.Of("#waiting").Hide() |> ignore
                JQuery.Of("#history-box").Animate(New [("height", 500. :> obj)], 500) |> ignore
                JQuery.Of("#history-box").Show() |> ignore
                JQuery.Of("#message").Show() |> ignore
                appendNotification "connect"
                JQuery.Of("#message > textarea").RemoveAttr("disabled") |> ignore
            | ServerProtocol.Listen msg -> appendMessage Other msg
        ws.Onclose <- fun () ->
            JQuery.Of("#message > textarea").Attr("disabled", "disabled") |> ignore
            appendNotification "disconnect"
            JQuery.Of("#reconnect-button").RemoveAttr("disabled") |> ignore
        ws

    let connect () =
        let ws = openChatWebSocket ()
        JQuery.Of("#reconnect-button").Attr("disabled", "disabled") |> ignore
        currentWebSocket <- ws

    let renderMain =
        connect ()
        let msgBox = TextArea [Attr.Disabled "disabled"] 
        Div [Attr.Id "chat-box"] -< [
            Div [ Attr.Id "waiting" ]
            Div [ Attr.Id "history-box";Attr.Style "display:none"] -<
                [ Div [ Attr.Id "history";  Attr.Class "scrollbox"; Attr.NewAttr "data-role" "scrollbox"; Attr.NewAttr "data-scroll" "vertical" ]]
//            Button [ Text "reconnect"; Attr.Id "reconnect-button"; Attr.Disabled "disabled" ]
//            |>! OnClick (fun x ev -> connect (); appendNotification "waiting...")
            Div [Attr.Id "message"; Attr.Style "display:none"] -< [
                msgBox
                |>! OnKeyPress (fun input char -> if char.CharacterCode = 13 then sendMessage msgBox)
            ]
        ]

type ChatControl () =
    inherit Web.Control()

    [<JavaScript>]
    override __.Body =
        ChatClient.renderMain :> _