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
        let msgElm = Div [Text msg] -< [Attr.Class "padding10"]
        msgElm.Html <- msgElm.Html.Replace("\n", "</br>")
        Div [ msgElm ]
        -<
        match orientation with 
        | System -> [Attr.Class "balloon"]
        | Me -> [Attr.Class "balloon right"]
        | Other -> [Attr.Class "balloon left"]

    let renderAudio (path : string) =
        let se = HTML5.Tags.Audio []
        let audio = As<HTMLAudioElement> se.Dom
        audio.Src <- path
        audio, se

    let joinAudio = renderAudio "audio/join.mp3"
    let messageAudio = renderAudio "audio/message.mp3"
    let disconnectAudio = renderAudio "audio/disconnect.mp3"
    let playAudio (audio : HTMLAudioElement, _) = audio.Play()
    let elementOf (_, elm) = elm

    [<Inline("$dom.linkify()")>]
    let linkify dom = X<Dom.Element>

    let appendMessage (orientation : Orientation) (msg : string) =
        let msgElm = renderMessage orientation msg
        JQuery.Of("#history").Append(JQuery.Of(msgElm.Dom) |> linkify) |> ignore
        JQuery.Of("#history-box").ScrollTop(JQuery.Of("#history").Height()) |> ignore
    
    let sayByOther (msg : string) =
        appendMessage Other msg
        messageAudio |> playAudio

    let mutable currentWebSocket = Unchecked.defaultof<WebSocket>
    let mutable currentTimer = Unchecked.defaultof<JavaScript.Handle>

    let sendMessage () =
        let msg = (JQuery.Of("#message > textarea").Val() |> string).Trim()

        if msg = "" then 
            ()
        else
            currentWebSocket.Send(Speak msg |> Json.Stringify)
            appendMessage Me msg
            JQuery.Of("#message > textarea").Val("") |> ignore

    let ping () = currentWebSocket.Send(Ping |> Json.Stringify)


    let openChatWebSocket () =
        let ws = WebSocket("ws://" + Window.Self.Location.Host + "/ChatWebSocket")
        ws.Onmessage <- fun ev -> 
            match Json.Parse(ev.Data.ToString()) |> As<ServerProtocol> with
            | ServerProtocol.Join -> 
                JQuery.Of("#waiting").Hide() |> ignore
                JQuery.Of("#history-box").Animate(New [("height", 500. :> obj)], 500) |> ignore
                JQuery.Of("#history-box").Show() |> ignore
                JQuery.Of("#message").Show() |> ignore
                appendMessage System "相手が見つかりました。チャットを開始します。"
                JQuery.Of("#message > textarea").RemoveAttr("disabled") |> ignore
                joinAudio |> playAudio
            | ServerProtocol.Listen msg -> sayByOther msg
        ws.Onclose <- fun () ->
            JavaScript.ClearInterval currentTimer
            JQuery.Of("#message > textarea").Attr("disabled", "disabled") |> ignore
            appendMessage System "チャットが切断されました。"
            JQuery.Of("#reconnect-button").RemoveAttr("disabled") |> ignore
            disconnectAudio |> playAudio
        currentTimer <- JavaScript.SetInterval ping (10 * 1000)
        ws

    let connect () =
        let ws = openChatWebSocket ()
        JQuery.Of("#reconnect-button").Attr("disabled", "disabled") |> ignore
        currentWebSocket <- ws


    [<Inline("$ev.keyCode")>]
    let keyCode ev = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault ev = X

    [<Inline("$ev.shiftKey")>]
    let shiftKey ev = X

    let onKeyPressMessage ev =
        if keyCode ev = 13 && not <| shiftKey ev then
            sendMessage ()
            preventDefault ev
        true

    let renderMain =
        connect ()
        JQuery.Of(fun _ -> JQuery.Of("#message > textarea").On("keypress", onKeyPressMessage)) |> ignore
        Div [Attr.Id "chat-box"] -< [
            joinAudio |> elementOf
            messageAudio |> elementOf
            disconnectAudio |> elementOf
            Div [ Attr.Id "waiting" ] -<
              [P [Text "相手を探しています"]]
            Div [ Attr.Id "history-box";Attr.Style "display:none"] -<
                [ Div [ Attr.Id "history"]]
//            Button [ Text "reconnect"; Attr.Id "reconnect-button"; Attr.Disabled "disabled" ]
//            |>! OnClick (fun x ev -> connect (); appendNotification "waiting...")
            Div [Attr.Id "message"; Attr.Style "display:none"] -< [
                TextArea [Attr.Disabled "disabled"] 
            ]
        ]

type ChatControl () =
    inherit Web.Control()

    [<JavaScript>]
    override __.Body =
        ChatClient.renderMain :> _
