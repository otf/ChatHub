namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery

open Protocol
open ChatRendering

[<JavaScript>]
module ChatClient =
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
    

    let timeoutOfWriting = 3000
    let mutable writtenTimer = None : JavaScript.Handle option
    let writtenByOther () =
        if JQuery.Of("#written").Length = 0 then
            let msgElm = Div [ Div [Text "..."] -< [Attr.Class "padding10"] ] -< [Attr.Id "written"; Attr.Class "balloon left"]
            JQuery.Of("#history").Append(JQuery.Of(msgElm.Dom)) |> ignore
            JQuery.Of("#history-box").ScrollTop(JQuery.Of("#history").Height()) |> ignore
            writtenTimer |> Option.iter JavaScript.ClearTimeout
            writtenTimer <- Some <| JavaScript.SetTimeout (fun () -> JQuery.Of("#written").Remove() |> ignore) timeoutOfWriting

    let sayByOther (msg : string) =
        writtenTimer |> Option.iter JavaScript.ClearTimeout
        JQuery.Of("#written").Remove() |> ignore
        appendMessage Other msg
        messageAudio |> playAudio

    let mutable currentWebSocket = None : WebSocket option
    let mutable currentTimer = None : JavaScript.Handle option

    let send (msg:ClientProtocol) (ws : WebSocket) = ws.Send(msg |> Json.Stringify)

    let sendMessage () =
        let msg = (JQuery.Of("#message > textarea").Val() |> string).Trim()

        if msg = "" then 
            ()
        else
            currentWebSocket |> Option.iter (Speak msg |> send)
            appendMessage Me msg
            JQuery.Of("#message > textarea").Val("") |> ignore

    let ping () = currentWebSocket |> Option.iter (Ping |> send)

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
            | ServerProtocol.Written -> writtenByOther ()
            | ServerProtocol.Listen msg -> sayByOther msg
        ws.Onclose <- fun () ->
            currentTimer |> Option.iter JavaScript.ClearInterval
            JQuery.Of("#message > textarea").Attr("disabled", "disabled") |> ignore
            appendMessage System "チャットが切断されました。"
            JQuery.Of("#reconnect-button").RemoveAttr("disabled") |> ignore
            disconnectAudio |> playAudio
        currentTimer <- Some <| JavaScript.SetInterval ping (10 * 1000)
        ws

    let connect () =
        let ws = openChatWebSocket ()
        JQuery.Of("#reconnect-button").Attr("disabled", "disabled") |> ignore
        currentWebSocket <- Some ws


    [<Inline("$ev.keyCode")>]
    let keyCode ev = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault ev = X

    [<Inline("$ev.shiftKey")>]
    let shiftKey ev = X


    let mutable wasSentWrite = false
    let mutable clearWriteTimer = None : JavaScript.Handle option
    let onKeyDown ev =
        if not wasSentWrite then
            wasSentWrite <- true
            currentWebSocket |> Option.iter (Write |> send)
            clearWriteTimer |> Option.iter JavaScript.ClearTimeout
            clearWriteTimer <- Some <| JavaScript.SetTimeout (fun () -> wasSentWrite <- false) timeoutOfWriting

        true

    let onKeyPressMessage ev =
        if keyCode ev = 13 && not <| shiftKey ev then
            wasSentWrite <- false
            sendMessage ()
            preventDefault ev
        true

    let renderMain =
        connect ()
        JQuery.Of(fun _ -> JQuery.Of("#message > textarea").On("keypress", onKeyPressMessage)) |> ignore
        JQuery.Of(fun _ -> JQuery.Of("#message > textarea").On("keydown", onKeyDown)) |> ignore
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
