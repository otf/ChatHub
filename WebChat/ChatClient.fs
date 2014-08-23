namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery
open IntelliFactory.WebSharper.EcmaScript

open Protocol
open ChatRendering
open JavaScriptBinding

[<JavaScript>]
module ChatClient =
    let getQuoteIt link = JQuery.Get("https://quoteit.herokuapp.com/clip.json?u=" + encodeUrl link)

    let appendQuoteIt (msgElm : JQuery) (body : string) = 
        let result = ((body ? results): string []).[0]
        let quoteIt = renderQuoteIt <| Json.Parse(RegExp("<p>(.*)</p>").Exec(result).[1]) ? html 
        msgElm.Append(quoteIt.Dom)

    let appendToHistory (elm:Element) =
        JQuery.Of("#history").Append(JQuery.Of(elm.Dom)) |> ignore
        JQuery.Of("#history-box").ScrollTop(JQuery.Of("#history").Height()) |> ignore

    let appendMessage (orientation : Orientation) (msg : string) =
        let msgElm = renderMessage orientation msg
        let msgElmJQuery = JQuery.Of(msgElm.Dom) |> linkify
        JQuery.Of("a", msgElmJQuery).Each(fun link -> (JQuery.Of(link : Dom.Element).Attr("href") |> getQuoteIt).Done(appendQuoteIt msgElmJQuery) |> ignore) 
        |> ignore
        msgElm |> appendToHistory
    
    let writingTimeout = 3000
    let mutable writtenTimer = None : JavaScript.Handle option

    let writtenByOther () =
        if JQuery.Of("#written").Length = 0 then
            renderWritten () |> appendToHistory
            writtenTimer |> Option.iter JavaScript.ClearTimeout
            writtenTimer <- Some <| JavaScript.SetTimeout (fun () -> JQuery.Of("#written").Remove() |> ignore) writingTimeout

    let sayByOther (msg : string) =
        writtenTimer |> Option.iter JavaScript.ClearTimeout
        JQuery.Of("#written").Remove() |> ignore
        appendMessage Other msg
        ChatAudio.message |> ChatAudio.play

    let mutable webSocket = None : WebSocket option

    // ホスティングサーバーなどは通信のないまま接続し続けると勝手に切られることがあるので定期的にPingを送るようにする
    let mutable pingTimer = None : JavaScript.Handle option

    let send (msg:ClientProtocol) (ws : WebSocket) = ws.Send(msg |> Json.Stringify)

    let sendMessage () =
        let msg = (JQuery.Of("#message > textarea").Val() |> string).Trim()

        if msg = "" then 
            ()
        else
            webSocket |> Option.iter (Speak msg |> send)
            appendMessage Me msg
            JQuery.Of("#message > textarea").Val("") |> ignore

    let ping () = webSocket |> Option.iter (Ping |> send)

    let join () =
        JQuery.Of("#waiting").Hide() |> ignore
        JQuery.Of("#history-box").Animate(New [("height", 500. :> obj)], 500) |> ignore
        JQuery.Of("#history-box").Show() |> ignore
        JQuery.Of("#message").Show() |> ignore
        appendMessage System "相手が見つかりました。チャットを開始します。"
        JQuery.Of("#message > textarea").RemoveAttr("disabled") |> ignore
        ChatAudio.join |> ChatAudio.play

    let openWebSocket () =
        let ws = WebSocket("ws://" + Window.Self.Location.Host + "/ChatWebSocket")
        ws.Onmessage <- fun ev -> 
            match Json.Parse(ev.Data.ToString()) |> As<ServerProtocol> with
            | ServerProtocol.Join -> join ()
            | ServerProtocol.Written -> writtenByOther ()
            | ServerProtocol.Listen msg -> sayByOther msg
        ws.Onclose <- fun () ->
            pingTimer |> Option.iter JavaScript.ClearInterval
            JQuery.Of("#message > textarea").Attr("disabled", "disabled") |> ignore
            appendMessage System "チャットが切断されました。"
            JQuery.Of("#reconnect-button").RemoveAttr("disabled") |> ignore
            ChatAudio.disconnect |> ChatAudio.play
        pingTimer <- Some <| JavaScript.SetInterval ping (10 * 1000)
        ws

    let connect () =
        let ws = openWebSocket ()
        JQuery.Of("#reconnect-button").Attr("disabled", "disabled") |> ignore
        webSocket <- Some ws

    let mutable wasSentWrite = false
    let mutable clearWriteTimer = None : JavaScript.Handle option
    let onKeyDown ev =
        if not wasSentWrite then
            wasSentWrite <- true
            webSocket |> Option.iter (Write |> send)
            clearWriteTimer |> Option.iter JavaScript.ClearTimeout
            clearWriteTimer <- Some <| JavaScript.SetTimeout (fun () -> wasSentWrite <- false) writingTimeout

        true

    let onKeyPressMessage ev =
        // 改行(13)とシフトキー同時押しで明示的に改行することができる
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
            ChatAudio.elementOf ChatAudio.join
            ChatAudio.elementOf ChatAudio.message
            ChatAudio.elementOf ChatAudio.disconnect
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
