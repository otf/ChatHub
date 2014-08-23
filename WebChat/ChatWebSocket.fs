namespace WebChat

open System.Web
open Microsoft.Web.WebSockets
open System.Collections.Generic

open Protocol

type Client<'a> = 
    | Waiting
    | Chatting of 'a

type WebSocketChatHandler () =
    inherit WebSocketHandler ()

    static let handlerClientMap = Dictionary()
    static let waitingList = ResizeArray()

    let addAndTryBeginChat (me : WebSocketChatHandler) = 
        match waitingList |> Seq.tryFind(fun _ -> true) with
        | Some other -> 
            handlerClientMap.[other] <- Chatting me
            handlerClientMap.[me] <- Chatting other
            Join |> encodeServerProtocol |> other.Send
            Join |> encodeServerProtocol |> me.Send
            waitingList.Remove(other) |> ignore
        | _ -> 
            handlerClientMap.[me] <- Waiting
            waitingList.Add(me)
    
    let tryRemove (me : WebSocketChatHandler) =
        waitingList.Remove(me) |> ignore
        match handlerClientMap.TryGetValue(me) with
        | (true, Chatting other) -> 
            other.Close()
            handlerClientMap.Remove(other) |> ignore
        | _ -> ()

        handlerClientMap.Remove(me) |> ignore

    let tryInputting other =
        Written |> encodeServerProtocol |> (other : WebSocketChatHandler).Send

    let trySpeak msg other = 
        Listen msg |> encodeServerProtocol |> (other : WebSocketChatHandler).Send

    let brancketMe this f =
        lock handlerClientMap (fun () -> f this)

    let brancketOther this f =
        lock handlerClientMap (fun () ->
            match handlerClientMap.TryGetValue(this) with
            | (true, Chatting other) ->
                f other
            | _ ->
                failwith "チャット中じゃないのに発言しようとしました。")

    override this.OnOpen () = brancketMe this addAndTryBeginChat

    override this.OnClose() = brancketMe this tryRemove

    override this.OnMessage (message : string) = 
        match message |> decodeClientProtocol with
        | Ping -> ()
        | Write -> 
            brancketOther this <| tryInputting
        | Speak msg -> 
            brancketOther this <| trySpeak msg
type ChatWebSocket() = 
    interface IHttpHandler with
        member this.ProcessRequest context =
            if context.IsWebSocketRequest then
                context.AcceptWebSocketRequest(new WebSocketChatHandler())
        member this.IsReusable = true