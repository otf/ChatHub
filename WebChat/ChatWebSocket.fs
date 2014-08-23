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

    static let clients = Dictionary()

    let addAndTryBeginChat (me : WebSocketChatHandler) = 
        match clients |> Seq.tryFind (fun (KeyValue(_, other)) -> other = Waiting) with
        | Some (KeyValue (other, _)) -> 
            clients.[other] <- Chatting me
            clients.[me] <- Chatting other
            Join |> encodeServerProtocol |> other.Send
            Join |> encodeServerProtocol |> me.Send
        | None -> 
            clients.[me] <- Waiting
    
    let tryRemove (me : WebSocketChatHandler) =
        match clients.TryGetValue(me) with
        | (true, Chatting other) -> 
            other.Close()
            clients.Remove(other) |> ignore
        | _ -> ()

        clients.Remove(me) |> ignore

    let tryInputting other =
        Written |> encodeServerProtocol |> (other : WebSocketChatHandler).Send

    let trySpeak msg other = 
        Listen msg |> encodeServerProtocol |> (other : WebSocketChatHandler).Send

    let brancketOther this f =
        lock clients (fun () ->
            match clients.TryGetValue(this) with
            | (true, Chatting other) ->
                f other
            | _ ->
                failwith "チャット中じゃないのに発言しようとしました。")

    override this.OnOpen () = lock clients (fun () -> addAndTryBeginChat this)

    override this.OnClose() = lock clients (fun () -> tryRemove this)

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