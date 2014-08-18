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

    static let handlers = Dictionary()

    let addAndTryBeginChat (newHandler : WebSocketChatHandler) = 
        match handlers |> Seq.tryFind (fun kvp -> kvp.Value = Waiting) with
        | Some (KeyValue (other, _)) -> 
            handlers.[other] <- Chatting newHandler
            handlers.Add(newHandler, Chatting other)
            Join |> encodeServerProtocol |> other.Send
            Join |> encodeServerProtocol |> newHandler.Send
        | None -> handlers.[newHandler] <- Waiting
    
    let tryRemove (removeHandler : WebSocketChatHandler) =
        match handlers.TryGetValue(removeHandler) with
        | (true, Chatting other) -> 
            other.Close()
            handlers.Remove(other) |> ignore
        | _ -> ()

        handlers.Remove(removeHandler) |> ignore

    let trySpeak handler msg = 
        match handlers.TryGetValue(handler) with
        | (true, Chatting other) -> 
            Listen msg |> encodeServerProtocol |> other.Send
        | _ -> ()

    override this.OnOpen () = lock handlers (fun () -> addAndTryBeginChat this)

    override this.OnClose() = lock handlers (fun () -> tryRemove this)

    override this.OnMessage (message : string) = 
        match message |> decodeClientProtocol with
        | Ping -> ()
        | Speak msg -> lock handlers (fun () -> trySpeak this msg)

type ChatWebSocket() = 
    interface IHttpHandler with
        member this.ProcessRequest context =
            if context.IsWebSocketRequest then
                context.AcceptWebSocketRequest(new WebSocketChatHandler())
        member this.IsReusable = true