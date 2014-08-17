namespace WebChat

open System.Web
open Microsoft.Web.WebSockets
open System.Collections.Concurrent

open Protocol

type Client<'a> = 
    | Waiting
    | Chatting of 'a

type WebSocketChatHandler () =
    inherit WebSocketHandler ()

    static let handlers = ConcurrentDictionary()

    override this.OnOpen () =
        match handlers |> Seq.tryFind (fun kvp -> kvp.Value = Waiting) with
        | Some kvp -> 
            if handlers.TryUpdate(kvp.Key, Chatting(this), Waiting) then
                handlers.TryAdd(this, Chatting(kvp.Key)) |> ignore
                kvp.Key.Send(encodeServerProtocol Join)
                this.Send(encodeServerProtocol Join)
            else
                handlers.TryAdd(this, Waiting) |> ignore
        | None -> handlers.TryAdd(this, Waiting) |> ignore

    override this.OnClose() =
        match handlers.TryGetValue(this) with
        | (true, Chatting target) -> target.Close()
        | _ -> ()

        handlers.TryRemove(this) |> ignore

    member private this.onSpeak (msg) =
        match handlers.TryGetValue(this) with
        | (true, Chatting target) -> target.Send(encodeServerProtocol (Listen msg))
        | _ -> ()

    override this.OnMessage (message : string) = 
        match message |> decodeClientProtocol with
        | Ping -> ()
        | Speak msg -> this.onSpeak(msg)

type ChatWebSocket() = 
    interface IHttpHandler with
        member this.ProcessRequest context =
            if context.IsWebSocketRequest then
                context.AcceptWebSocketRequest(new WebSocketChatHandler())
        member this.IsReusable = true