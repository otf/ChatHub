namespace WebChat

open IntelliFactory.WebSharper

open System.Web
open System.Net.WebSockets
open Microsoft.Web.WebSockets
open System.Collections.Concurrent

[<JavaScript>]
type ClientProtocol = 
    | Speak of string

[<JavaScript>]
type ServerProtocol = 
    | Join
    | Listen of string

module Protocol =
  let decodeClientProtocol (json : string) =
      let jP = Core.Json.Provider.Create()
      let dec = jP.GetDecoder<ClientProtocol>()
      dec.Decode (Core.Json.Parse json)

  let encodeServerProtocol (x : ServerProtocol) =
      let jP = Core.Json.Provider.Create()
      let enc = jP.GetEncoder<ServerProtocol>()
      enc.Encode x
      |> jP.Pack
      |> Core.Json.Stringify

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
                kvp.Key.Send(Protocol.encodeServerProtocol Join)
                this.Send(Protocol.encodeServerProtocol Join)
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
        | (true, Chatting target) -> target.Send(Protocol.encodeServerProtocol (Listen msg))
        | _ -> ()

    override this.OnMessage (message : string) = 
        match message |> Protocol.decodeClientProtocol with
        | Speak msg -> this.onSpeak(msg)

type ChatWebSocket() = 
    interface IHttpHandler with
        member this.ProcessRequest context =
            if context.IsWebSocketRequest then
                context.AcceptWebSocketRequest(new WebSocketChatHandler())
        member this.IsReusable = true