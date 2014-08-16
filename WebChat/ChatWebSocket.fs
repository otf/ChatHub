namespace WebChat

open IntelliFactory.WebSharper

open System.Web
open System.Net.WebSockets
open Microsoft.Web.WebSockets

type WebSocketChatHandler () =
    inherit WebSocketHandler ()

    static let handlers = ResizeArray()

    override this.OnOpen () =
        handlers.Add(this)

    override this.OnClose() =
        handlers.Remove(this) |> ignore

    override this.OnMessage (message : string) =
        handlers |> Seq.filter ((<>) this) |> Seq.iter (fun h -> h.Send(message))

type ChatWebSocket() = 
    interface IHttpHandler with
        member this.ProcessRequest context =
            if context.IsWebSocketRequest then
                context.AcceptWebSocketRequest(new WebSocketChatHandler())
        member this.IsReusable = true