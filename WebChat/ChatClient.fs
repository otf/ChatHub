namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<JavaScript>]
module ChatClient =
    let render =
        Div [
            Div [ Attr.Id "chat-box" ]
            Input [ Text ""; Attr.Id "message-box"; Attr.Type "text" ] 
        ]

type ChatControl () =
    inherit Web.Control()

    [<JavaScript>]
    override __.Body =
        ChatClient.render :> _