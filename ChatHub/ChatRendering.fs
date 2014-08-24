namespace ChatHub

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module ChatRendering =
    type Orientation = System | Me | Other

    let renderQuoteIt (body : string) =
        let result = Div [ Attr.Class "padding10"]
        result.Html <- body
        result

    let renderMessage (orientation : Orientation) (msg : string) =
        let msgElm = Div [Text msg] -< [Attr.Class "padding10"]
        msgElm.Html <- msgElm.Html.Replace("\n", "</br>")
        Div [ msgElm ]
        -<
        match orientation with 
        | System -> [Attr.Class "balloon"]
        | Me -> [Attr.Class "balloon right"]
        | Other -> [Attr.Class "balloon left"]

    let renderWritten () =
        Div [ Div [Text "..."] -< [Attr.Class "padding10"] ] -< [Id "written"; Attr.Class "balloon left"]        

    let renderAudio (path : string) =
        let se = HTML5.Tags.Audio []
        let audio = As<HTMLAudioElement> se.Dom
        audio.Src <- path
        audio, se
