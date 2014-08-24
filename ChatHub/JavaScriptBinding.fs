namespace ChatHub

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.JQuery

[<JavaScript>]
module JavaScriptBinding =
    [<Inline("$dom.linkify()")>]
    let linkify (dom : JQuery) = X<JQuery>

    [<Inline("$ev.keyCode")>]
    let keyCode (ev : Event) = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault (ev : Event) = X

    [<Inline>]
    let onKeyDown f (elm : Element)  = 
        JQuery.Of(elm.Body).Keydown(fun _ arg -> f elm arg).Ignore

    [<Inline>]
    let onKeyPress f (elm : Element)  = 
        JQuery.Of(elm.Body).Keypress(fun _ arg -> f elm arg).Ignore

    [<Inline("$ev.shiftKey")>]
    let shiftKey (ev : Event) = X

    [<Inline("encodeURIComponent($url)")>]
    let encodeUrl (url : string) = X
