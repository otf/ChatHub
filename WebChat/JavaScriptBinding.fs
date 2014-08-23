namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.JQuery

[<JavaScript>]
module JavaScriptBinding =
    [<Inline("$dom.linkify()")>]
    let linkify dom = X<JQuery>

    [<Inline("$ev.keyCode")>]
    let keyCode ev = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault ev = X

    let onKeyDown f (elm : Element)  = 
        JQuery.Of(elm.Body).Keydown(fun _ arg -> f elm arg).Ignore

    let onKeyPress f (elm : Element)  = 
        JQuery.Of(elm.Body).Keypress(fun _ arg -> f elm arg).Ignore

    [<Inline("$ev.shiftKey")>]
    let shiftKey ev = X

    [<Inline("encodeURIComponent($url)")>]
    let encodeUrl url = X