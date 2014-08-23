namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.JQuery

[<JavaScript>]
module JavaScriptBinding =
    [<Inline("$dom.linkify()")>]
    let linkify dom = X<JQuery>

    [<Inline("$ev.keyCode")>]
    let keyCode ev = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault ev = X

    [<Inline("$ev.shiftKey")>]
    let shiftKey ev = X

    [<Inline("encodeURIComponent($url)")>]
    let encodeUrl url = X