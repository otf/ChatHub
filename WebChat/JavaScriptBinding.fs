namespace WebChat

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html5

[<JavaScript>]
module JavaScriptBinding =
    [<Inline("$dom.linkify()")>]
    let linkify dom = X<Dom.Element>

    [<Inline("$ev.keyCode")>]
    let keyCode ev = X

    [<Inline("$ev.preventDefault()")>]
    let preventDefault ev = X

    [<Inline("$ev.shiftKey")>]
    let shiftKey ev = X
