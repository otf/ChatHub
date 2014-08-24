namespace ChatHub

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets

type Action =
    | Home
    | Chat

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Content.HtmlElement>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

module Site =

    let HomePage =
        Skin.WithTemplate "HomePage" <| fun ctx ->
            [
                H1 [Text "ChatHub"]
                A [HRef (ctx.Link Chat)] -< [Text "Start ChatHub"]
            ]

    let ChatPage =
        Skin.WithTemplate "ChatPage" <| fun ctx ->
            [
                Div [Attributes.Class "container"] -<
                    [H1 [Text "ChatHub"]] -<
                    [new ChatControl()]
            ]

    let Main =
        Sitelet.Sum [
            Sitelet.Content "/" Home HomePage
            Sitelet.Content "/Chat" Chat ChatPage
        ]

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home; Chat]

type Global() =
    inherit System.Web.HttpApplication()

    member g.Application_Start(sender: obj, args: System.EventArgs) =
        ()

[<assembly: Website(typeof<Website>)>]
do ()
