namespace ChatHub

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html5

open ChatRendering

[<JavaScript>]
module ChatAudio =
    let join = renderAudio "audio/join.mp3"
    let message = renderAudio "audio/message.mp3"
    let disconnect = renderAudio "audio/disconnect.mp3"
    let play (audio : HTMLAudioElement, _) = audio.Play()
    let elementOf (_, elm) = elm
