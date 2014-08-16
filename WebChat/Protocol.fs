namespace WebChat
module Protocol =
  open IntelliFactory.WebSharper

  [<JavaScript>]
  type ClientProtocol = 
      | Speak of string

  [<JavaScript>]
  type ServerProtocol = 
      | Join
      | Listen of string

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