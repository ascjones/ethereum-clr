module Ethereum.Protocol.Tests

open System.Net
open System.Numerics
open Ethereum.Protocol
open FsUnit
open Xunit

// Wire Protocol Tests

[<Fact>]
let ``Hello round trips successfully`` () =
    let hello =
        { Protocol = ProtocolVersion.PoC4
          Client = "EthereumCLR"
          Capabilities = Capabilities.PeerDiscovery ||| Capabilities.TransactionRelaying
          Port = 30303
          Node = None } |> Hello

    hello 
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some hello)

[<Fact>]
let ``Disconnect round trips successfully`` () =
    let disconnect =
        { Reason = Some Reason.Requested } |> Disconnect

    disconnect
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some disconnect)

let ``Ping round trips successfully`` () =
    let ping = Ping

    ping
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some ping)

let ``Pong round trips successfully`` () =
    let pong = Pong

    pong
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some pong)

let ``GetPeer round trips successfully`` () =
    let getPeers = GetPeers

    getPeers
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some getPeers)

[<Fact>]
let ``Peers round trips successfully`` () =
    let peers =
        { Peers = 
              [ { IP = IPAddress.Loopback
                  Port = 30303
                  Node = BigInteger (256L) } ] } |> Peers

    peers
    |> Wire.encode
    |> Wire.decode
    |> should equal (Some peers)
