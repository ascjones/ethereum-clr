module Ethereum.Protocol.Wire

open System
open System.Net
open System.Numerics
open Ethereum.Common
open Ethereum.Encoding
open FSharpx
open FSharpx.Option

// Hello

type Hello =
    { Protocol: ProtocolVersion
      Client: string
      Capabilities: Capabilities
      Port: int
      Node: BigInteger option }

    static member encode (x: Hello) =
        let rlps =
            [| RLPInt 0x00
               RLPInt (int x.Protocol)
               RLPInt 0x00
               RLPString x.Client
               RLPInt (int x.Capabilities)
               RLPInt x.Port |]

        let rlps = Option.getOrElseWith rlps (fun (n: BigInteger) -> 
            rlps &<! Bytes (n.ToByteArray ())) x.Node

        Array rlps

and ProtocolVersion =
    | PoC1 = 0x00
    | PoC2 = 0x01
    | PoC3 = 0x07
    | PoC4 = 0x09

and [<Flags>] Capabilities =
    | PeerDiscovery = 0x01
    | TransactionRelaying = 0x02
    | BlockChainQuerying = 0x04

// Disconnect

type Disconnect =
    { Reason: Reason option }

    static member encode (x: Disconnect) =
        let rlps = 
            [| RLPInt 0x01 |]

        let rlps = Option.getOrElseWith rlps (fun r -> 
            rlps &<! RLPInt (int r)) x.Reason

        Array rlps

and Reason =
    | Requested = 0x00
    | TCPError = 0x01
    | BadProtocol = 0x02
    | UselessPeer = 0x03
    | TooManyPeers = 0x04
    | AlreadyConnected = 0x05
    | WrongGenesisBlock = 0x06
    | IncompatibleNetworkProtocol = 0x07
    | ClientQuitting = 0x08
    
// Peers

type Peers =
    { Peers: Peer list }

    static member encode (x: Peers) =
        Array (RLPInt 0x011 &>! (x.Peers |> List.map Peer.encode |> Array.ofList))

and Peer =
    { IP: IPAddress
      Port: int
      Node: BigInteger }

    static member encode (x: Peer) =
        let rlps = 
            [| Bytes (x.IP.GetAddressBytes ())
               RLPInt x.Port
               Bytes (x.Node.ToByteArray ()) |]

        Array rlps

// Protocol

type Protocol =
    | Hello of Hello
    | Disconnect of Disconnect
    | Ping
    | Pong
    | GetPeers
    | Peers of Peers

    static member encode (x: Protocol) =
        match x with
        | Hello x -> Hello.encode x
        | Disconnect x -> Disconnect.encode x
        | Ping -> Array [| RLPInt 0x02 |]
        | Pong -> Array [| RLPInt 0x03 |]
        | GetPeers -> Array [| RLPInt 0x10 |]
        | Peers x -> Peers.encode x

    static member decode (x: RLP) =
        match x with
        | Array [| RLPInt t;  _ |] when t = 0x00 -> None // should check to see how optimized this gets...
        | _ -> None
