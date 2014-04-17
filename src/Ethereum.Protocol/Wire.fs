module Ethereum.Protocol.Wire

open System
open System.Net
open System.Numerics
open Ethereum.Common
open Ethereum.Encoding

// Hello

type Hello =
    { Protocol: ProtocolVersion
      Client: string
      Capabilities: Capabilities
      Port: int
      Node: BigInteger option }

    static member encode (x: Hello) =
        let items =
            [| RLPInt 0
               ProtocolVersion.encode x.Protocol
               RLPInt 0
               RLPString x.Client
               RLPInt (int x.Capabilities)
               RLPInt x.Port |]

        let items =
            match x.Node with
            | Some n -> items &<! Bytes (n.ToByteArray ())
            | _ -> items

        Array items

and ProtocolVersion =
    | PoC1
    | PoC2
    | PoC3
    | PoC4

    static member encode (x: ProtocolVersion) =
        match x with
        | PoC1 -> RLPInt 0
        | PoC2 -> RLPInt 1
        | PoC3 -> RLPInt 7
        | PoC4 -> RLPInt 9

and [<Flags>] Capabilities =
    | PeerDiscovery = 0x01
    | TransactionRelaying = 0x02
    | BlockChainQuerying = 0x04

// Disconnect

type Disconnect =
    { Reason: Reason option }

    static member encode (x: Disconnect) =
        let items = 
            [| RLPInt 1 |]

        let items =
            match x.Reason with
            | Some reason -> items &<! Reason.encode reason
            | _ -> items

        Array items

and Reason =
    | Requested
    | TCPError
    | BadProtocol
    | UselessPeer
    | TooManyPeers
    | AlreadyConnected
    | WrongGenesisBlock
    | IncompatibleNetworkProtocol
    | ClientQuitting

    static member encode (x: Reason) =
        match x with
        | Requested -> RLPInt 0
        | _ -> RLPInt 1 // TODO: Fill in the rest!
    
// Peers

type Peers =
    { Peers: Peer list }

    static member encode (x: Peers) =
        Array (RLPInt 11 &>! (x.Peers |> List.map Peer.encode |> Array.ofList))

and Peer =
    { IP: IPAddress
      Port: int
      Node: BigInteger }

    static member encode (x: Peer) =
        let items = 
            [| Bytes (x.IP.GetAddressBytes ())
               RLPInt x.Port
               Bytes (x.Node.ToByteArray ()) |]

        Array items

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
        | Ping -> Array [| RLPInt 2 |]
        | Pong -> Array [| RLPInt 3 |]
        | GetPeers -> Array [| RLPInt 10 |]
        | Peers x -> Peers.encode x

    static member decode (x: RLP) =
        match x with
        | Array [| RLPInt t;  _ |] when t = 0 -> None // should check to see how optimized this gets...
        | _ -> None
