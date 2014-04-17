module Ethereum.Protocol

open System
open System.Net
open System.Numerics
open Ethereum.Common
open Ethereum.Encoding
open FSharpx
open FSharpx.Option

// Helpers

let private getAt i f x =
    maybe {
        return! Array.tryGet i x >>= f }

let private (|Head|_|) x =
    match x with
    | RLPArray x -> 
        match Array.tryGet 0 x with
        | Some (RLPInt x) -> Some x
        | _ -> None
    | _ -> None

// Block Protocol

type Block =
    { Header: BlockHeader
      Transactions: Transaction list
      Uncles: BlockHeader list }

and BlockHeader =
    { ParentHash: byte list
      UnclesHash: byte list 
      Coinbase: byte list
      StateRoot: byte list
      TransactionsHash: byte list
      Difficulty: int
      Timestamp: int
      ExtraData: byte list
      Nonce: byte list }

and Transaction =
    { Number: int }

// Wire Protocol

// Hello

type Hello =
    { Protocol: ProtocolVersion
      Client: string
      Capabilities: Capabilities
      Port: int
      Node: BigInteger option }

    static member encode (x: Hello) =
        let rlps =
            [| RLPInt 0
               RLPInt (int x.Protocol)
               RLPInt 0
               RLPString x.Client
               RLPInt (int x.Capabilities)
               RLPInt x.Port |]

        let rlps = Option.getOrElseWith rlps (fun (n: BigInteger) -> 
            rlps &<! RLPBytes (n.ToByteArray ())) x.Node

        RLPArray rlps

    static member decode (x: RLP) =
        maybe {
            let! x = (|RLPArray|_|) x
            let! protocol = enum<ProtocolVersion> <!> getAt 1 (|RLPInt|_|) x
            let! client = getAt 3 (|RLPString|_|) x
            let! capabilities = enum<Capabilities> <!> getAt 4 (|RLPInt|_|) x
            let! port = getAt 5 (|RLPInt|_|) x

            let node =
                Array.tryGet 6 x
                |> Option.bind (|RLPBytes|_|)
                |> Option.map (fun x -> BigInteger (x))

            return {
                Protocol = protocol
                Client = client
                Capabilities = capabilities
                Port = port
                Node = node } }

and ProtocolVersion =
    | PoC1 = 0
    | PoC2 = 1
    | PoC3 = 7
    | PoC4 = 9

and [<Flags>] Capabilities =
    | PeerDiscovery = 1
    | TransactionRelaying = 2
    | BlockChainQuerying = 4

// Disconnect

type Disconnect =
    { Reason: Reason option }

    static member encode (x: Disconnect) =
        let rlps = 
            [| RLPInt 1 |]

        let rlps = Option.getOrElseWith rlps (fun r -> 
            rlps &<! RLPInt (int r)) x.Reason

        RLPArray rlps

    static member decode (x: RLP) =
        maybe {
            let! x = (|RLPArray|_|) x

            let reason = 
                Array.tryGet 1 x 
                |> Option.bind (|RLPInt|_|) 
                |> Option.map (enum<Reason>)

            return {
                Reason = reason } }

and Reason =
    | Requested = 0
    | TCPError = 1
    | BadProtocol = 2
    | UselessPeer = 3
    | TooManyPeers = 4
    | AlreadyConnected = 5
    | WrongGenesisBlock = 6
    | IncompatibleNetworkProtocol = 7
    | ClientQuitting = 8
    
// Peers

type Peers =
    { Peers: Peer list }

    static member encode (x: Peers) =
        let rlps =
            [| RLPInt 11
               RLPArray (x.Peers |> List.map Peer.encode |> Array.ofList) |]

        RLPArray rlps

    static member decode (x: RLP) =
        maybe {
            let! x = (|RLPArray|_|) x
            let! peers = (fun x -> 
                x 
                |> Array.map Peer.decode 
                |> Array.choose id
                |> List.ofArray) <!> getAt 1 (|RLPArray|_|) x

            return {
                Peers = peers } }

and Peer =
    { IP: IPAddress
      Port: int
      Node: BigInteger }

    static member encode (x: Peer) =
        let rlps = 
            [| RLPBytes (x.IP.GetAddressBytes ())
               RLPInt x.Port
               RLPBytes (x.Node.ToByteArray ()) |]

        RLPArray rlps

    static member decode (x: RLP) =
        maybe {
            let! x = (|RLPArray|_|) x
            let! ip = (fun (x: byte []) -> IPAddress (x)) <!> getAt 0 (|RLPBytes|_|) x
            let! port = getAt 1 (|RLPInt|_|) x
            let! node = (fun (x: byte []) -> BigInteger (x)) <!> getAt 2 (|RLPBytes|_|) x

            return {
                IP = ip
                Port = port
                Node = node } }

// Wire

type Wire =
    | Hello of Hello
    | Disconnect of Disconnect
    | Ping
    | Pong
    | GetPeers
    | Peers of Peers

    static member encode (x: Wire) =
        match x with
        | Hello x -> Hello.encode x
        | Disconnect x -> Disconnect.encode x
        | Ping -> RLPArray [| RLPInt 2 |]
        | Pong -> RLPArray [| RLPInt 3 |]
        | GetPeers -> RLPArray [| RLPInt 10 |]
        | Peers x -> Peers.encode x

    static member decode (x: RLP) =
        match x with
        | Head t when t = 0 -> Hello.decode x |> Option.map Hello
        | Head t when t = 1 -> Disconnect.decode x |> Option.map Disconnect
        | Head t when t = 2 -> Some Ping
        | Head t when t = 3 -> Some Pong
        | Head t when t = 10 -> Some GetPeers
        | Head t when t = 11 -> Peers.decode x |> Option.map Peers
        | _ -> None
