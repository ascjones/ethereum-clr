module Ethereum

open System
open System.Net
open System.Net.Sockets
open System.IO
open System.Threading
open System.Threading.Tasks
open Ethereum.Common
open Ethereum.Encoding
open Ethereum.Protocol

type AsyncBuilder with
    member __.Bind(f : Task<'T>, g : 'T -> Async<'S>) = __.Bind(Async.AwaitTask f, g)
    member __.Bind(f : Task, g : unit -> Async<'S>) = __.Bind(f.ContinueWith ignore, g)

type private PeerResponse =
    | Acknowledge
    | Reply of Wire
    | Fault of exn

// The magic sync token which should be the first 4 bytes of every message.
// https://github.com/ethereum/wiki/wiki/%5BEnglish%5D-Wire-Protocol
let magicTokenBytes = BigEndian.toBytes(0x22400891)

type Peer(tcpCli : TcpClient) =
    let stream = tcpCli.GetStream()

    let rec peerLoop () = async {
        
        printfn "Peer waiting for messages"
        let! firstFourBytes = stream.AsyncRead 4
        printfn "Received bytes %A" firstFourBytes

//        if firstFourBytes = magicTokenBytes then

        let! lengthBytes = stream.AsyncRead 4
        let length = lengthBytes |> BigEndian.ofBytes
        let! msgBytes = stream.AsyncRead length
        let msg = msgBytes |> decode |> Wire.decode

        match msg with
        | Some m ->
            match m with
            | Hello h ->
                printfn "Received Hello Message"
            | x -> printfn "Not handling messages of %A" x
        | None -> printfn "Could not decode incoming message %A" msgBytes

        return! peerLoop ()
    } 

    member __.Start (ct : CancellationToken) = 
        Async.Start(peerLoop (), ct)
    

type TcpServer(ipEndpoint : IPEndPoint) = 

    member __.Start () = 
        let cancel = new CancellationTokenSource()
        let listener = new TcpListener(ipEndpoint)
        listener.Start()

        let rec serverLoop peers = async {
            try
                printfn "Waiting for connections on Port %i" ipEndpoint.Port
                let! (client : TcpClient) = listener.AcceptTcpClientAsync()
                printfn "Accepted Peer Connection"
                let peer = Peer(client)
                peer.Start(cancel.Token)
                return! serverLoop (peer::peers)
            with e -> printfn "Error starting TcpServer: %A" e
        }

        Async.Start(serverLoop [], cancel.Token)


