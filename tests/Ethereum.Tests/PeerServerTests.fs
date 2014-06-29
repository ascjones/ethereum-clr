namespace Ethereum.Tests

module TcpServerTests =

    open System.Net
    open System.Net.Sockets
    open System.IO
    open FsUnit
    open Xunit
    open Ethereum.Protocol
    open Ethereum.Encoding
    open Ethereum.Common
    open Ethereum

    let startPeerServer serverEndpoint =
        let peerServer = new PeerServer(serverEndpoint)
        printfn "Starting TcpServer"
        peerServer.Start()

    [<Fact>]
    let ``Peer responds to Hello packet with Hello`` () =
        let hello = 
            { Protocol = ProtocolVersion.PoC1
              Client = "Test"
              Capabilities = Capabilities.PeerDiscovery
              Port = 9999
              Node = None }

        let serverEndpoint = new IPEndPoint(IPAddress.Loopback, 30303)
        startPeerServer serverEndpoint

        use tcpClient = new TcpClient(new IPEndPoint(IPAddress.Loopback, 9999))
        printfn "Connecting to host"
        tcpClient.Connect(serverEndpoint)

        let stream = tcpClient.GetStream()

        let magicTokenBytes = BigEndian.toBytes(0x22400891)
        let bytes = Hello hello |> Wire.encode |> encode
        let bytesLength = bytes.Length |> BigEndian.to4Bytes
        stream.Write(magicTokenBytes, 0, 4)
        stream.Write(bytesLength, 0, 4)
        stream.Write(bytes, 0, bytes.Length)
        stream.Flush()

        let getResponse = 
            async {
                let! responseMagicToken = stream.AsyncRead 4
                let! responseLengthBytes = stream.AsyncRead 4
                let responseLength = responseLengthBytes |> BigEndian.ofBytes
                let! responseBytes = stream.AsyncRead responseLength
                return responseBytes |> decode |> Wire.decode
            }

        let response = Async.RunSynchronously(getResponse, 2000)

        // just testing the TCP stuff so expect the same message back
        response |> should equal (Some (Hello hello))





