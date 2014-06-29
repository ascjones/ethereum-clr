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

    let startTcpServer serverEndpoint =
        let tcpServer = new TcpServer(serverEndpoint)
        printfn "Starting TcpServer"
        tcpServer.Start()
        System.Threading.Thread.Sleep(500)

    [<Fact>]
    let ``Send Hello to TcpServer`` () =
        let hello = 
            { Protocol = ProtocolVersion.PoC1
              Client = "Test"
              Capabilities = Capabilities.PeerDiscovery
              Port = 9999
              Node = None }

        let serverEndpoint = new IPEndPoint(IPAddress.Loopback, 30303)
        startTcpServer serverEndpoint

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

        System.Threading.Thread.Sleep(2000)


