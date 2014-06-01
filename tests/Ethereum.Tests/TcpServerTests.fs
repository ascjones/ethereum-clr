namespace Ethereum.Tests

module TcpServerTests =

    open FsUnit
    open Xunit

    [<Fact>]
    let ``True`` () =
        true |> should equal true
