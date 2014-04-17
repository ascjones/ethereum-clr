module Ethereum.Encoding.Tests

open Ethereum.Common
open Ethereum.Encoding
open FsUnit
open Xunit

// Data

let [<Literal>] private cat = "cat"
let [<Literal>] private dog = "dog"
let [<Literal>] private lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"

// Helpers

let private bytes (x: string) =
    RLPBytes (String.toBytes x)

let private codec (rlp, expected) =
    encode rlp 
    |> fun x -> x, decode x 
    |> should equal (expected, rlp)

// Bytes

[<Fact>]
let ``bytes of length 0 are encoded as a single prefix byte`` () =
    codec (RLPBytes [||], [| 0x80uy |])

[<Fact>]
let ``bytes of length 1 of value 0x00 are encoded as themselves`` () =
    codec (RLPBytes [| 0x00uy |], [| 0x00uy |])

[<Fact>]
let ``bytes of length 1 in the range 0x00 - 0x7f are encoded as themselves`` () =
    codec (RLPBytes [| 0x15uy |], [| 0x15uy |])

[<Fact>]
let ``bytes of length < 55 are encoded with a single prefix byte`` () =
    codec (bytes dog, 0x83uy &>! String.toBytes dog)
    
[<Fact>]
let ``bytes of length > 55 are encoded with a prefix byte and big endian binary length`` () =
    codec (bytes lorem, [| 0xb8uy; 0x38uy |] &&! String.toBytes lorem)

// Array

[<Fact>]
let ``arrays of length 0 are encoded as a single prefix byte`` () =
    codec (RLPArray [||], [| 0xc0uy |])

[<Fact>]
let ``arrays of encoded length < 55 are encoded with a single prefix byte`` () =
    codec (RLPArray [| bytes cat; bytes dog |], 0xc8uy &>! encode (bytes cat) &&! encode (bytes dog))

[<Fact>]
let ``arrays of encoded length > 55 are encoded with a prefix byte and big endian binary length`` () =
    codec (RLPArray [| bytes lorem |],  [| 0xf8uy; 0x3auy |] &&! encode (bytes lorem))
