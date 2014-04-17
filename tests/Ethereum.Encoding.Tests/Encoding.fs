module Ethereum.Encoding.Tests

open Ethereum.Common
open Ethereum.Encoding
open FsUnit
open Xunit

// Data

let [<Literal>] cat = "cat"
let [<Literal>] dog = "dog"
let [<Literal>] lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit"

// Helpers

let bytes (x: string) =
    Bytes (String.toBytes x)

let codec (rlp, expected) =
    encode rlp 
    |> fun x -> x, decode x 
    |> should equal (expected, rlp)

// Item Tests

[<Fact>]
let ``items of length 0 are encoded as a single prefix byte`` () =
    codec (Bytes [||], [| 0x80uy |])

[<Fact>]
let ``items of length 1 of value 0x00 are encoded as themselves`` () =
    codec (Bytes [| 0x00uy |], [| 0x00uy |])

[<Fact>]
let ``items of length 1 in the range 0x00 - 0x7f are encoded as themselves`` () =
    codec (Bytes [| 0x15uy |], [| 0x15uy |])

[<Fact>]
let ``items of length < 55 are encoded with a single prefix byte`` () =
    codec (bytes dog, Array.concat [ [| 0x83uy |]; String.toBytes dog ])
    
[<Fact>]
let ``items of length > 55 are encoded with a prefix byte and big endian binary length`` () =
    codec (bytes lorem, Array.concat [ [| 0xb8uy; 0x38uy |]; String.toBytes lorem ])

// List Tests

[<Fact>]
let ``lists of length 0 are encoded as a single prefix byte`` () =
    codec (Array [||], [| 0xc0uy |])

[<Fact>]
let ``lists of encoded length < 55 are encoded with a single prefix byte`` () =
    codec (Array [| bytes cat; bytes dog |], Array.concat [ [| 0xc8uy |]; encode (bytes cat); encode (bytes dog) ])

[<Fact>]
let ``lists of encoded length > 55 are encoded with a prefix byte and big endian binary length`` () =
    codec (Array [| bytes lorem |], Array.concat [ [| 0xf8uy; 0x3auy |]; encode (bytes lorem) ])
