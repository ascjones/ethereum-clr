module Ethereum.Encoding

open System
open System.Text
open Ethereum.Common
open FSharpx

// Types

type RLP =
    | RLPArray of RLP []
    | RLPBytes of byte []

// Encode

let private (|S|M|L|) (data: _ []) =
    match data, data.Length with
    | x, 1 -> S (x)
    | x, l when l <= 55 -> M (x, l)
    | x, l -> L (x, l)

let private prefix off data =
    match data with
    | S (x) -> x
    | M (x, l) -> byte (off + l) &>! x
    | L (x, l) -> BigEndian.toBytes l |> fun l -> byte (off + l.Length + 0x37) &>! l &&! x

let rec encode rlp =
    match rlp with
    | RLPArray x -> prefix 192 (Array.foldBack (fun r x -> encode r &&! x) x [||])
    | RLPBytes x -> prefix 128 x

// Decode

let private deprefix off data =
    match data &<> 1 with
    | [| x |], xs when x < 128uy -> [| x |], xs
    | [| x |], xs when x < byte (off + 55) -> xs &<> (int x - off)
    | [| x |], xs -> xs &<> (int x - off - 55) |> mapFst BigEndian.ofBytes ||> (flip (&<>))
    | _ -> [||], [||]

let private (|B|A|) (data: _ []) =
    match data.[0] with
    | x when x < 192uy -> B (deprefix 128 data)
    | _ -> A (deprefix 192 data)

let rec private decodeArray rlps data =
    match data with
    | [||] -> rlps
    | A (x, data) -> decodeArray (rlps &<! RLPArray (decodeArray [||] x)) data       
    | B (x, data) -> decodeArray (rlps &<! RLPBytes (x)) data

let decode data =
    (decodeArray [||] data).[0]

// Primitives

let private encoding = Encoding.UTF8

let (|RLPArray|_|) =
    function | RLPArray x -> Some x | _ -> None

let (|RLPBytes|_|) =
    function | RLPBytes x -> Some x | _ -> None

let (|RLPInt|_|) =
    function | RLPBytes x -> Some (BigEndian.ofBytes x) | _ -> None

let RLPInt x =
    RLPBytes (BigEndian.toBytes x)

let (|RLPString|_|) =
    function | RLPBytes x -> Some (String.ofBytes x) | _ -> None

let RLPString (x: string) =
    RLPBytes (String.toBytes x)