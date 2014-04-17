module Ethereum.Encoding

open System
open System.Text
open Ethereum.Common
open FSharpx
open FSharpx.Collections

// Types

type RLP =
    | Bytes of byte []
    | Array of RLP []

// Constants

let [<Literal>] private cut = 0x37
let [<Literal>] private item = 0x80 
let [<Literal>] private list = 0xc0

// Encode

let private (|S|M|L|) (data: _ []) =
    match data, data.Length with
    | x, 1 -> S (x)
    | x, l when l <= cut -> M (x, l)
    | x, l -> L (x, l)

let private prefix off data =
    match data with
    | S (x) -> x
    | M (x, l) -> byte (off + l) &>! x
    | L (x, l) -> BigEndian.toBinary l |> fun l -> byte (off + l.Length + cut) &>! l &&! x

let rec encode rlp =
    match rlp with
    | Bytes i -> prefix item i
    | Array l -> prefix list (Array.foldBack (fun r x -> encode r &&! x) l [||])

// Decode

let private deprefix off data =
    match data &<> 1 with
    | [| x |], xs when x < byte item -> [| x |], xs
    | [| x |], xs when x < byte (off + cut) -> xs &<> (int x - off)
    | [| x |], xs -> xs &<> (int x - off - cut) |> mapFst BigEndian.ofBinary ||> (flip (&<>))
    | _ -> [||], [||]

let private (|B|A|) (data: _ []) =
    match data.[0] with
    | x when x < byte list -> B (deprefix item data)
    | _ -> A (deprefix list data)

let rec private decodeList rlps data =
    match data with
    | [||] -> rlps
    | B (i, [||]) -> rlps &<! Bytes (i)
    | A (l, [||]) -> rlps &<! Array (decodeList [||] l)
    | B (i, data) -> decodeList (rlps &<! Bytes (i)) data
    | A (l, data) -> decodeList (rlps &<! Array (decodeList [||] l)) data       

let decode data =
    (decodeList [||] data).[0]

// Patterns

let private encoding = Encoding.UTF8

let (|RLPInt|_|) x =
    match x with
    | Bytes x -> Some (BigEndian.ofBinary x)
    | _ -> None

let RLPInt x =
    Bytes (BigEndian.toBinary x)

let (|RLPString|_|) x =
    match x with
    | Bytes x -> Some (String.ofBytes x)
    | _ -> None

let RLPString (x: string) =
    Bytes (String.toBytes x)
    