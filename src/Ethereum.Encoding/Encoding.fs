module Ethereum.Encoding

open System
open System.Text
open Ethereum.Common
open FSharpx.Collections

// Types

type RLP =
    | Item of byte list
    | List of RLP list

// Constants

let [<Literal>] private cut = 0x37
let [<Literal>] private item = 0x80 
let [<Literal>] private list = 0xc0

// Encode

let private (|S|M|L|) data =
    match data, List.length data with
    | d, 1 -> S (d)
    | d, l when l <= cut -> M (d, l)
    | d, l -> L (d, l)

let private prefix off data =
    match data with
    | S (d) -> d
    | M (d, l) -> byte (off + l) :: d
    | L (d, l) -> BigEndian.toBinary l |> fun l -> byte (off + List.length l + cut) :: l @ d

let rec encode rlp =
    match rlp with
    | Item i -> prefix item i
    | List l -> prefix list (List.foldBack (fun r x -> encode r @ x) l [])

// Decode

let private deprefix off data =
    match data with
    | x :: xs when x < byte item -> [ x ], xs
    | x :: xs when x < byte (off + cut) -> List.splitAt (int x - off) xs
    | x :: xs -> List.splitAt (int x - off - cut) xs |> mapFst BigEndian.fromBinary ||> List.splitAt
    | _ -> [], []

let private (|I|L|) data =
    match List.tryHead data with
    | Some (x) when x < byte list -> I (deprefix item data)
    | _ -> L (deprefix list data)

let rec private decodeList rlps data =
    match data with
    | [] -> rlps
    | I (i, []) -> rlps @ [ Item (i) ]
    | L (l, []) -> rlps @ [ List (decodeList [] l) ]
    | I (i, data) -> decodeList (rlps @ [ Item (i) ]) data
    | L (l, data) -> decodeList (rlps @ [ List (decodeList [] l) ]) data       

let decode data =
    decodeList [] data |> List.head

// Patterns

let private encoding = Encoding.UTF8

let (|RLPInt|_|) x =
    match x with
    | Item x -> Some (BigEndian.fromBinary x)
    | _ -> None

let RLPInt x =
    Item (BigEndian.toBinary x)

let (|RLPString|_|) x =
    match x with
    | Item x -> Some (encoding.GetString (Array.ofList x))
    | _ -> None

let RLPString (x: string) =
    Item (Array.toList (encoding.GetBytes (x)))
    