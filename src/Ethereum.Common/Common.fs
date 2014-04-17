module Ethereum.Common

open System
open System.Text

[<RequireQualifiedAccess>]
module Array =

    let splitAt split (x: _ []) =
        Array.sub x 0 split, Array.sub x split (x.Length - split)

    let tryGet i (x: _ []) =
        if x.Length > i then Some (x.[i]) else None

[<AutoOpen>]
module ArrayOperators =

    let (&>!) x xs =
        Array.concat [ [| x |]; xs ]

    let (&<!) xs x =
        Array.concat [ xs; [| x |] ]

    let (&&!) x y =
        Array.concat [ x; y ]

    let (&<>) xs i =
        Array.splitAt i xs

[<RequireQualifiedAccess>]
module BigEndian =

    let rec private trim (x: byte []) =
        if x.Length > 0 && x.[0] = 0x00uy then trim (Array.sub x 1 (x.Length - 1)) else x

    let rec private pad (x: byte []) =
        if x.Length < 4 then pad (0x00uy &>! x) else x

    let ofBytes (x: byte []) =
        pad x
        |> fun x -> if BitConverter.IsLittleEndian then Array.rev x else x
        |> fun x -> BitConverter.ToInt32 (x, 0)

    let toBytes (x: int) =
        BitConverter.GetBytes x
        |> fun x -> if BitConverter.IsLittleEndian then Array.rev x else x
        |> trim

[<RequireQualifiedAccess>]
module String =

    let private encoding = Encoding.UTF8

    let ofBytes (x: byte []) = 
        encoding.GetString (x)

    let toBytes (x: string) = 
        encoding.GetBytes (x)

[<AutoOpen>]
module Tuples =

    let mapFst f (x, y) = 
        f x, y
    
    let mapSnd f (x, y) = 
        x, f y
