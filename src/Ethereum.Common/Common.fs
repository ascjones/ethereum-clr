module Ethereum.Common

open System
open System.Text

[<RequireQualifiedAccess>]
module BigEndian =

    let rec private trim x =
        match x with
        | x :: xs when x = 0x00uy -> trim xs
        | _ -> x

    let rec private pad x =
        match List.length x with
        | 4 -> x
        | _ -> pad (0x00uy :: x)

    let fromBinary (bytes: byte list) =
        pad bytes
        |> Array.ofList
        |> fun x -> if BitConverter.IsLittleEndian then Array.rev x else x
        |> fun x -> BitConverter.ToInt32 (x, 0)

    let toBinary (x: int) =
        BitConverter.GetBytes x
        |> Array.toList
        |> fun x -> if BitConverter.IsLittleEndian then List.rev x else x
        |> trim

[<RequireQualifiedAccess>]
module List =

    let tryHead list =
        List.tryFind (fun _ -> true) list

[<RequireQualifiedAccess>]
module String =

    let ofBytes (x: byte list) = 
        Encoding.UTF8.GetString (Array.ofList x)

    let toBytes (x: string) = 
        Encoding.UTF8.GetBytes (x) |> List.ofArray

[<AutoOpen>]
module Tuples =

    let mapFst f (x, y) = 
        f x, y
    
    let mapSnd f (x, y) = 
        x, f y
