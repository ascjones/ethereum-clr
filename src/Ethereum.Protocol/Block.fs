module Ethereum.Protocol.Block

open System
open Ethereum.Common
open Ethereum.Encoding

type Block =
    { Header: BlockHeader
      Transactions: Transaction list
      Uncles: BlockHeader list }

and BlockHeader =
    { ParentHash: byte list
      UnclesHash: byte list 
      Coinbase: byte list
      StateRoot: byte list
      TransactionsHash: byte list
      Difficulty: int
      Timestamp: int
      ExtraData: byte list
      Nonce: byte list }

and Transaction =
    { Number: int }