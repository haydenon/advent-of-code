open System
open System.Collections

let getBinaryRepresentation char =
  let bitsToBoolList (bits : string) =
    bits.ToCharArray()
    |> Array.map ((=)'1')
    |> Array.toList
  match char with
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' -> "1010"
  | 'B' -> "1011"
  | 'C' -> "1100"
  | 'D' -> "1101"
  | 'E' -> "1110"
  | 'F' -> "1111"
  | _ -> failwith "Invalid input"
  |> bitsToBoolList

let loadData () =
  let hex =
    System.IO.File.ReadAllLines("./input.txt")
    |> Array.head
  hex.ToCharArray()
  |> Array.toList
  |> List.collect getBinaryRepresentation

let bitsToInt64 (bits : bool list) =
  let boolArray = bits |> List.toArray
  let bitArray = BitArray(Array.rev boolArray)
  let byteArray: byte[] = Array.zeroCreate 8;
  bitArray.CopyTo(byteArray, 0)
  BitConverter.ToInt64(byteArray, 0);

type PacketDetails =
  {
    Version : int
    PacketType : int
  }

type Packet =
  | Child of PacketDetails * int64
  | Parent of PacketDetails * Packet list

let rec parseLiteral version packet =
  let chunks =
    packet
    |> List.chunkBySize 5
  let endingChunk =
    chunks
    |> List.findIndex (fun ch ->  List.head ch |> not)
  let chunks = chunks |> List.take (endingChunk + 1)
  let value =
    chunks
    |> List.map (List.skip 1)
    |> List.collect id
    |> bitsToInt64
  let length = (endingChunk + 1) * 5
  Child({ Version = int version; PacketType = 4 }, value), (length + 6)

and parseBySubpacketCount version packetType packet =
  let packetCount = packet |> List.take 11 |> bitsToInt64 |> int
  let remainder = packet |> List.skip 11
  let parseSubPacket (packets, totalCount) _ =
    let subpacket, length = parsePacket (remainder |> List.skip totalCount)
    (subpacket :: packets, totalCount + length)
  let (packets, count) =
    seq { 1..packetCount }
    |> Seq.fold parseSubPacket ([], 0)
  Parent({ Version = int version; PacketType = int packetType }, packets |> List.rev), count + 18

and parseBySubpacketsLength version packetType packet =
  let packetsLength = packet |> List.take 15 |> bitsToInt64 |> int
  let remainder = packet |> List.skip 15
  let rec parseSubPackets packets remainingLength =
    if remainingLength <= 0
    then packets |> List.rev
    else
      let (packet, length) =
        remainder
        |> List.skip (packetsLength - remainingLength)
        |> parsePacket
      parseSubPackets (packet :: packets) (remainingLength - length)
  let subpackets = parseSubPackets [] packetsLength
  Parent({ Version = int version; PacketType = int packetType }, subpackets), packetsLength + 22

and parsePacket packet =
  let version = packet |> List.take 3 |> bitsToInt64
  let packetType = packet |> List.skip 3 |> List.take 3 |> bitsToInt64
  let remainder = packet |> List.skip 6
  match packetType with
  | 4L -> parseLiteral version remainder
  | _ ->
    let mode = remainder |> List.head
    let remainder = remainder |> List.skip 1
    match mode with
    | true -> parseBySubpacketCount version packetType remainder
    | false -> parseBySubpacketsLength version packetType remainder

let rec sumVersions packet =
  match packet with
  | Child({ Version = version}, _) -> version
  | Parent({ Version = version}, children) ->
    let childSum =
      children
      |> List.map sumVersions
      |> List.sum
    version + childSum

let binary op (packets : int64 list) =
  let first = packets[0]
  let second = packets[1]
  if op first second then 1L else 0L

let rec evaluate packet =
  match packet with
  | Child(_, value) -> value
  | Parent({ PacketType = packetType }, children) ->
    let childValues = children |> List.map evaluate
    let op =
      match packetType with
      | 0 -> List.sum
      | 1 -> List.reduce (*)
      | 2 -> List.min
      | 3 -> List.max
      | 5 -> binary (>)
      | 6 -> binary (<)
      | 7 -> binary (=)
      | _ -> failwith "Invalid packet type"
    op childValues

    // Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    // Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
    // Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
    // Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
    // Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    // Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
    // Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.

let data = loadData()
let packet =
  parsePacket data
  |> fst

packet
|> sumVersions
|> printfn "Part 1: %d"

packet
|> evaluate
|> printfn "Part 2: %d"
