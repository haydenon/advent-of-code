open System

let parseServices (services : string) =
  services.Split(",")
  |> Array.filter (fun ch -> ch <> "x")
  |> Array.map Int32.Parse

let loadFirstData () =
  let lines = System.IO.File.ReadAllLines("./input.txt")
  let services = parseServices lines.[1]
  (Int32.Parse lines.[0], services)

let parseServiceTimeline (services : string) =
  let tryParse (text : string) =
    let (success, num) = Int64.TryParse text
    if success
    then Some num
    else None
  services.Split(",")
  |> Array.map tryParse

let loadSecondData () =
  let lines = System.IO.File.ReadAllLines("./input.txt")
  parseServiceTimeline lines.[1]

let findEarliest start services =
  let sortedByWait =
    services
    |> Array.map (fun sid -> (sid, sid - (start % sid)))
    |> Array.sortBy (fun (_, wait) -> wait)
  sortedByWait.[0]

let isDivisible specified (num : int64) =
  specified
  |> Array.forall (fun (ind, sid) -> (num + int64 ind) % sid = int64 0)

let findServiceTimeline (services : int64 option array) =
  let specified =
    services
    |> Array.indexed
    |> Array.filter (fun (_, num) -> Option.isSome num)
    |> Array.map    (fun (ind, num) -> (int64 ind, Option.get num))
    |> Array.sortByDescending (fun (_,num) -> num)
    |> Array.toList

  let rec getEarliestTime start period buses =
    match buses with
    | (idx, busId) :: tail ->
      let time =
        seq { start..period..Int64.MaxValue }
        |> Seq.find (fun time -> (time + idx) % busId = 0L)
      getEarliestTime time (period * busId) tail
    | [] -> start

  getEarliestTime 100000000000000L 1L specified

[<EntryPoint>]
let main _ =
  let (start, services) = loadFirstData()
  let (service, wait) = findEarliest start services
  printfn "Earliest departure is service %d after %d time (multiplication: %d)" service wait ((int64 service) * (int64 wait))

  let secondServices = loadSecondData()
  findServiceTimeline secondServices
  |> printfn "Starting timestamp is %d"
  0