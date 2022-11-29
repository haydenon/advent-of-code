open System

type Field = { Name      : string
               BottomMin : int
               BottomMax : int
               TopMin    : int
               TopMax    : int}

let parseField (text : string) =
  let parseRange (text : string) =
    let range = text.Split('-') |> Array.map Int32.Parse
    (range.[0], range.[1])
  let parts = text.Split(':')
  let name = parts.[0]
  let ranges = parts.[1].Split("or")
  let (bottomMin, bottomMax) = parseRange ranges.[0]
  let (topMin, topMax) = parseRange ranges.[1]
  { Name = name
    BottomMin = bottomMin
    BottomMax = bottomMax
    TopMin = topMin
    TopMax = topMax }

let parseTicket (text : string) = text.Split(',') |> Array.map Int32.Parse

let isTicketLine (text : string) = Char.IsDigit text.[0]

let rec parseData parsingFields fields tickets lines  =
  match lines with
  | "" :: tail ->
    parseData false fields tickets tail
  | line :: tail when parsingFields ->
    parseData true (parseField line :: fields) tickets tail
  | line :: tail when isTicketLine line ->
    parseData false fields (parseTicket line :: tickets) tail
  | _ :: tail -> parseData false fields tickets tail
  | [] -> (List.rev fields, List.rev tickets)

let loadData () =
  System.IO.File.ReadAllLines("./input.txt")
  |> Array.toList
  |> parseData true [] []

let isValidValueForField value  { BottomMin = bmin; BottomMax = bmax; TopMin = tmin; TopMax = tmax } =
  (value >= bmin && value <= bmax) || (value >= tmin && value <= tmax)

let isValidValue fields value =
  List.exists (isValidValueForField value) fields

let rec getInvalidTicketValues fields tickets invalid invalidTickets =
  let isValidValue = isValidValue fields
  let getInvalidValues ticket =
    ticket |> Array.filter (isValidValue >> not) |> Array.toList
  match tickets with
  | ticket :: tail ->
    match getInvalidValues ticket with
    | [] -> getInvalidTicketValues fields tail invalid invalidTickets
    | inv -> getInvalidTicketValues fields tail (inv @ invalid) (ticket :: invalidTickets)
  | [] -> (invalid, invalidTickets)

let mapTicketsToFieldValues tickets =
  let ticketNum = List.length tickets
  let fieldNum = Array.length tickets.Head
  let fieldValues = Array.init fieldNum (fun _ -> Array.zeroCreate ticketNum)
  let fillForTicket ticketIdx ticket =
    ticket
    |> Array.iteri (fun fieldIdx num -> fieldValues.[fieldIdx].[ticketIdx] <- num)
  tickets |> List.iteri fillForTicket
  fieldValues

let findColumnsForField fieldValues taken field =
  let notTaken idx = not(List.contains idx taken)
  let matches (idx, values) =
    notTaken idx && values |> Array.forall (fun v -> isValidValueForField v field)
  fieldValues
  |> Array.indexed
  |> Array.filter matches
  |> Array.map fst

let rec findColumnsForFields fieldValues taken matches fields =
  match fields with
  | [] -> Some matches
  | field :: tail ->
    let possible = findColumnsForField fieldValues taken field
    possible
    |> Array.choose (fun col -> findColumnsForFields fieldValues (col :: taken) ((field, col) :: matches) tail)
    |> Array.tryHead

let getDepartureFields fields =
  fields
  |> List.filter (fun ({Name = name}, _) -> name.StartsWith "departure")
  |> List.map snd

let multiplyFields (ticket : int[]) fields =
  fields
  |> List.fold (fun acc fieldIdx -> (int64 ticket.[fieldIdx]) * acc) 1L

[<EntryPoint>]
let main _ =
  let (fields, tickets) = loadData()
  let yourTicket = tickets.Head
  let tickets = (List.skip 1 tickets)
  let (invalidValues, invalidTickets) = getInvalidTicketValues fields tickets [] []
  invalidValues
  |> List.sum
  |> printfn "Invalid values sum to: %d"

  let tickets = List.filter (fun t -> not(List.contains t invalidTickets)) tickets
  let fieldValues = mapTicketsToFieldValues tickets
  let orderedFields =
    fields
    |> List.sortBy (fun f -> findColumnsForField fieldValues [] f)
  findColumnsForFields fieldValues [] [] orderedFields
  |> Option.get
  |> getDepartureFields
  |> multiplyFields yourTicket
  |> printfn "Departure values multiplied: %d"
  0