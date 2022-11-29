let rec loadQuestions (input : string list) groups groupQuestions : char[][] list =
  match input with
  | line :: tail when line.Trim().Length > 0 ->
    loadQuestions tail groups (line.ToCharArray() :: groupQuestions)
  | _ :: tail ->
    loadQuestions tail (groupQuestions :: groups) []
  | [] ->
    groupQuestions :: groups
    |> List.map Array.ofList

let loadData () =
  let lines =
    System.IO.File.ReadAllLines("./input.txt")
    |> List.ofArray
  loadQuestions lines [] []

let groupQuestionDistinctCount (group : char[][]) =
  Array.collect id group
  |> Set.ofArray
  |> Set.count

let groupQuestionSameCount  (group : char[][]) =
  let lines = Array.length group
  let chars = Array.collect id group
  let allYes letter =
    chars
    |> Array.filter (fun ch -> ch = (char letter))
    |> Array.length
    |> (=) lines
  seq { (int 'a')..(int 'z') }
  |> Seq.filter allYes
  |> Seq.length

[<EntryPoint>]
let main _ =
  let data = loadData()
  data
  |> List.sumBy groupQuestionDistinctCount
  |> printfn "Total distinct questions: %d"

  data
  |> List.sumBy groupQuestionSameCount
  |> printfn "Total answered by all:    %d"
  0