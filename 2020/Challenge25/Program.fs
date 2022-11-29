open System

let loadData () =
  let values =
    System.IO.File.ReadAllLines("./input.txt")
    |> Array.map int64
  (values.[0], values.[1])

let [<Literal>] DivideBy = 20201227L

let rec getLoopSize subjectNum currIdx value target  =
  if value = target
  then currIdx
  else
    let value = value * subjectNum
    let value = value % DivideBy
    getLoopSize subjectNum (currIdx + 1) value target

let rec getEncryptionKey loopSize subjectNum currIdx value   =
  if currIdx = loopSize
  then value
  else
    let value = value * subjectNum
    let value = value % DivideBy
    getEncryptionKey loopSize subjectNum (currIdx + 1) value

[<EntryPoint>]
let main _ =
  let (cardPub, doorPub) = loadData()
  let getLoopSize = getLoopSize 7L 0 1L
  let (cardLoop, doorLoop) = (getLoopSize cardPub, getLoopSize doorPub)
  printfn "Card loop is: %d, door loop is: %d" cardLoop doorLoop
  let encryptionKey = getEncryptionKey cardLoop doorPub 0 1L
  printfn "The encryption key is %d" encryptionKey
  0