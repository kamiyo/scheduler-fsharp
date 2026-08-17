open System.IO
open System.Text.Json
open System.Text.Json.Serialization

type StudentId = StudentId of int
type SlotId = SlotId of int

type RangeInput = int * int
type SlotInput = RangeInput list list

type PreferencesType = {
    Times: (int Set) list Skippable
    Days: int Set Skippable
}

type AvoidancesType = {
    Times: (int Set) list Skippable
    Days: int Set Skippable
}

type Student =
    { Name: string
      AvailableSlots: RangeInput list list
      Preferences: PreferencesType Skippable
      Avoidances: AvoidancesType Skippable }

type MappedPreferences =
    { Times: (int * int) Set Option
      Days: int Set Option }

type MappedAvoidances =
    { Times: (int * int) Set Option
      Days: int Set Option }

type MappedStudent =
    { Name: string
      AvailableSlots: (int * int) Set
      Preferences: MappedPreferences
      Avoidances: MappedAvoidances }

type InputShape =
    { slots: SlotInput
      students: Student list }

let mapSkippable f =
    function
    | Skip -> None
    | Include x -> Some (f x)

let options = JsonFSharpOptions.Default().WithUnionTagName("type")

let inputOpts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
options.AddToJsonSerializerOptions(inputOpts)

let json = File.ReadAllText "input.json"

let data = JsonSerializer.Deserialize<InputShape>(json, inputOpts)

let expandRange (startTime: int, endTime: int) =
    [ for x in startTime .. 100 .. endTime - 1 -> x ]

let mapAvailability (input: SlotInput) =
    input
    |> List.mapi (fun day ranges ->
        ranges
        |> List.collect expandRange
        |> List.map (fun time -> day, time))
    |> List.concat
    |> Set.ofList

let listListToSetTuple (input: int Set list) =
    input |> List.mapi (fun i s -> Set.map (fun (t) -> (i, t)) s) |> Set.unionMany

let slotsTuples = mapAvailability data.slots

let makeStudentsMap (students: Student list) =
    students
    |> List.map (fun s ->
        { Name = s.Name
          AvailableSlots = s.AvailableSlots |> mapAvailability |> Set.intersect slotsTuples
          Preferences = {
            Times = s.Preferences
                    |> mapSkippable (fun p -> p.Times |> mapSkippable listListToSetTuple)
                    |> Option.flatten
            Days = s.Preferences
                    |> mapSkippable (fun p -> p.Days |> mapSkippable id)
                    |> Option.flatten
          }
          Avoidances = {
            Times = s.Avoidances
                    |> mapSkippable (fun a -> a.Times |> mapSkippable listListToSetTuple)
                    |> Option.flatten
            Days = s.Avoidances
                    |> mapSkippable (fun a -> a.Days |> mapSkippable id)
                    |> Option.flatten
          }
        }
    )

let orderTimesByLeastConstraint (others: (MappedStudent list)) (slots: Set<int * int>) =
    slots
    |> Set.toList
    |> List.sortByDescending (fun slot ->
        others
        |> List.filter (fun s -> Set.contains slot s.AvailableSlots)
        |> List.length)

let getNextStudent (slots: Set<int * int>) (students: MappedStudent list) =
    students |> List.sortBy (fun st -> st.AvailableSlots |> Set.intersect slots |> Set.count)

type ScoreBreakdown =
    {
        Preference : int
        Avoidance : int
        Total : int
    }

let scoreStudent (student: MappedStudent) (slot: int * int) =
    let preferences = match student.Preferences.Times with
                            | Some(times) when times |> Set.contains slot -> 5
                            | _ -> 0
                            +
                            match student.Preferences.Days with
                            | Some(days) when days |> Set.contains (fst slot) -> 10
                            | _ -> 0
                            
    let avoidances = match student.Avoidances.Times with
                            | Some(times) when times |> Set.contains slot -> -5
                            | _ -> 0
                            +
                           match student.Avoidances.Days with
                            | Some(days) when days |> Set.contains (fst slot) -> -10
                            | _ -> 0
    
    {
        Preference = preferences
        Avoidance = avoidances
        Total = preferences + avoidances
    }

let maximumRemainingScore
    (slots: Set<int * int>)
    (students: MappedStudent list)
    =
    students
    |> List.sumBy (fun student ->
        student.AvailableSlots
        |> Set.intersect slots
        |> Seq.map (scoreStudent student >> _.Total)
        |> Seq.toList
        |> function
            | [] -> 0
            | scores -> List.max scores)

let hasDeadEnd slots students =
    students
    |> List.exists (fun student ->
        Set.intersect student.AvailableSlots slots
        |> Set.isEmpty)

let mutable bestSolutions: Map<int * int, string> list * int = ([], System.Int32.MinValue)

let mutable nodesVisited = 0L
let mutable nodesPruned = 0L
let mutable solutionsFound = 0L
let mutable nodesDeadEnded = 0L


let rec makeSchedule
    (slots: Set<int * int>)
    (students: (MappedStudent list))
    (occupied: Map<int * int, string>)
    (accumulatedScore: int)
    =
    nodesVisited <- nodesVisited + 1L
    let shouldPrune =
        match bestSolutions with
        | ([], _) -> false
        | (l, score) ->
            let maxRemaining = maximumRemainingScore slots students
            accumulatedScore + maxRemaining <= score

    if shouldPrune then
        nodesPruned <- nodesPruned + 1L
        ()
    else
        match getNextStudent slots students with
        | [] ->
            solutionsFound <- solutionsFound + 1L
            match bestSolutions with
            | ([], _) -> bestSolutions <- ([occupied], accumulatedScore)
            | (l, s) when accumulatedScore = s ->
                bestSolutions <- (occupied :: l, s)
            | (l, s) when accumulatedScore > s ->
                bestSolutions <- ([occupied], accumulatedScore)
            | _ -> ()
        | head :: remaining ->
            head.AvailableSlots
            |> Set.intersect slots
            |> orderTimesByLeastConstraint remaining
            |> Seq.iter (fun slot ->
                let newOccupied = occupied.Add(slot, head.Name)
                let newScore = accumulatedScore + (scoreStudent head slot).Total
                let newSlots = Set.remove slot slots

                if not (hasDeadEnd newSlots remaining) then
                    makeSchedule 
                        (Set.remove slot slots)
                        remaining
                        newOccupied
                        newScore                        
                else
                    nodesDeadEnded <- nodesDeadEnded + 1L)

let occupied: Map<int * int, string> = Map.empty

let mappedStudents = makeStudentsMap data.students

makeSchedule slotsTuples mappedStudents occupied 0

type TimeEntry = { Time: int; Name: string; Score: ScoreBreakdown option }

type Result = {
    Score: int;
    Schedule: Map<string, TimeEntry list>
}

let result =
    match bestSolutions with
    | ([], _) -> [{ Score = 0; Schedule = Map.empty<string, TimeEntry list> }]
    | (lists, score) ->
        lists
        |> List.map (fun l ->
            slotsTuples
            |> Set.toList
            |> List.map (fun key ->
                match Map.tryFind key l with
                | Some(name) -> key, name
                | _ -> key, "")
            |> List.fold
                (fun prev ((d, h), v) ->
                    let day =
                        match d with
                        | 0 -> "Mon"
                        | 1 -> "Tue"
                        | 2 -> "Wed"
                        | 3 -> "Thu"
                        | 4 -> "Fri"
                        | _ -> ""

                    Map.change
                        day
                        (fun e ->
                            let studentScore = 
                                match mappedStudents |> List.tryFind (fun s -> s.Name = v) with
                                | Some(s) -> Some(scoreStudent s (d, h))
                                | None -> None
                            match e with
                            | Some(existing) -> { Time = h; Name = v; Score = studentScore } :: existing |> Some
                            | None -> [ { Time = h; Name = v; Score = studentScore } ] |> Some )
                        prev)
                    Map.empty<string, TimeEntry list>
            |> Map.map (fun k v -> List.rev v)
            |> fun schedule -> { Score = score; Schedule = schedule })

printfn "Nodes visited: %d" nodesVisited
printfn "Nodes pruned: %d" nodesPruned
printfn "Solutions found: %d" solutionsFound
printfn "Deadend Nodes: %d" nodesDeadEnded

let jsonString = JsonSerializer.Serialize(result)

File.WriteAllText("result.json", jsonString)
