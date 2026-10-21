module Scheduler

open Common

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

type StudentId = StudentId of int
type SlotId = SlotId of int

// let orderTimesByLeastConstraint (others: (MappedStudent list)) (slots: Set<int * int * int>) =
//     slots
//     |> Set.toList
//     |> List.sortByDescending (fun slot ->
//         others
//         |> List.filter (fun s -> Set.contains slot s.AvailableSlots)
//         |> List.length)

let orderTimesByLeastConstraint
    (others: MappedStudent list)
    (slots: Set<int * int * int>) =

    slots
    |> Set.toList
    |> List.map (fun slot ->
        let count =
            others
            |> List.sumBy (fun student ->
                if Set.contains slot student.AvailableSlots then 1 else 0)
        slot, count)
    |> List.sortByDescending snd
    |> List.map fst

let getNextStudent (slots: Set<int * int * int>) (students: MappedStudent list) =
    students |> List.sortBy (fun st -> st.AvailableSlots |> Set.intersect slots |> Set.count)

let scoreStudent (student: MappedStudent) ((_, day, time): int * int * int) =
    let dayTime = (day, time)
    let TimePreferences = match student.Preferences.Times with
                            | Some(times) when times |> Set.contains dayTime -> 5
                            | _ -> 0
    let DayPreferences = match student.Preferences.Days with
                            | Some(days) when days |> Set.contains day -> 10
                            | _ -> 0

    let TimeAvoidances = match student.Avoidances.Times with
                            | Some(times) when times |> Set.contains dayTime -> -5
                            | _ -> 0
    let DayAvoidances = match student.Avoidances.Days with
                            | Some(days) when days |> Set.contains day -> -10
                            | _ -> 0

    {
        Preference = TimePreferences + DayPreferences
        Avoidance = TimeAvoidances + DayAvoidances
        Total = TimePreferences + DayPreferences + TimeAvoidances + DayAvoidances
    }



let maximumRemainingScore
    (scoreLookup: Map<string * (int * int * int), int>)
    (slots: Set<int * int * int>)
    (students: MappedStudent list)
    =
    students
    |> List.sumBy (fun student ->
        student.AvailableSlots
        |> Set.intersect slots
        |> Seq.map (fun slot ->
            Map.find (student.Name, slot) scoreLookup)
        |> Seq.toList
        |> function
            | [] -> 0
            | scores -> List.max scores)

// let hasDeadEnd slots students =
//     students
//     |> List.exists (fun student ->
//         Set.intersect student.AvailableSlots slots
//         |> Set.isEmpty)

let hasDeadEnd slots students =
    students
    |> List.exists (fun student ->
        let possible =
            Set.intersect student.AvailableSlots slots


        Set.isEmpty possible)

let mutable nodesVisited = 0L
let mutable nodesPruned = 0L
let mutable solutionsFound = 0L
let mutable nodesDeadEnded = 0L

let countInWeek (week: int) (slots: Map<int * int * int, string>) =
    slots
    |> Map.toSeq
    |> Seq.filter (fun ((w, _, _), _) -> w = week)
    |> Seq.length

let balanceWeight = 5

let removeWeekFromSameStudent (week: int) (studentName: string) (requests: MappedStudent list) =
    requests
    |> List.map (fun request ->
        match request.Name with
        | s when studentName = s ->
            { request with
                AvailableSlots = request.AvailableSlots
                                    |> Set.filter (fun (w, _, _) -> w <> week) }
        | _ ->
            request)

let progressTimer = System.Diagnostics.Stopwatch.StartNew()
let mutable lastReport = 0L
let mutable nextStudentCalls = 0L

let rec makeSchedule
    (scoreLookup: Map<string * (int * int * int), int>)
    (slots: Set<int * int * int>)
    (students: MappedStudent list)
    (occupied: Map<int * int * int, string>)
    (accumulatedScore: int)
    (bestSolutions: (Map<int * int * int, string> list * int) ref)
    =
    nodesVisited <- nodesVisited + 1L
    // let weekDiff = abs (countInWeek 1 occupied - countInWeek 2 occupied)

    let weekDiff = 0

    let shouldPrune =
        match !bestSolutions with
        | ([], _) -> false
        | (l, score) ->
            let maxRemaining = maximumRemainingScore scoreLookup slots students
            let minPossible = max 0 (weekDiff - List.length students)
            // let upperBound = accumulatedScore + maxRemaining - balanceWeight * minPossible
            // upperBound <= score
            accumulatedScore + maxRemaining <= score

    if nodesVisited % 1000L = 0L then

        printfn "Visited %d nodes; pruned %d; solutions %d; dead ends %d" nodesVisited nodesPruned solutionsFound nodesDeadEnded
        printfn "Visited: %d" nodesVisited
        printfn "Pruned: %d" nodesPruned
        printfn "Dead ends: %d" nodesDeadEnded
        printfn "Solutions: %d" solutionsFound

    if shouldPrune then
        nodesPruned <- nodesPruned + 1L
        ()
    else
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let nextStudent = getNextStudent slots students
        timer.Stop()

        nextStudentCalls <- nextStudentCalls + 1L

        if nextStudentCalls % 1000L = 0L then
            printfn "getNextStudent call %d: %d ms"
                nextStudentCalls timer.ElapsedMilliseconds

        match nextStudent with
        | [] ->
            solutionsFound <- solutionsFound + 1L
            printfn "Solution Found"
            // let finalScore = accumulatedScore - balanceWeight * weekDiff

            match !bestSolutions with
            | ([], _) -> bestSolutions := ([occupied], accumulatedScore)
            // | (l, s) when finalScore = s ->
            //     bestSolutions := (occupied :: l, s)
            | (l, s) when accumulatedScore > s ->
                bestSolutions := ([occupied], accumulatedScore)
            | _ -> ()
        | head :: remaining ->
            let candidates =
                head.AvailableSlots
                |> Set.intersect slots

            if Set.isEmpty candidates then
                nodesDeadEnded <- nodesDeadEnded + 1L
            else
                candidates
                |> Set.intersect slots
                |> orderTimesByLeastConstraint remaining
                |> Seq.iter (fun slot ->
                    let week, _, _ = slot
                    let newOccupied = occupied.Add(slot, head.Name)
                    let newScore = accumulatedScore + (scoreStudent head slot).Total
                    let newSlots = Set.remove slot slots
                    let newRemaining = removeWeekFromSameStudent week head.Name remaining

                    if not (hasDeadEnd newSlots newRemaining) then
                        makeSchedule
                            scoreLookup
                            newSlots
                            newRemaining
                            newOccupied
                            newScore
                            bestSolutions
                    else
                        nodesDeadEnded <- nodesDeadEnded + 1L)

// NOT COMPILED OR RUN.
// In Scheduler.fs, replace everything from `type TimeEntry` down to (but not
// including) `let schedule` with this block. The function signature is the same,
// so Solver.fs doesn't need to change.

type TimeEntry = { Time: int; Name: string; Score: ScoreBreakdown option }

type DayResult = {
    Day: int
    DayName: string
    Slots: TimeEntry list
}

type WeekResult = {
    Week: int
    Days: DayResult list
}

// One scheduled session, from a single student's point of view.
type StudentSession = {
    Week: int
    Day: int
    DayName: string
    Time: int
    When: string // e.g. "Week 1, Mon 900"
    Score: ScoreBreakdown
}

type StudentResult = {
    Student: string
    Sessions: StudentSession list // ordered by week, day, time
}

type Result = {
    Score: int
    Weeks: WeekResult list // the schedule laid out week > day > time
    Students: StudentResult list // the same schedule grouped by student
}

let formatSchedule
    (path: string)
    (slotsTuples: Set<int * int * int>)
    (mappedStudents: MappedStudent list)
    (bestSolutions: (Map<int * int * int, string> list * int) ref)
    =
    let studentsByName =
        mappedStudents |> List.map (fun s -> s.Name, s) |> Map.ofList

    // Turns one solution (slot -> student name) into week > day > time entries.
    let buildResult (occupied: Map<int * int * int, string>) (score: int) : Result =
        let weeks =
            slotsTuples
            |> Set.toList // already ordered by (week, day, time)
            |> List.groupBy (fun (week, _, _) -> week)
            |> List.map (fun (week, weekSlots) ->
                let days =
                    weekSlots
                    |> List.groupBy (fun (_, day, _) -> day)
                    |> List.map (fun (day, daySlots) ->
                        let entries =
                            daySlots
                            |> List.map (fun ((_, _, time) as slot) ->
                                match Map.tryFind slot occupied with
                                | Some name ->
                                    { Time = time
                                      Name = name
                                      Score =
                                        studentsByName
                                        |> Map.tryFind name
                                        |> Option.map (fun s -> scoreStudent s slot) }
                                | None ->
                                    // open slot nobody was assigned to
                                    { Time = time; Name = ""; Score = None })

                        { Day = day
                          DayName = dayNumToStr day
                          Slots = entries })

                { Week = week; Days = days })

        // Same solution, grouped by student instead of by time.
        let students =
            occupied
            |> Map.toList // ordered by (week, day, time)
            |> List.groupBy snd
            |> List.sortBy fst
            |> List.map (fun (name, entries) ->
                let sessions =
                    entries
                    |> List.map (fun (((week, day, time) as slot), _) ->
                        let dayName = dayNumToStr day

                        { Week = week
                          Day = day
                          DayName = dayName
                          Time = time
                          When = $"Week {week}, {dayName} {time}"
                          Score = scoreStudent (Map.find name studentsByName) slot })

                { Student = name; Sessions = sessions })

        { Score = score; Weeks = weeks; Students = students }

    let result =
        match bestSolutions.Value with
        | ([], _) -> [ { Score = 0; Weeks = []; Students = [] } ]
        | (solutions, score) -> solutions |> List.map (fun occupied -> buildResult occupied score)

    File.WriteAllText(path, JsonSerializer.Serialize(result))

let schedule (input: string) (output: string) =

    let json = File.ReadAllText input
    let data = JsonSerializer.Deserialize<InputShape>(json, inputOpts)
    let weeks = data.slots.Length
    let slotsTuples = mapSlots data.slots
    let occupied: Map<int * int * int, string> = Map.empty
    let studentDB = makeStudentsMap weeks slotsTuples data.students

    let scoreLookup =
        studentDB
        |> List.collect (fun student ->
            student.AvailableSlots
            |> Set.toList
            |> List.map (fun slot ->
                (student.Name, slot), (scoreStudent student slot).Total))
        |> Map.ofList

    let needed =
        data.needed
        |> List.map
            (fun s ->
                List.find (fun (l: MappedStudent) -> l.Name = s) studentDB)

    let bestSolutions: (Map<int * int * int, string> list * int) ref = ref ([], System.Int32.MinValue)

    makeSchedule scoreLookup slotsTuples needed occupied 0 bestSolutions

    formatSchedule output slotsTuples studentDB bestSolutions

