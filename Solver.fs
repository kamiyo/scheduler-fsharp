module Solver

// NOT COMPILED OR RUN - written against the OR-Tools .NET API from memory.
// Add the package first:  dotnet add package Google.OrTools
// In the .fsproj, compile order must be: Common.fs, Scheduler.fs, Solver.fs, Program.fs

open System.IO
open System.Text.Json
open Google.OrTools.Sat
open Common
open Scheduler // for scoreStudent and formatSchedule

// One candidate assignment: "this student in this slot", plus the 0/1 solver
// variable that says whether the solver chose it.
type Placement =
    { StudentName: string
      Slot: int * int * int
      Var: BoolVar }

let private weekOf ((week, _, _): int * int * int) = week

// How much it costs to put a student's second (or third) session in the same
// week. Compare it with your score units (5 and 10):
//   huge (e.g. 1000) -> different weeks always win; same week only if unavoidable
//   small (e.g. 5)   -> same week is allowed when it buys a better time slot
//   0                -> weeks don't matter at all
let sameWeekPenalty = 1000L

// Cost of each break in a day's sessions, i.e. each extra separate block of
// sessions beyond the first. Compare with your score units (5 and 10):
//   below 5  -> density only breaks ties between equally-good schedules
//   10+      -> density can override someone's preferred time
let gapPenalty = 3L

// How to treat an hour that isn't offered at all (like a lunch hour that is
// missing from the slot list):
//   false -> sessions on either side of it count as contiguous
//   true  -> they count as separated by a break
let missingHourIsBreak = false

// Hard limit on back-to-back hours in one day. Only true consecutive hours count,
// so a lunch hour that isn't offered as a slot ends a run. Set it high (e.g. 99)
// to turn the limit off.
let maxHoursInARow = 3

// Minimum number of calendar days between any two of one student's sessions:
//   2 -> not on the same day or on consecutive days
//   1 -> only the same day is banned
//   0 -> no rule
// Days are counted on a calendar (week 2 Monday is 7 days after week 1 Monday),
// so this can only bite when two sessions land in the same week.
let minDaysApart = 2

// Consistency across weeks: each different slot a student uses costs this many
// points, so the solver prefers giving a student the same slot every week.
// Compare with your score units (5 and 10):
//   1-3   -> consistency only breaks ties (it never costs preference points)
//   5+    -> it starts competing with density
//   10+   -> it can override someone's preferred time
//   0     -> off
let consistencyPenalty = 2L

// true  -> "same slot" means same weekday AND same time (Tue 1400 every week)
// false -> only the weekday has to match (Tuesday, any time)
let consistencyUsesTime = true

// Deadline rule: every student must have at least this many sessions on or
// before the deadline day (counting from week 1). Set to 0 to turn it off.
// A student who needs fewer sessions than this only needs all of theirs by then.
let lessonsRequiredByDeadline = 2
let deadlineWeek = 3
let deadlineDay = 1 // 0 = Mon, 1 = Tue, 2 = Wed, 3 = Thu, 4 = Fri

// Stop searching after this many seconds and use the best schedule found so far.
// The best schedule is usually found quickly; most of the time after that goes
// into proving nothing better exists. The status line shows Optimal (proven) or
// Feasible (stopped by the time limit).
let timeLimitSeconds = 30

let private patternOf ((_, day, time): int * int * int) =
    if consistencyUsesTime then (day, time) else (day, 0)

let private calendarDay ((week, day, _): int * int * int) = (week - 1) * 7 + day

// Stability: keep the new schedule close to a previous one. The previous schedule
// is passed to scheduleWithSolver as an optional path to an earlier output JSON
// from this program (only its "Weeks" part is read). None turns stability off.

// Cost of a session that is NOT in a slot the student already had. Each such
// session is compared with the student's nearest previous session and charged:
//   changePenalty + perDayShiftPenalty * (calendar days away)
//                 + perHourShiftPenalty * (hours of time-of-day difference)
// so a small shift costs less than a big one. Compare with your score units
// (5 and 10): around 5 makes the solver reluctant to move people, 10+ makes it
// give up a preferred time rather than move someone.
let changePenalty = 5L
let perDayShiftPenalty = 1L
let perHourShiftPenalty = 1L

// Reads an earlier output JSON into: student name -> the slots they had.
let private loadPreviousSchedule (path: string) : Map<string, (int * int * int) list> =
    use doc = JsonDocument.Parse(File.ReadAllText path)
    let root = doc.RootElement
    // The file is a list of solutions; use the first one.
    let solution = if root.ValueKind = JsonValueKind.Array then root.[0] else root

    [ for week in solution.GetProperty("Weeks").EnumerateArray() do
        let w = week.GetProperty("Week").GetInt32()

        for day in week.GetProperty("Days").EnumerateArray() do
            let d = day.GetProperty("Day").GetInt32()

            for slot in day.GetProperty("Slots").EnumerateArray() do
                let name = slot.GetProperty("Name").GetString()

                if not (System.String.IsNullOrEmpty name) then
                    name, (w, d, slot.GetProperty("Time").GetInt32()) ]
    |> List.groupBy fst
    |> List.map (fun (name, pairs) -> name, pairs |> List.map snd)
    |> Map.ofList

// Cost of putting a student in `slot`, given the slots they had before.
let private moveCost (before: (int * int * int) list) (slot: int * int * int) : int64 =
    if List.isEmpty before || List.contains slot before then
        0L
    else
        let (_, _, time) = slot

        before
        |> List.map (fun prev ->
            let (_, _, prevTime) = prev

            changePenalty
            + perDayShiftPenalty * int64 (abs (calendarDay slot - calendarDay prev))
            + perHourShiftPenalty * int64 (abs (time - prevTime) / 100))
        |> List.min

let scheduleWithSolver (input: string) (output: string) (previousPath: string option) =

    // ---- 1. Load and map the input exactly like Scheduler.schedule does ----
    let json = File.ReadAllText input
    let data = JsonSerializer.Deserialize<InputShape>(json, inputOpts)
    let weeks = data.slots.Length
    let slotsTuples = mapSlots data.slots
    let studentDB = makeStudentsMap weeks slotsTuples data.students

    let studentByName =
        studentDB |> List.map (fun s -> s.Name, s) |> Map.ofList

    // [("Xu Lu", 3); ("Zhihan Li", 2); ...]  - how many sessions each student needs
    let counts = data.needed |> List.countBy id

    // ---- 2. Create the model and one 0/1 variable per (student, available slot) ----
    // x = 1 means "this student is scheduled in this slot".
    let model = CpModel()

    let placements =
        [ for (name, _) in counts do
            let student = studentByName.[name]
            for slot in student.AvailableSlots do
                { StudentName = name
                  Slot = slot
                  Var = model.NewBoolVar($"{name}@{slot}") } ]

    // Adds up a group of variables into one expression, e.g. x1 + x2 + x3
    let sumOf (group: Placement seq) =
        LinearExpr.Sum(group |> Seq.map (fun p -> p.Var :> LinearExpr))

    // ---- 3. Constraints. They all use the same pattern: ----
    //      pick a group of variables, add them up, and bound the total.

    // (a) Each student gets exactly as many sessions as they need.
    //     "Xu Lu" needs 3: sum of all Xu Lu variables must be exactly 3.
    for (name, k) in counts do
        let mine = placements |> List.filter (fun p -> p.StudentName = name)
        model.AddLinearConstraint(sumOf mine, int64 k, int64 k) |> ignore

    // (b) One student per slot.
    //     Group every variable that refers to the same slot, e.g. all students
    //     who could use (week 2, Tue, 1400). At most one of them can be 1.
    for (_slot, group) in placements |> List.groupBy (fun p -> p.Slot) do
        model.AddLinearConstraint(sumOf group, 0L, 1L) |> ignore

    // (c) Different weeks are preferred, not required (soft constraint).
    //     For each (student, week) group, `extra` counts how many sessions beyond
    //     the first landed in that week:   sum(x) - extra <= 1
    //     `extra` can't go below 0, and the objective charges sameWeekPenalty for
    //     each unit, so the solver keeps it at 0 unless that's too costly.
    let extras =
        [ for ((name, week), group) in
              placements |> List.groupBy (fun p -> p.StudentName, weekOf p.Slot) do
            let extra =
                model.NewIntVar(0L, int64 (List.length group), $"extra_{name}_w{week}")

            let terms =
                (group |> List.map (fun p -> p.Var :> LinearExpr)) @ [ extra :> LinearExpr ]

            let coeffs = (group |> List.map (fun _ -> 1L)) @ [ -1L ]

            model.AddLinearConstraint(LinearExpr.WeightedSum(terms, coeffs), -1000L, 1L)
            |> ignore

            extra ]

    // (d) Density: fewer breaks within a day.
    //     A "block" is a run of consecutive occupied slots. For each slot we make a
    //     `start` variable that must be 1 whenever the slot is occupied and the
    //     slot before it is not:   start >= used(this) - used(previous)
    //     Each block start costs gapPenalty. Every used day has at least one block,
    //     so a `dayUsed` bonus gives that first block back, which leaves a charge
    //     only for the extra blocks (the breaks).
    let daySlotGroups =
        slotsTuples |> Set.toList |> List.groupBy (fun (w, d, _) -> w, d)

    let placementsBySlot =
        placements |> List.groupBy (fun p -> p.Slot) |> Map.ofList

    // The 0/1 variables for "someone is in this slot" (at most one is 1).
    let usedAt slot =
        placementsBySlot
        |> Map.tryFind slot
        |> Option.defaultValue []
        |> List.map (fun p -> p.Var)

    let startVars =
        [ for ((week, day), daySlots) in daySlotGroups do
            let prevs =
                None :: (daySlots |> List.map Some |> List.truncate (daySlots.Length - 1))

            for (prev, ((_, _, time) as cur)) in List.zip prevs daySlots do
                let curVars = usedAt cur

                if not (List.isEmpty curVars) then
                    let prevVars =
                        match prev with
                        | Some((_, _, prevTime) as p) when
                            not missingHourIsBreak || time - prevTime = 100
                            ->
                            usedAt p
                        | _ -> []

                    let start = model.NewBoolVar($"start_w{week}_d{day}_t{time}")

                    // start - used(cur) + used(prev) >= 0
                    let terms =
                        [ start :> LinearExpr ]
                        @ (curVars |> List.map (fun v -> v :> LinearExpr))
                        @ (prevVars |> List.map (fun v -> v :> LinearExpr))

                    let coeffs =
                        [ 1L ]
                        @ (curVars |> List.map (fun _ -> -1L))
                        @ (prevVars |> List.map (fun _ -> 1L))

                    model.AddLinearConstraint(LinearExpr.WeightedSum(terms, coeffs), 0L, 10L)
                    |> ignore

                    start ]

    let dayUsedVars =
        [ for ((week, day), daySlots) in daySlotGroups do
            let dayVars = daySlots |> List.collect usedAt

            if not (List.isEmpty dayVars) then
                let dayUsed = model.NewBoolVar($"dayUsed_w{week}_d{day}")

                // dayUsed can only be 1 if at least one session is in this day:
                // sum(used) - dayUsed >= 0
                let terms =
                    (dayVars |> List.map (fun v -> v :> LinearExpr)) @ [ dayUsed :> LinearExpr ]

                let coeffs = (dayVars |> List.map (fun _ -> 1L)) @ [ -1L ]

                model.AddLinearConstraint(LinearExpr.WeightedSum(terms, coeffs), 0L, 1000L)
                |> ignore

                dayUsed ]

    // (e) No more than maxHoursInARow back-to-back hours.
    //     Slide a window of (maxHoursInARow + 1) slots along each day. If the window
    //     is truly consecutive hours (no missing hour inside it), at most
    //     maxHoursInARow of those slots may be occupied.
    for (_day, daySlots) in daySlotGroups do
        for window in List.windowed (maxHoursInARow + 1) daySlots do
            let (_, _, firstTime) = List.head window
            let (_, _, lastTime) = List.last window

            if lastTime - firstTime = 100 * maxHoursInARow then
                let vars = window |> List.collect usedAt

                if not (List.isEmpty vars) then
                    model.AddLinearConstraint(
                        LinearExpr.Sum(vars |> List.map (fun v -> v :> LinearExpr)),
                        0L,
                        int64 maxHoursInARow
                    )
                    |> ignore

    // (f) A student's sessions are at least minDaysApart calendar days apart.
    //     Any two sessions closer than that fit inside a window of minDaysApart
    //     consecutive calendar days, so each such window may hold at most one
    //     of the student's sessions.
    if minDaysApart > 0 then
        for (_name, mine) in placements |> List.groupBy (fun p -> p.StudentName) do
            let windowStarts =
                mine |> List.map (fun p -> calendarDay p.Slot) |> List.distinct

            for start in windowStarts do
                let window =
                    mine
                    |> List.filter (fun p ->
                        let d = calendarDay p.Slot
                        d >= start && d < start + minDaysApart)

                if List.length window > 1 then
                    model.AddLinearConstraint(sumOf window, 0L, 1L) |> ignore

    // (g) Consistency across weeks.
    //     A "pattern" is a weekday + time (e.g. Tue 1400), the same in every week.
    //     Each (student, pattern) gets a `patternUsed` variable forced to 1 whenever
    //     the student has a session in that pattern in any week. The objective
    //     charges consistencyPenalty per pattern used. Since a student's session
    //     count is fixed, fewer patterns used means more repeated slots.
    let patternVars =
        [ for (name, mine) in placements |> List.groupBy (fun p -> p.StudentName) do
            for (pattern, group) in mine |> List.groupBy (fun p -> patternOf p.Slot) do
                let patternUsed = model.NewBoolVar($"pattern_{name}_{pattern}")

                for p in group do
                    // patternUsed - x >= 0
                    model.AddLinearConstraint(
                        LinearExpr.WeightedSum([ patternUsed :> LinearExpr; p.Var :> LinearExpr ], [ 1L; -1L ]),
                        0L,
                        1L
                    )
                    |> ignore

                patternUsed ]

    // (h) Deadline: at least lessonsRequiredByDeadline sessions per student on or
    //     before (deadlineWeek, deadlineDay). Same pattern as the other constraints:
    //     group the student's variables that fall before the deadline, bound the sum.
    let isByDeadline ((week, day, _): int * int * int) =
        (week, day) <= (deadlineWeek, deadlineDay)

    if lessonsRequiredByDeadline > 0 then
        for (name, k) in counts do
            let early =
                placements
                |> List.filter (fun p -> p.StudentName = name && isByDeadline p.Slot)

            let required = min lessonsRequiredByDeadline k
            model.AddLinearConstraint(sumOf early, int64 required, int64 k) |> ignore

    // ---- 4. Objective: maximize the total score ----
    // Each variable is weighted by the score of that student in that slot, so the
    // objective is the sum of (score * x). It uses your existing scoreStudent, so
    // it optimizes whatever scoreStudent returns.
    let scoreOf (p: Placement) =
        int64 (scoreStudent studentByName.[p.StudentName] p.Slot).Total

    // Stability: what each student had in the previous schedule (empty = off).
    let previous =
        match previousPath with
        | Some path -> loadPreviousSchedule path
        | None -> Map.empty

    let stabilityCostOf (p: Placement) =
        moveCost (Map.tryFind p.StudentName previous |> Option.defaultValue []) p.Slot

    // Total score of the chosen placements, minus the same-week penalty,
    // minus the break penalty (block starts, with the first block of each day refunded),
    // minus the consistency penalty (one charge per different slot a student uses),
    // minus the stability cost (moving a session away from the previous schedule).
    let objectiveTerms =
        (placements |> List.map (fun p -> p.Var :> LinearExpr))
        @ (extras |> List.map (fun e -> e :> LinearExpr))
        @ (startVars |> List.map (fun v -> v :> LinearExpr))
        @ (dayUsedVars |> List.map (fun v -> v :> LinearExpr))
        @ (patternVars |> List.map (fun v -> v :> LinearExpr))

    let objectiveWeights =
        (placements |> List.map (fun p -> scoreOf p - stabilityCostOf p))
        @ (extras |> List.map (fun _ -> -sameWeekPenalty))
        @ (startVars |> List.map (fun _ -> -gapPenalty))
        @ (dayUsedVars |> List.map (fun _ -> gapPenalty))
        @ (patternVars |> List.map (fun _ -> -consistencyPenalty))

    model.Maximize(LinearExpr.WeightedSum(objectiveTerms, objectiveWeights))

    // ---- 5. Solve ----
    let solver = new CpSolver()
    // Stop after timeLimitSeconds and take the best found so far.
    solver.StringParameters <- $"max_time_in_seconds:{timeLimitSeconds}"
    // Optional: print the solver's progress. Put it in the same string as the time
    // limit above, because assigning StringParameters again replaces the old value:
    // solver.StringParameters <- $"max_time_in_seconds:{timeLimitSeconds} log_search_progress:true"
    let status = solver.Solve model

    match status with
    | CpSolverStatus.Optimal
    | CpSolverStatus.Feasible ->
        // Optimal = proven best. Feasible = valid, but stopped (time limit) before proving it.

        // ---- 6. Read the answer back out ----
        let chosen =
            placements |> List.filter (fun p -> solver.BooleanValue p.Var)

        let occupied: Map<int * int * int, string> =
            chosen |> List.map (fun p -> p.Slot, p.StudentName) |> Map.ofList

        // The objective includes penalties, so recompute the plain score for reporting.
        let realScore = chosen |> List.sumBy (fun p -> int (scoreOf p))
        let sameWeekCount = extras |> List.sumBy (fun e -> int (solver.Value e))
        let countTrue (vars: BoolVar list) =
            vars |> List.sumBy (fun v -> if solver.BooleanValue v then 1 else 0)

        let breakCount = countTrue startVars - countTrue dayUsedVars

        printfn
            "Status: %A, score: %d, extra same-week sessions: %d, breaks: %d"
            status realScore sameWeekCount breakCount

        // List every student who ended up with more than one session in a week.
        let sameWeek =
            chosen
            |> List.groupBy (fun p -> p.StudentName, weekOf p.Slot)
            |> List.filter (fun (_, group) -> List.length group > 1)
            |> List.sortBy fst

        if List.isEmpty sameWeek then
            printfn "No student has more than one session in the same week."
        else
            printfn "Students with multiple sessions in the same week:"

            for ((name, week), group) in sameWeek do
                let times =
                    group
                    |> List.map (fun p -> p.Slot)
                    |> List.sort
                    |> List.map (fun (_, day, time) -> $"{dayNumToStr day} {time}")
                    |> String.concat ", "

                printfn "  %s, week %d: %s" name week times

        // Consistency: who has the same slot every week, and who doesn't.
        let describePattern (day, time) =
            if consistencyUsesTime then $"{dayNumToStr day} {time}" else dayNumToStr day

        let byStudent = chosen |> List.groupBy (fun p -> p.StudentName) |> List.sortBy fst

        let inconsistent =
            byStudent
            |> List.filter (fun (_, ps) -> (ps |> List.map (fun p -> patternOf p.Slot) |> List.distinct |> List.length) > 1)

        printfn
            "Students with the same slot every week: %d of %d"
            (List.length byStudent - List.length inconsistent)
            (List.length byStudent)

        for (name, ps) in inconsistent do
            let parts =
                ps
                |> List.groupBy (fun p -> patternOf p.Slot)
                |> List.map (fun (pattern, group) ->
                    let weeks =
                        group |> List.map (fun p -> weekOf p.Slot) |> List.sort |> List.map string |> String.concat ", "

                    $"{describePattern pattern} (week {weeks})")
                |> String.concat "; "

            printfn "  %s: %s" name parts

        // Stability: how this schedule differs from the previous one.
        if not (Map.isEmpty previous) then
            let describeSlot (week, day, time) = $"Week {week} {dayNumToStr day} {time}"

            let diffs =
                [ for (name, ps) in chosen |> List.groupBy (fun p -> p.StudentName) |> List.sortBy fst do
                    let nowSlots = ps |> List.map (fun p -> p.Slot)
                    let before = Map.tryFind name previous |> Option.defaultValue []
                    let dropped = before |> List.filter (fun sl -> not (List.contains sl nowSlots))
                    let added = nowSlots |> List.filter (fun sl -> not (List.contains sl before))

                    if not (List.isEmpty added) || not (List.isEmpty dropped) then
                        name, dropped, added ]

            let changedCount = diffs |> List.sumBy (fun (_, _, added) -> List.length added)

            printfn "Sessions that differ from the previous schedule: %d of %d" changedCount (List.length chosen)

            for (name, dropped, added) in diffs do
                let show slots =
                    slots |> List.sort |> List.map describeSlot |> String.concat ", "

                printfn "  %s: dropped [%s]; added [%s]" name (show dropped) (show added)

        let bestSolutions = ref ([ occupied ], realScore)
        formatSchedule output slotsTuples studentDB bestSolutions

    | CpSolverStatus.Infeasible ->
        // No schedule satisfies all the constraints at once (e.g. a student
        // has fewer usable slots than sessions needed).
        failwith "No valid schedule exists for these inputs."

    | other -> failwithf "Solver ended with status %A" other