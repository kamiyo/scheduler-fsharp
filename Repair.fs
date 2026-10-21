module Repair

open Common

open System.IO
open System.Text.Json
open System.Text.Json.Serialization

type TimeEntry = { Time: int; Name: string; Score: ScoreBreakdown option }

type DayListing = {
    Day: int
    DayName: string
    Slots: TimeEntry list
}

type RepairInputShape = {
    Score: int;
    Schedule: DayListing list
    Repair: string list
}

let mapSkippable f =
    function
    | Skip -> None
    | Include x -> Some (f x)

let options = JsonFSharpOptions.Default().WithUnionTagName("type")

let inputOpts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
options.AddToJsonSerializerOptions(inputOpts)

let existingScheduleToSlots (schedule: DayListing list) =
    schedule
    |> List.mapi
        (fun index day ->
            day.Slots
            |> List.map (fun slot -> ((index, slot.Time), slot.Name)))
    |> List.concat
    |> Map.ofList

let repairSchedule
    (toRepair: string list)
    (slots: Set<int * int>)
    (constraints: MappedStudent list)
    (occupied: Map<int * int, string>)
    (bestSolutions: (Map<int * int, string> list * int) ref)
    (depth: int)
    =

    ()



let repair (repairPath: string) (input: string) (output: string) =

    let json = File.ReadAllText input
    let repair = File.ReadAllText repairPath
    let constraints = JsonSerializer.Deserialize<InputShape>(json, inputOpts)
    let repair = JsonSerializer.Deserialize<RepairInputShape>(repair, inputOpts)

    let slotsTuples = mapAvailability constraints.slots
    let mappedStudents = makeStudentsMap slotsTuples constraints.students

    let occupied: Map<int * int, string> = existingScheduleToSlots repair.Schedule
    let toChange = repair.Repair
    let occupiedWithRemoved = occupied |> Map.filter (fun k v -> List.contains v toChange)

    printfn "%A" occupied
    ()
