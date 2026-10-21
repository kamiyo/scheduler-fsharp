module Common

open System.Text.Json
open System.Text.Json.Serialization


type RangeInput = int * int
type SlotInput = RangeInput list list list

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
      AvailableSlots: (int * int * int) Set
      Preferences: MappedPreferences
      Avoidances: MappedAvoidances }

type InputShape =
    { slots: SlotInput
      needed: string list
      students: Student list }

type ScoreBreakdown =
    {
        Preference : int
        Avoidance : int
        Total : int
    }

let mapSkippable f =
    function
    | Skip -> None
    | Include x -> Some (f x)

let dayNumToStr d =
    match d with
    | 0 -> "Mon"
    | 1 -> "Tue"
    | 2 -> "Wed"
    | 3 -> "Thu"
    | 4 -> "Fri"
    | _ -> ""

let options = JsonFSharpOptions.Default().WithUnionTagName("type")

let inputOpts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
options.AddToJsonSerializerOptions(inputOpts)

let expandRange (startTime: int, endTime: int) =
    [ for x in startTime .. 100 .. endTime - 1 -> x ]

let mapSlots (input: SlotInput) =
    input
    |> List.mapi (fun week weekList ->
        weekList
        |> List.mapi (fun day ranges ->
            ranges
            |> List.collect expandRange
            |> List.map (fun time -> week + 1, day, time))
        |> List.concat)
    |> List.concat
    |> Set.ofList

let mapAvailability (weeks: int) (input: RangeInput list list) =
    input
    |> List.mapi (fun day ranges ->
        ranges
        |> List.collect expandRange
        |> List.map (fun time -> seq { for x in 1 .. weeks -> x, day, time } |> Seq.toList))
        |> List.concat
    |> List.concat
    |> Set.ofList

let listListToSetTuple (input: int Set list) =
    input |> List.mapi (fun i s -> Set.map (fun (t) -> (i, t)) s) |> Set.unionMany

let makeStudentsMap (weeks: int) (slotsTuples) (studentDB: Student list) =
    studentDB
    |> List.map (fun s ->
        { Name = s.Name
          AvailableSlots = s.AvailableSlots |> (mapAvailability weeks) |> Set.intersect slotsTuples
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