open Solver
open Scheduler

open Argu

type Arguments =
    | [<AltCommandLine("-i")>] Input of path: string
    | [<AltCommandLine("-o")>] Output of path: string
    | [<AltCommandLine("-p")>] Previous of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "input JSON file."
            | Output _ -> "output schedule JSON."
            | Previous _ -> "a previous schedule JSON to compare to."

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<Arguments>()
    let args = parser.ParseCommandLine argv
    let input = args.GetResult Input
    let output = Option.defaultValue "output.json" (args.TryGetResult Output)
    let previous = args.TryGetResult Previous

    Solver.scheduleWithSolver input output previous

    0