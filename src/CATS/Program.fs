module CATS.Program

open System.IO

open Util
open CommandLineParser
open QPTL
open CauseChecking
open CauseSketching
open InstanceUtil

let private raiseExceptions= false

[<EntryPoint>]
let main args =
    try
        let swTotal = System.Diagnostics.Stopwatch()
        swTotal.Start()

        let cmdArgs =
            try
                CommandLineParser.parseCommandLineArguments (Array.toList args)
            with CommandLineParsingException msg ->
                raise <| AnalysisException $"Error when parsing args: %s{msg}"

        let config = AutoHyperQCore.SolverConfiguration.getConfig()


        let logger = fun s -> if cmdArgs.Log then printf $"%s{s}"

        match cmdArgs.ExecMode with
        | None ->
            raise
            <| AnalysisException "Must specify input mode (--check or --sketch)"
        | Some(CauseCheck path) ->
            let content =
                try
                    File.ReadAllText(path)
                with
                | _ -> raise <| AnalysisException $"Could not open/read file %s{path}"
            
            let instance =
                match InstanceUtil.Parser.parseCauseCheckInstance config content with
                | Result.Ok x -> x
                | Error msg -> raise <| AnalysisException $"Could not parse instance: %s{msg}"

            match InstanceUtil.CauseCheckInstance.findError instance with 
            | None -> ()
            | Some msg -> raise <| AnalysisException $"Found error in instance: %s{msg}"

            let options: CauseCheckingOptions = 
                {
                    CauseCheckingOptions.ConstructCounterfactualAutomaton = cmdArgs.UseContingencies
                    Logger = logger
                    AutoHyperLogger = fun s -> if cmdArgs.LogAutoHyper then printf $"%s{s}" 
                    RaiseExceptions = raiseExceptions
                }

            let res, _ =
                CauseChecking.checkPotentialCause config options instance.System instance.Lasso instance.Cause instance.Effect instance.Inputs

            options.LoggerN ""
            if res then printfn "Is Cause" else printfn "No Cause"

        | Some(CauseSketch path) ->
            let content =
                try
                    File.ReadAllText(path)
                with
                | _ -> raise <| AnalysisException $"Could not open/read file %s{path}"
            
            let instance =
                match InstanceUtil.Parser.parseCauseSketchInstance config content with
                | Result.Ok x -> x
                | Error msg -> raise <| AnalysisException $"Could not parse instance: %s{msg}"

            match InstanceUtil.CauseSketchInstance.findError instance with 
            | None -> ()
            | Some msg -> raise <| AnalysisException $"Found error in instance: %s{msg}"

            let options: CauseSketchingOptions = 
                {
                    CauseSketchingOptions.ConstructCounterfactualAutomaton = cmdArgs.UseContingencies
                    SketchMode = SketchMode.PositiveAndNegativeConjunction
                    MaximalNumberOfAttempts = None
                    MaximalNumberOfNonPrunedAttempts = None
                    Logger = logger
                    AutoHyperLogger = fun s -> if cmdArgs.LogAutoHyper then printf $"%s{s}" 
                    RaiseExceptions = raiseExceptions
                }

            let res, _ =
                CauseSketching.exploreCauseSketch config options instance.System instance.Lasso instance.CauseSketch instance.Effect instance.Inputs

            options.LoggerN ""

            match res with 
            | None -> 
                printfn "Could not find cause"
            | Some cause -> 
                let causeString = QPTL.print (fun x -> "\"" + x + "\"") (fun x -> "\"" + x + "\"") cause
                printfn $"Found Cause: %s{causeString}"
            
                
        logger "\n"
        logger $"Total Time: %i{swTotal.ElapsedMilliseconds}ms (%.4f{double(swTotal.ElapsedMilliseconds)/ 1000.0}s)\n"

        0
    with
    | Util.AnalysisException err when raiseExceptions ->
        printfn $"Error: %s{err}"
        reraise ()
    | AutoHyperQCore.Util.AutoHyperQCoreException err when raiseExceptions ->
        printfn $"Error: %s{err}"
        reraise ()
    | _ when raiseExceptions -> reraise ()
    | Util.AnalysisException err 
    | AutoHyperQCore.Util.AutoHyperQCoreException err ->
        printfn $"Error: %s{err}"
        exit -1
    | e ->
        printfn $"General Error: %s{e.Message}"
        exit -1

