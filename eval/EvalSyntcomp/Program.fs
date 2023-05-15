module EvalSyntcomp.Program

open System
open System.Collections.Generic

open FsOmegaLib.JSON
open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open AutoHyperQCore.SolverConfiguration

open CATS.CauseChecking
open CATS.CauseSketching
open CATS.InstanceUtil

open Util

let raiseExceptions = false

type Res = 
    {
        FailedStage : Option<FailedStage>
        Time : int64
        Size : int 
        Inputs : int
    }


let run args = 
    let config = SolverConfiguration.getConfig()

    let catsConfig = 
        {
            AutoHyperQCore.SolverConfiguration.SolverConfiguration.MainPath = Some config.MainPath
            Ltl2tgbaPath = Some config.Ltl2tgbaPath
            AutfiltPath = Some config.AutfiltPath
        }

    let numberOfFormulas = 10 
    let lassoLength = 5

    let options: CauseCheckingOptions = 
        {
            CauseCheckingOptions.ConstructCounterfactualAutomaton = false
            Logger = fun _ -> ()
            AutoHyperLogger = fun _ -> ()
            RaiseExceptions = raiseExceptions
        }

    let swTotal = System.Diagnostics.Stopwatch()
    swTotal.Start()

    let c = 
        System.IO.File.ReadAllText "./syntcomp_instances.json"

    let json = 
        match FsOmegaLib.JSON.Parser.parseJsonString c with 
        | Result.Ok x -> x 
        | Result.Error e -> failwith e

    let instances = 
        JSON.getList json

    let instances = 
        instances 
        |> List.mapi (fun i instance -> 
            printfn $"Gen Instances for %i{i}/%i{instances.Length}"
            PrepareSyntcompInstance.genInstances config numberOfFormulas lassoLength instance
        )
        |> List.concat

    
    printfn $"Number of instances: %i{instances.Length}"

    let mutable results = []

    let mutable c = 0

    let sw = System.Diagnostics.Stopwatch() 

    for instance in instances do 
        try 
            printf $"Starting (%i{c+1}/%i{instances.Length})..."

            sw.Restart()
            let _, (stats: CauseCheckingStatistics) =
                CATS.CauseChecking.checkPotentialCause catsConfig options instance.System instance.Lasso instance.Cause instance.Effect instance.Inputs
 
            printfn "Done"

            let r = 
                {
                    Res.FailedStage = stats.FailedStage
                    Time = sw.ElapsedMilliseconds
                    Size = instance.System.States.Count
                    Inputs = instance.Inputs.Length
                }

            results <- r :: results
        
        with 
        | _ -> 
            ()

        c <- c + 1

    let sizeData = 
        results
        |> List.map (fun x -> 
            ["size", x.Size |> double |> JNumber; 
            "time",  x.Time |> double |> JNumber]
            |> Map.ofList
            |> JObject
        )
        |> JList
        |> JSON.toString

    printfn $"\n\n%s{sizeData} \n\n"

    printfn $"Overall time: %i{swTotal.ElapsedMilliseconds} ms (%.4f{double(swTotal.ElapsedMilliseconds) / 1000.0} s)"

[<EntryPoint>]
let main args =
    try 
        run args
    with
    | Util.EvalException err | AutoHyperQCore.Util.AutoHyperQCoreException err | CATS.Util.AnalysisException err when raiseExceptions ->
        printfn $"Error: %s{err}"
        reraise ()
    | _ when raiseExceptions -> reraise ()
    | Util.EvalException err | AutoHyperQCore.Util.AutoHyperQCoreException err | CATS.Util.AnalysisException err ->
        printfn $"Error: %s{err}"
        exit -1
    | e ->
        printfn $"General Error: %s{e.Message}"
        exit -1
    0