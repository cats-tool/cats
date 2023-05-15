module EvalSketchRandom.Program

open System
open System.Collections.Generic

open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open AutoHyperQCore.SolverConfiguration

open CATS.CauseChecking
open CATS.CauseSketching
open CATS.InstanceUtil

open Util

type Res = 
    {
        LassoLengths : list<int>
        CauseCandidateInfos : list<CauseCandidateInfo>
    }

let raiseExceptions = false

let run _ = 
    let config = SolverConfiguration.getConfig()

    let catsConfig = 
        {
            AutoHyperQCore.SolverConfiguration.SolverConfiguration.MainPath = Some config.MainPath
            Ltl2tgbaPath = Some config.Ltl2tgbaPath
            AutfiltPath = Some config.AutfiltPath
        }

    let numberOfInstancesPerSize = 50
    let density = 0.1 
    let numberOfInputs = 1
    let maxAttempts = 1000
    let maxNonPrunedAttempts = 10
 
    let options: CauseSketchingOptions = 
        {
            CauseSketchingOptions.ConstructCounterfactualAutomaton = false
            SketchMode = SketchMode.PositiveAndNegativeConjunction
            MaximalNumberOfAttempts = Some maxAttempts
            MaximalNumberOfNonPrunedAttempts = Some maxNonPrunedAttempts
            Logger = fun _ -> ()
            AutoHyperLogger = fun _ -> ()
            RaiseExceptions = raiseExceptions
        }

    let systemSizes = [10..10..200]

    let resultsPerSize = new Dictionary<int,Res>()

    let swTotal = System.Diagnostics.Stopwatch()

    swTotal.Start()

    for systemSize in systemSizes do 
        printfn $"Working on Size: %i{systemSize}"
        let mutable allChecks = []
        let mutable lassoLengths = []

        let instances = GenerateSketchInstances.generateInstances config numberOfInstancesPerSize systemSize density numberOfInputs

        let mutable c = 0

        for instance in instances do 
            printf $"Starting (%i{c+1}/%i{instances.Length})..."
            c <- c + 1
            match CATS.InstanceUtil.CauseSketchInstance.findError instance with 
            | None -> ()
            | Some msg -> raise <| EvalException $"Found error in instance: %s{msg}"

            try 
                let res, (stats: CauseSketchingStatistics) =
                    CATS.CauseSketching.exploreCauseSketch catsConfig options instance.System instance.Lasso instance.CauseSketch instance.Effect instance.Inputs

                let numberOfCandidates = stats.InfosPerCandidate.Length
                let numberOfNonPrunedCandidates = 
                    stats.InfosPerCandidate
                    |> List.filter (fun x -> x.IsPruned |> not)
                    |> List.length

                printfn $"Done (explored %i{numberOfCandidates} candidates, %i{numberOfNonPrunedCandidates} non-pruned)"

                match res with 
                | None -> 
                    ()
                | Some _ -> 
                    ()

                allChecks <- allChecks @ stats.InfosPerCandidate
                lassoLengths <- lassoLengths @ [instance.Lasso.Prefix.Length + instance.Lasso.Loop.Length]
            with 
                | e -> 
                    printfn $"Error: %s{e.Message}"
                    ()
            
        
        let res = 
            {
                LassoLengths = lassoLengths
                CauseCandidateInfos = allChecks
            }

        resultsPerSize.Add (systemSize, res)

    // Print the Results
    for systemSize in systemSizes do 
        let res = resultsPerSize.[systemSize]
    
        // The average length of the counterexample in these instances
        let avgLassoLength =
            res.LassoLengths
            |> List.map double 
            |> List.average

        let avgTimePerNonPrunedChecks = 
            res.CauseCandidateInfos
            |> List.filter (fun x -> x.IsPruned |> not)
            |> List.map (fun x -> double (x.Time))
            |> List.average

        let avgTimePerInstance = 
            res.CauseCandidateInfos
            |> List.map (fun x -> double (x.Time))
            |> List.sum 
            |> fun y -> y / (double numberOfInstancesPerSize)

        printfn "============================================="
        printfn $"System size: %i{systemSize}"
        printfn $"Avg Lasso Length: %f{avgLassoLength}"
        printfn $"Avg time per non-pruned check : %f{avgTimePerNonPrunedChecks}"
        printfn $"Avg time per instance : %f{avgTimePerInstance}"
        printfn "============================================="
        printfn ""

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