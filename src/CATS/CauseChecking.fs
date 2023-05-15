module CATS.CauseChecking

open System

open FsOmegaLib.GNBA

open AutoHyperQCore.HyperQPTL
open AutoHyperQCore.SolverConfiguration
open AutoHyperQCore.ModelChecking

open LassoTrace
open QPTL
open CounterfactualAutomaton

let swAutoHyper = System.Diagnostics.Stopwatch()

type CauseCheckingOptions = 
    {
        ConstructCounterfactualAutomaton : bool

        Logger : String -> unit
        AutoHyperLogger : String -> unit
        RaiseExceptions : bool
    }

    member this.LoggerN s = this.Logger (s + "\n")


type FailedStage = 
    | PC1 
    | PC2 
    | PC31 
    | PC32
    | PC4 


type CauseCheckingStatistics = 
    {
        FailedStage : Option<FailedStage>
        TimesPerCheck : list<int64>
    }


let checkPotentialCause
    (solverConfig: SolverConfiguration)
    (options : CauseCheckingOptions)
    (system: GNBA<int, String>)
    (lasso: LassoTrace<String>)
    (cause: QPTL<String, String>)
    (effect: QPTL<String, String>)
    (inputs: list<String>)
    =

    let gnba = 
        if options.ConstructCounterfactualAutomaton then 
            options.Logger "Started CA Construction...."
            let counterfactualAutomaton = CounterfactualAutomaton.constructCounterfactualGnba system lasso inputs |> GNBA.convertStatesToInt
            options.LoggerN "Done"

            // We do not need the counterfactual proposition for the check, so we project it away
            let gnba =
                counterfactualAutomaton
                |> GNBA.projectToTargetAPs (
                    counterfactualAutomaton.APs
                    |> List.filter (fun y ->
                        match y with
                        | AP _ -> true
                        | Contingency _ -> false)
                )
                |> GNBA.mapAPs (fun x ->
                    match x with
                    | AP y -> y
                    | Contingency _ -> failwith "Not possible")
            gnba
        else 
            system

    let referenceTraceVariable = "REF"

    //let pc1, pc2, pc3_1, pc3_2, pc4 =
    //    QueryConstructor.constructQuery referenceTraceVariable cause effect inputs

    let pcs = QueryConstructor.constructQuery referenceTraceVariable cause effect inputs

    let lassoSystem = LassoTrace.constructSystem lasso

    let mcOptions = 
        {
            ModelCheckingOptions.ComputeWitnesses = false
            InitialSystemSimplification = true
            IntermediateSimplification = true
            Logger = options.AutoHyperLogger
            RaiseExceptions = options.RaiseExceptions
        }

    // Given a list of trace variables, produce a map as needed for AutoHyperQ
    let createTsMap (traceVars : list<String>) = 
        traceVars
        |> List.map (fun x -> 
            let s = 
                if x = referenceTraceVariable then 
                    // This trace is resolved on the lasso 
                    lassoSystem
                else 
                    gnba
            x, s
            )
        |> Map.ofList
    
    swAutoHyper.Start()

    let sw = System.Diagnostics.Stopwatch()
    
    let mutable times = []

    let r =
        options.Logger "Checking PC1...."
        sw.Restart()
        let res, _ = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions (pcs.PC1 |> HyperQPTL.quantifiedTraceVariables |> createTsMap) pcs.PC1
        times <- times @ [sw.ElapsedMilliseconds]
        if res then
            options.LoggerN $"holds. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
            options.Logger "Checking PC4..."
            sw.Restart()

            let res, _ = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions (pcs.PC4 |> HyperQPTL.quantifiedTraceVariables |> createTsMap) pcs.PC4
            times <- times @ [sw.ElapsedMilliseconds]
            if res then
                options.LoggerN $"holds. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                options.Logger "Checking PC2..."
                sw.Restart()

                let res, _ = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions (pcs.PC2 |> HyperQPTL.quantifiedTraceVariables |> createTsMap) pcs.PC2
                times <- times @ [sw.ElapsedMilliseconds]
                if res then
                    options.LoggerN $"holds. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                    options.Logger "Checking PC3.2 (small implication)..."
                    sw.Restart()

                    let res, _ = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions (pcs.PC32 |> HyperQPTL.quantifiedTraceVariables |> createTsMap) pcs.PC32
                    times <- times @ [sw.ElapsedMilliseconds]
                    if res then
                        options.LoggerN $"holds. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                        options.Logger "Checking PC3.1 (complex implication)..."
                        sw.Restart()

                        let res, _ = AutoHyperQCore.ModelChecking.modelCheck solverConfig mcOptions (pcs.PC31 |> HyperQPTL.quantifiedTraceVariables |> createTsMap) pcs.PC31
                        times <- times @ [sw.ElapsedMilliseconds]
                        if res then
                            options.LoggerN $"holds. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                            true, {CauseCheckingStatistics.FailedStage = None; TimesPerCheck = times}
                        else
                            options.LoggerN $"does not hold. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                            false, {CauseCheckingStatistics.FailedStage = Some PC31; TimesPerCheck = times}
                    else
                        options.LoggerN $"does not hold. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                        false, {CauseCheckingStatistics.FailedStage = Some PC32; TimesPerCheck = times}
                else
                    options.LoggerN $"does not hold. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                    false, {CauseCheckingStatistics.FailedStage = Some PC2; TimesPerCheck = times}
            else
                options.LoggerN $"does not hold. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
                false, {CauseCheckingStatistics.FailedStage = Some PC4; TimesPerCheck = times}
        else
            options.LoggerN $"does not hold. %i{sw.ElapsedMilliseconds}ms (%.4f{double (sw.ElapsedMilliseconds) / 1000.0}s)"
            false, {CauseCheckingStatistics.FailedStage = Some PC1; TimesPerCheck = times}

    swAutoHyper.Stop()

    r



