module CATS.CauseSketching

open System

open FsOmegaLib.GNBA

open AutoHyperQCore.SolverConfiguration

open LassoTrace
open QPTL
open SketchQPTL
open CauseChecking

type SketchMode = 
    | PostiveConjunction
    | PositiveAndNegativeConjunction
    | FullFormula

type CauseSketchingOptions = 
    {
        ConstructCounterfactualAutomaton : bool
        SketchMode : SketchMode
        MaximalNumberOfAttempts : option<int>
        MaximalNumberOfNonPrunedAttempts : option<int>

        Logger : String -> unit
        AutoHyperLogger : String -> unit
        RaiseExceptions : bool
    }

    member this.LoggerN s = this.Logger (s + "\n")



type CauseCandidateInfo = 
    {
        IsPruned : bool 
        Time : int64
    }

type CauseSketchingStatistics = 
    {
        InfosPerCandidate : list<CauseCandidateInfo>
    }

let private allPositiveConjunctions (aps : list<String>) = 
    Util.computeBooleanPowerSet (List.length aps)
    |> Seq.map (fun x -> 
        List.zip x aps 
        |> List.filter fst 
        |> List.map snd 
        |> List.map (fun x -> QPTL.Atom (QPTLAtom.AP x))
        |> fun y -> if List.isEmpty y then QPTL.True else List.reduce (fun a b -> QPTL.And(a, b)) y
            )

let private allPositiveAndNegativeConjunctions (aps : list<String>) = 
    Util.computeFinitePowerSet [0; 1; 2] (List.length aps)
    |> Seq.map (fun x -> 
        List.zip x aps 
        |> List.choose (fun (i, x) -> 
            match i with 
            | 0 -> QPTL.Atom (QPTLAtom.AP x) |> QPTL.Not |> Some
            | 1 -> QPTL.Atom (QPTLAtom.AP x) |> Some
            | _ -> None
            )
        |> fun y -> if List.isEmpty y then QPTL.True else List.reduce (fun a b -> QPTL.And(a, b)) y
        )


let exploreCauseSketch
    (config: SolverConfiguration)
    (options : CauseSketchingOptions)
    (system: GNBA<int, String>)
    (lasso: LassoTrace<String>)
    (causeSketch: SketchQPTL<String, String, list<String>>)
    (effect: QPTL<String, String>)
    (inputs: list<String>)
    =

    let holeFiller = 
        match options.SketchMode with 
        | PostiveConjunction -> allPositiveConjunctions
        | PositiveAndNegativeConjunction -> allPositiveAndNegativeConjunctions
        | _ -> failwith ""

    let checkingOptions = 
        {
            CauseCheckingOptions.ConstructCounterfactualAutomaton = options.ConstructCounterfactualAutomaton
            Logger = options.Logger
            AutoHyperLogger = options.AutoHyperLogger
            RaiseExceptions = options.RaiseExceptions
        }

    let allCauseCandidates = 
        causeSketch
        |> SketchQPTL.generateConcretInstances holeFiller

    let sw = System.Diagnostics.Stopwatch()

    let rec search (attemptCount : int, nonPrunedAttemptCount : int) (candidates : seq<QPTL<String,String>>) = 
        if options.MaximalNumberOfAttempts.IsSome && attemptCount > options.MaximalNumberOfAttempts.Value then 
            options.LoggerN "Reached the maximal cutoff bound"
            None, {InfosPerCandidate = []}
        elif options.MaximalNumberOfNonPrunedAttempts.IsSome && nonPrunedAttemptCount > options.MaximalNumberOfNonPrunedAttempts.Value then 
            options.LoggerN "Reached the maximal cutoff bound (non-pruned attempts)"
            None, {InfosPerCandidate = []}
        else 
            match Seq.tryHead candidates with 
            | None -> None, {InfosPerCandidate = []}
            | Some causeCandidate -> 
                options.LoggerN "================= ================="
                let causeString = QPTL.print (fun x -> "\"" + x + "\"") (fun x -> "\"" + x + "\"") causeCandidate
                options.LoggerN $"Cause Candidate: %s{causeString}"
                sw.Restart()

                let isCause, isPruned = 
                    let holdsOnLasso = LassoTrace.satisfies lasso causeCandidate
                    if not holdsOnLasso then 
                        false, true
                    else 
                        let r, _ = checkPotentialCause config checkingOptions system lasso causeCandidate effect inputs
                        r, false

                let info = {CauseCandidateInfo.IsPruned = isPruned; Time = sw.ElapsedMilliseconds}

                if isCause then 
                    Some (causeCandidate), {InfosPerCandidate = [info]}
                else 
                    let r, li = search (attemptCount + 1, nonPrunedAttemptCount + if isPruned then 0 else 1) (Seq.tail candidates)
                    r, {InfosPerCandidate = info :: li.InfosPerCandidate}

            
    search (0, 0) allCauseCandidates


    