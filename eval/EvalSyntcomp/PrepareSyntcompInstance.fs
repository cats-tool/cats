module EvalSyntcomp.PrepareSyntcompInstance 

open System
open System.IO
open System.Collections.Generic

open FParsec

open FsOmegaLib.JSON
open FsOmegaLib.SAT
open FsOmegaLib.LTL
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open CATS.LassoTrace
open CATS.InstanceUtil
open CATS.QPTL
open CATS.SketchQPTL

open Util
open Util.SystemCallUtil
open SolverConfiguration

let rnd = new Random(0)

let private apParser = 
    pipe2 
        letter 
        (manyChars (letter <|> digit <|> pchar '_'))
        (fun x y -> string(x) + y)

let private getRandomFormula (solverConfig : SolverConfiguration) (seed : int) (number : int) (aps : list<String>) =  
    let arg = 
        [
            "--seed";
            string(seed);
            "--tree-size=22..30";
            "--ltl-priorities \"xor=0, M=0\"";
            "-p";
            "-n" + string(number);
            aps |> Util.combineStringsWithSeperator " ";
        ]
        |> Util.combineStringsWithSeperator " "

    let res = Util.SystemCallUtil.systemCall solverConfig.RandltlPath arg

    assert(res.Stderr.Trim() = "")

    let out = res.Stdout
    
    let formulas = 
        out.Split ('\n')
        |> Array.toList
        |> List.map (fun x -> x.Trim())
        |> List.filter (fun x -> x <> "")
        |> List.map (fun x -> 
            match FsOmegaLib.LTL.Parser.parseLTL apParser x with 
            | Result.Ok y -> y 
            | Result.Error e -> failwith e
            )
        |> List.map (fun x -> 
            x 
            |> LTL.map (fun a -> QPTLAtom.AP a)
            |> QPTL.fromLTL)

    formulas

exception FailedLasso 

let private getLassoInGNBA (gnba : GNBA<'T, 'L>) (lassoLength : int) = 
    assert(gnba.NumberOfAcceptingSets = 0)
    assert(gnba.InitialStates.Count = 1)

    let shortestPaths = AutoHyperQCore.GraphUtil.shortestPathsBetweenAllPairs gnba.States (fun x -> gnba.Edges.[x]) false

    let initState = gnba.InitialStates |> Set.toSeq |> Seq.head

    let rec generateLassoPath (length : int) (s : 'T) = 
        if length <= 0 then
            if shortestPaths.ContainsKey(s, s) |> not then 
                raise FailedLasso

            let loop = shortestPaths.[s, s] |> fst

            [], loop
        else 
            let sucs = gnba.Edges.[s]

            let g, s' = sucs.[rnd.Next(sucs.Length - 1)]

            let prefix, loop = generateLassoPath (length - 1) s'

            g:: prefix, loop

    let mapDnfListToSolution (l : list<DNF<int>>) = 
        l 
        |> List.map (fun dnf -> 
            let clause = dnf.[rnd.Next(dnf.Length - 1)]
            
            [0..gnba.APs.Length - 1]
            |> List.filter (fun i -> 
                if List.contains (PL i) clause then 
                    true 
                elif List.contains (NL i) clause then 
                    false 
                else    
                    // Not specified, flip at random
                    rnd.NextDouble() <= 0.5
                )
            |> set
            )

    let prefix, loop = generateLassoPath lassoLength initState

    let lasso = 
        {
            LassoTrace.APs = gnba.APs;
            Prefix = mapDnfListToSolution prefix;
            Loop = mapDnfListToSolution loop
        }

    lasso




let genInstances (solverConfig : SolverConfiguration) numberOfFormulas lassoLength  (instance : Json) = 
    
    match instance |> JSON.lookup "strat" |> JSON.tryGetString with 
    | None -> 
        []
    | Some systemString -> 
        let gnba = 
            match FsOmegaLib.Operations.AutomatonFromString.convertHoaStringToGNBA false solverConfig.MainPath solverConfig.AutfiltPath Effort.LOW systemString with 
            | Success x -> x 
            | Fail err -> failwith err

        let inputs = instance |> JSON.lookup "inputs" |> JSON.getList |> List.map JSON.getString
        let outputs = instance |> JSON.lookup "outputs" |> JSON.getList |> List.map JSON.getString

        if gnba.States.Count > 300 || inputs.Length > 4 || List.isEmpty inputs || List.isEmpty outputs then 
            []
        else 
            try 
                let lasso = getLassoInGNBA gnba lassoLength

                let inputFormulas = getRandomFormula solverConfig (rnd.Next()) numberOfFormulas inputs 
                let outputFormulas = getRandomFormula solverConfig (rnd.Next()) numberOfFormulas outputs 

                List.zip inputFormulas outputFormulas
                |> List.map (fun (cause, effect) -> 
                    {
                        CauseCheckInstance.Cause = cause
                        Effect = effect
                        Inputs = inputs
                        Lasso = lasso
                        System = gnba
                    }
                    )
            with 
            | FailedLasso -> []
