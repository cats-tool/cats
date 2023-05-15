module EvalSketchRandom.GenerateSketchInstances 

open System
open System.IO
open System.Collections.Generic

open FsOmegaLib.SAT
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

let private generateRandomSystems (intermediateFilesPath: String) (autfiltPath : String) (randAutPath : String) numberOfSystems numberOfStates aps density = 

    let targetPath = Path.Combine [|intermediateFilesPath; "randomAut.hoa"|]

    let arg = 
        [
            "-D";
            "-Q" + string(numberOfStates);
            "-e" + string(density);
            "-A0";
            "-S";
            "-n" + string(numberOfSystems);
            aps |> Util.combineStringsWithSeperator " ";
            "-o " + targetPath
        ]
        |> Util.combineStringsWithSeperator " "

    let res = Util.SystemCallUtil.systemCall randAutPath arg

    if res.Stderr.Trim() <> "" then 
        raise <| EvalException $"Error by randaut: %s{res.Stderr.Trim()}"
   
    let autString = File.ReadAllText(targetPath)
   
    let a = 
        autString.Split ("--END--", StringSplitOptions.TrimEntries)
        |> Array.toList
        |> List.filter (fun s -> s <> "")
        |> List.map (fun s -> s + "\n--END--")
        |> List.map (fun s -> 
            match FsOmegaLib.Operations.AutomatonFromString.convertHoaStringToGNBA false intermediateFilesPath autfiltPath Effort.LOW s with 
            | Success x -> x 
            | Fail err ->   
                raise <| EvalException $"Error when obtaining GNBA: %s{err}"
            )

    a


let private modifyAndComputeLasso (gnba : GNBA<int, 'L>) (errorAP : 'L) = 
    assert(gnba.InitialStates.Count = 1)

    assert(List.contains errorAP gnba.APs |> not)

    let newAPs = gnba.APs @ [errorAP]
    let errorApIndex = List.length newAPs - 1

    let potentialErrorStates = 
        gnba.States 
        |> Set.filter (fun x -> gnba.InitialStates.Contains x |> not) 
        |> Set.toList

    let errorState = potentialErrorStates.[rnd.Next(potentialErrorStates.Length - 1)]
    
    let newState = gnba.Skeleton.States |> Set.maxElement |> fun x -> x + 1
    let initState = gnba.InitialStates |> Set.toList |> List.head

    let gnba' = 
        {
            GNBA.Skeleton =     
                {
                    AutomatonSkeleton.States = gnba.Skeleton.States |> Set.add newState
                    APs = newAPs
                    Edges = 
                        gnba.Skeleton.Edges
                        |> Map.add newState [(DNF.trueDNF, newState)]
                        |> Map.map (fun _ l -> 
                            // Ensure that the error proposition holds on no regular transition
                            l 
                            |> List.map (fun (g, s) -> g |> List.map (fun clause -> NL errorApIndex :: clause ), s)
                            )
                        |> Map.add errorState [( [[PL errorApIndex]], newState)]
                }
            InitialStates = gnba.InitialStates
            NumberOfAcceptingSets = 0
            AcceptanceSets = 
                gnba.States
                |> Seq.map (fun x -> x, Set.empty)
                |> Map.ofSeq
                |> Map.add newState Set.empty
        }


    let queue = Queue<_>()
    queue.Enqueue ([], initState)
    let mutable sol = None

    while (queue.Count <> 0) do 
        let p, s = queue.Dequeue()

        if s = errorState then 
            sol <- Some p 
            // break
            queue.Clear()
        else 
            for g, s' in gnba'.Edges[s] do 
                let ns = p @ [g], s'
                queue.Enqueue(ns)

    let prefix = 
        sol 
        |> Option.get
        |> List.map (fun g -> 
            let clause = g.[rnd.Next(g.Length - 1)]

            let entry = 
                [0..newAPs.Length - 1]
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

            entry
            )

    let lasso = 
        {
            LassoTrace.APs = newAPs
            Prefix = prefix @ [Set.singleton errorApIndex]
            Loop = [Set.empty] // No APs hold in the lasso
        }

    gnba', lasso

let private generateNextSketch (length) (aps : list<'L>) = 
    [0..length - 1]
    |> List.map (fun i -> 
        let rec construct i = 
            if i <= 0 then 
                SketchQPTL.Hole aps
            else 
                SketchQPTL.X (construct (i - 1))

        construct i
        )
    |> fun x -> 
        if List.isEmpty x then 
            SketchQPTL.True 
        else 
            x
            |> List.reduce (fun a b -> SketchQPTL.And(a, b))
    
let private generateNextEffect (length) (atom : QPTL<'T, 'L>) = 
    let rec construct i = 
        if i <= 0 then 
            atom
        else 
            QPTL.X (construct (i - 1))

    construct length

let generateInstances (config : SolverConfiguration) numberOfSystems (numberOfStates: int) density numberOfInputs = 
    let inputAPs = List.init numberOfInputs (fun i -> "i" + string(i))

    let a = generateRandomSystems config.MainPath config.AutfiltPath config.RandAutPath numberOfSystems numberOfStates inputAPs density

    let benchmarks = 
        a 
        |> List.map (fun x -> 
            
            let gnba, lasso = modifyAndComputeLasso x "error"

            let sketch = 
                {
                    CauseSketchInstance.System = gnba
                    Lasso = lasso
                    CauseSketch = generateNextSketch (lasso.Prefix.Length - 1) inputAPs
                    Effect = generateNextEffect  (lasso.Prefix.Length - 1) (QPTL.Atom (QPTLAtom.AP "error"))
                    Inputs = inputAPs
                }
            sketch
            )

    benchmarks
