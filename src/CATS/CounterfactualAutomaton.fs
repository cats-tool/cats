module CATS.CounterfactualAutomaton 

open System.Collections.Generic

open FsOmegaLib.SAT
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.GNBA

open LassoTrace

type ContingencyAP<'L> =
    | AP of 'L
    | Contingency of 'L

let constructCounterfactualGnba (aut : GNBA<'T, 'L>) (lasso : LassoTrace<'L>) (inputAPs : list<'L>) = 

    // We assert that both talk about the same set of APs and remap the lasso so the indicies match those of the automaton
    let lasso = LassoTrace.remapToFixedAPs aut.APs lasso

    let aps = aut.APs
    
    let inputs =
        aps
        |> List.mapi (fun i x ->
            if List.contains x inputAPs then 
                Some i 
            else 
                None
            )
        |> List.choose id
        
    let outputs =
        aps
        |> List.mapi (fun i x ->
            if List.contains x inputAPs then 
                None
            else 
                Some i
            )
        |> List.choose id
        
    let newAps =
        (
            aps
            |> List.map AP
        )
        @
        (
            outputs
            |> List.map (fun j -> Contingency (aps.[j]))
        )
       
    // Bring edges to custom format that enables easier checking for counterfactual transitions
    let ioEdges =
        aut.Skeleton.States
        |> Set.toList
        |> List.map (fun s ->
            let edges = 
                aut.Edges[s]
                |> List.map (fun (g, t) ->
                    g
                    |> List.map (fun clause ->
                        let inputMap =
                            inputs
                            |> List.mapi (fun i j ->
                                if List.contains (PL j) clause then
                                    Some (i, true)
                                elif List.contains (NL j) clause then
                                    Some (i, false)
                                else
                                    None
                                )
                            |> List.choose id
                            |> Map.ofList
                            
                        let outputMap =
                            outputs
                            |> List.mapi (fun i j ->
                                if List.contains (PL j) clause then
                                    Some (i, true)
                                elif List.contains (NL j) clause then
                                    Some (i, false)
                                else
                                    None
                                )
                            |> List.choose id
                            |> Map.ofList
                           
                        inputMap, outputMap, t
                    )  
                )
                |> List.concat
            s, edges
           )
        |> Map.ofList
        
    let initStates = 
        aut.InitialStates
        |> Seq.map (fun x -> x, 0)

    let queue = new Queue<_>(initStates)

    let allStates = new HashSet<_>(initStates)
    let newEdgesDict = new Dictionary<'T * int, list<DNF<int> * ('T * int)>>()

    while queue.Count <> 0 do 
        let s, k = queue.Dequeue()
        
        let nextK =
            if k < LassoTrace.length lasso - 1 then
                k + 1
            else
                // Start at the first position of the loop
                lasso.Prefix.Length
        
        let newEdges = 
            ioEdges.[s]
            |> List.map (fun (i, o, _) ->
               
                // We iterate over every possible evaluation of the output contingencies
                Util.computeBooleanPowerSet outputs.Length
                |> Seq.toList
                |> List.map (fun e ->
                    let constraints =
                        e
                        |> List.mapi (fun i x ->
                            if x then
                                // This contingency is set, so we copy the output value that is used on the lasso step 
                                if Set.contains outputs[i] (LassoTrace.get k lasso) then
                                    // The ith output is set on the lasso, so we add a constraint that this output must be set
                                    Some (i, true)
                                else
                                    Some (i, false)
                            else
                                // The contingency for the ith input is NOT set, so we stay within the system
                                if Map.containsKey i o then
                                    // The ith output is fixed in the current transition, so we add a constraint that the output remain the way it currently is
                                    Some(i, o.[i])
                                else
                                    // The ith output is not fixed, so can be chosen freely
                                    None
                            
                            )
                        |> List.choose id
                        |> Map.ofList
                       
                    // We use constraints to filter out all edges that match the correct outputs
                    
                    ioEdges.Values
                    |> List.concat
                    |> List.map (fun (_, o, t) -> o, t)
                    // Filter out those edges that match all constraints
                    |> List.filter (fun (o, _) ->
                        o
                        |> Map.toList
                        |> List.forall (fun (i, v) ->
                            Map.containsKey i constraints |> not || constraints.[i] = v
                            )
                        )
                    // Join the constraints from the edge and from the constraints
                    |> List.map (fun (o, t) ->
                        let newOutputMap =
                            // We can assume that those two maps agree on all values
                            (Map.toList o, Map.toList constraints)
                            ||> List.append
                            |> Map.ofList
                        
                        newOutputMap, t
                        )
                    |> List.map (fun (o, t) ->
                        // This could also be constricted globally (as it only depneds on the current state and not on the concrete edge)
                        // We do it here to increase readability
                        let inputClause =
                            i
                            |> Map.toList
                            |> List.map (fun (i, v) ->
                                if v then
                                    Literal.PL (inputs.[i])
                                else
                                   Literal.NL (inputs.[i]) 
                                )
                            
                        let outputClause =
                            o
                            |> Map.toList
                            |> List.map (fun (i, v) ->
                                if v then
                                    Literal.PL (outputs.[i])
                                else
                                   Literal.NL (outputs.[i]) 
                                )
                           
                        let offSet = List.length aps
                        
                        let contingencyClause =
                            e
                            |> List.mapi (fun i v ->
                                if v then
                                    Literal.PL (i + offSet)
                                else
                                    Literal.NL (i + offSet)
                                )
                        
                        inputClause @ outputClause @ contingencyClause |> List.singleton, (t, nextK)
                        
                        )
                    )
                    |> List.concat
                )
            |> List.concat
        
        newEdgesDict.Add((s, k), newEdges)
        
        for (_, s) in newEdges do
            if allStates.Contains s |> not then
                allStates.Add s |> ignore
                queue.Enqueue s 
        
    
    {
        GNBA.Skeleton =
            {
                AutomatonSkeleton.States = set allStates
                APs = newAps
                Edges = Util.dictToMap newEdgesDict
            }
        InitialStates = set initStates
        AcceptanceSets =
            allStates
            |> Seq.map (fun (s, k) ->
                (s, k), aut.AcceptanceSets.[s]
                )
            |> Map.ofSeq
        NumberOfAcceptingSets = aut.NumberOfAcceptingSets
    } 
   