module CATS.LassoTrace

open System

open FsOmegaLib.SAT
open FsOmegaLib.AutomatonSkeleton
open FsOmegaLib.GNBA

open Util
open QPTL

type LassoTrace<'L> = 
    {
        APs: list<'L>
        Prefix : list<Set<int>>
        Loop : list<Set<int>>
    }

module LassoTrace = 
    let length (lasso : LassoTrace<'L>) = 
        List.length lasso.Prefix
        + 
        List.length lasso.Loop

    let print (stringer : 'L -> String) (lasso : LassoTrace<'L>) = 
        let apString = "AP: " + (lasso.APs |> List.map stringer |> List.map (fun x -> "\"" + x + "\"") |> Util.combineStringsWithSeperator " ")
        
        let prefixString = 
            "Prefix: " + (lasso.Prefix |> List.map (fun s -> s |> Set.toList |> List.map string |> Util.combineStringsWithSeperator " " |> fun x -> "{" + x + "}") |> Util.combineStringsWithSeperator " ")
        
        let loopString = 
            "Loop: " + (lasso.Loop |> List.map (fun s -> s |> Set.toList |> List.map string |> Util.combineStringsWithSeperator " " |> fun x -> "{" + x + "}") |> Util.combineStringsWithSeperator " ")
        
        apString + "\n" + prefixString + "\n" + loopString

    let remapToFixedAPs (aps : list<'L>) (lasso : LassoTrace<'L>) =
        if set aps <> set lasso.APs then
            raise <| AnalysisException "Can only remap lasso when the same APs are used"
            
        let remapping =
            lasso.APs
            |> List.mapi (fun i a ->
                i, List.findIndex ((=) a) aps
                )
            |> Map.ofList
            
        {
            LassoTrace.APs = aps
            Prefix =
                lasso.Prefix
                |> List.map (fun s ->
                    s |> Set.map (fun i -> remapping.[i])
                    )
            Loop =
                lasso.Loop
                |> List.map (fun s ->
                    s |> Set.map (fun i -> remapping.[i])
                    )
        }
            
        
    let get (i : int) (lasso : LassoTrace<'L>) =
        if i < lasso.Prefix.Length then
            lasso.Prefix.[i]
        else
            lasso.Loop[(i - lasso.Prefix.Length) % lasso.Loop.Length]

    // Construct a system that generates exactly this lasso trace
    let constructSystem (lasso : LassoTrace<'L>) =
        let states = [0..(length lasso) - 1] |> set

        let edges= 
            states 
            |> Set.toList
            |> List.map (fun x -> 
                let nextState = 
                    if x < length lasso - 1 then
                        x + 1
                    else
                        lasso.Prefix.Length

                let guard : DNF<int> = 
                    [0..lasso.APs.Length - 1]
                    |> List.map (fun i -> 
                        if Set.contains i (get x lasso) then 
                            PL i 
                        else 
                            NL i
                        )
                    |> List.singleton

                x, [(guard, nextState)]
            )
            |> Map.ofList

        {
            GNBA.Skeleton = 
                {
                    AutomatonSkeleton.States = [0..(length lasso) - 1] |> set
                    APs = lasso.APs
                    Edges = edges
                }
            InitialStates = Set.singleton 0
            AcceptanceSets = 
                states 
                |> Set.toList
                |> List.map (fun x -> x, Set.empty)
                |> Map.ofList
            NumberOfAcceptingSets = 0
        }

    let satisfies (lasso : LassoTrace<'L>) (formula : QPTL<'L, 'U>) = 

        let rec checkPosition i (f : QPTL<'L, 'U>) = 
            match f with 
            | Atom (AP x) -> 
                let apIndex = lasso.APs |> List.findIndex (fun y -> y = x)
                get i lasso |> Set.contains apIndex
            | Atom (QAP _) -> failwith ""
            | True -> true 
            | False -> false 
            | And(e1, e2) -> 
                checkPosition i e1 && checkPosition i e2
            | Implies(e1, e2) -> 
                not (checkPosition i e1) || checkPosition i e2
            | Equiv(e1, e2) -> 
                let res1 = checkPosition i e1
                let res2 = checkPosition i e2
                (res1 && res2) || (not res1 && not res2)
            | Xor(e1, e2) -> 
                let res1 = checkPosition i e1
                let res2 = checkPosition i e2
                (res1 && not res2) || (not res1 && res2)
            | Or(e1, e2) -> 
                checkPosition i e1 || checkPosition i e2
            | U(e1, e2) -> 
                let maxIndex = 
                    if i < length lasso then 
                        length lasso - 1 + lasso.Loop.Length - 1
                    else 
                        i + lasso.Loop.Length - 1

                match [i..maxIndex] |> List.tryFindIndex (fun j -> checkPosition j e2) with 
                | None -> false
                | Some j -> 
                    [i..j - 1]
                    |> List.forall (fun j -> checkPosition j e1)
            | W(e1, e2) -> 
                checkPosition i (G e2) || checkPosition i (U(e1, e2))
            | M _ -> failwith ""
            | R _ -> failwith ""
            | F e -> 
                let maxIndex = 
                    if i < length lasso then 
                        length lasso - 1 + lasso.Loop.Length - 1
                    else 
                        i + lasso.Loop.Length - 1
                [i..maxIndex]
                |> List.exists (fun j -> checkPosition j e)
            | G e -> 
                let maxIndex = 
                    if i < length lasso then 
                        length lasso - 1 + lasso.Loop.Length - 1
                    else 
                        i + lasso.Loop.Length - 1
                [i..maxIndex]
                |> List.forall (fun j -> checkPosition j e)
            | X e -> 
                checkPosition (i + 1) e
            | Not e -> checkPosition i e |> not
            | Exists _ -> failwith ""
            | Forall _ -> failwith ""

        checkPosition 0 formula
