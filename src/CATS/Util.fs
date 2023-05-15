module CATS.Util 

open System
open System.Collections.Generic

exception AnalysisException of String 

/// Given a number n, computes all lists of booleans of length n 
let rec computeBooleanPowerSet n =
    if n = 0 then
        Seq.singleton []
    else
        let r = computeBooleanPowerSet (n-1)
        Seq.append (Seq.map (fun x -> true::x) r) (Seq.map (fun x -> false::x) r)

let rec computeFinitePowerSet (options : seq<'T>) n =
    if n = 0 then
        Seq.singleton []
    else
        let r = computeFinitePowerSet options (n-1)
        
        options
        |> Seq.map (fun x -> r |> Seq.map (fun y -> x::y))
        |> Seq.concat
    

let rec cartesianProduct (LL: list<seq<'a>>) =
    match LL with
    | [] -> Seq.singleton []
    | L :: Ls ->
        seq {
            for x in L do
                for xs in cartesianProduct Ls -> x :: xs
        }

let rec combineStringsWithSeperator (s: String) (l: list<String>) = 
    match l with 
    | [] -> ""
    | [x] -> x
    | x::y::xs -> 
        x + s + combineStringsWithSeperator s (y::xs)

let dictToMap (d : Dictionary<'A, 'B>) = 
    d 
    |> Seq.map (fun x -> x.Key, x.Value)
    |> Map.ofSeq

module ParserUtil = 
    open FParsec
    
    let escapedStringParser : Parser<string, unit> =
        let escapedCharParser : Parser<string, unit> =  
            anyOf "\"\\/bfnrt"
            |>> fun x -> 
                match x with
                | 'b' -> "\b"
                | 'f' -> "\u000C"
                | 'n' -> "\n"
                | 'r' -> "\r"
                | 't' -> "\t"
                | c   -> string c

        pchar '"' >>. (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                            (pstring "\\" >>. escapedCharParser)) .>> pchar '"'
