module CATS.CommandLineParser

open System

type ExecutionMode =
    | CauseCheck of String
    | CauseSketch of String

type CommandLineArguments =
    { 
        ExecMode: option<ExecutionMode>
        UseContingencies : bool 

        Log : bool 
        LogAutoHyper : bool 
        RaiseExceptions : bool
    }

    static member Default =
        { 
            ExecMode = None 
            UseContingencies = false

            Log = false 
            LogAutoHyper = false 
            RaiseExceptions = false
        }

let rec private splitByPredicate (f: 'T -> bool) (xs: list<'T>) =
    match xs with
    | [] -> [], []
    | x :: xs ->
        if f x then
            [], x :: xs
        else
            let r1, r2 = splitByPredicate f xs
            x :: r1, r2

exception CommandLineParsingException of String

let parseCommandLineArguments (args: list<String>) =
    let rec parseArgumentsRec (args: list<String>) (opt: CommandLineArguments) =

        match args with
        | [] -> opt
        | x :: xs ->
            match x with
            | "--check" ->
                let args, ys = splitByPredicate (fun (x: String) -> x.[0] = '-') xs

                if List.length args < 1 then
                    raise <| CommandLineParsingException "Option --check must be followed by an argument"
                else
                    let instancePath = args[0]
                    
                    parseArgumentsRec
                        ys
                        { opt with
                            ExecMode = instancePath |> CauseCheck |> Some     
                        }
            | "--sketch" ->
                let args, ys = splitByPredicate (fun (x: String) -> x.[0] = '-') xs

                if List.length args < 1 then
                    raise <| CommandLineParsingException "Option --sketch must be followed by an argument"
                else
                    let instancePath = args[0]
                    
                    parseArgumentsRec
                        ys
                        { opt with
                            ExecMode = instancePath |> CauseSketch |> Some     
                        }

            | "--contingencies" ->
                parseArgumentsRec
                    xs
                    { opt with
                        UseContingencies = true  
                    }
            | "--log" ->
                parseArgumentsRec
                    xs
                    { opt with
                        Log = true  
                    }
            | "--log-mc" ->
                parseArgumentsRec
                    xs
                    { opt with
                        LogAutoHyper = true  
                    }
            | _ -> raise <| CommandLineParsingException("Option " + x + " is not supported")

    parseArgumentsRec args CommandLineArguments.Default

