module CATS.InstanceUtil 

open System

open FsOmegaLib.GNBA
open FsOmegaLib.Operations

open AutoHyperQCore.SolverConfiguration

open QPTL
open SketchQPTL
open LassoTrace

type CauseCheckInstance = 
    {
        System : GNBA<int, String> 
        Lasso : LassoTrace<String>
        Cause : QPTL<String, String> 
        Effect : QPTL<String, String> 
        Inputs : list<String>
    }
    
module CauseCheckInstance =
    exception private NotWellFormedException of String

    let findError (instance : CauseCheckInstance) =
        try
            if QPTL.isClosed instance.Cause |> not then 
                raise
                <| NotWellFormedException $"Cause formula contains free propositional variables"

            if QPTL.isClosed instance.Effect |> not then 
                raise
                <| NotWellFormedException $"Effect formula contains free propositional variables"

            instance.Inputs
            |> List.iter (fun x ->
                if List.contains x instance.System.APs |> not then
                    raise
                    <| NotWellFormedException $"Input %s{x} was given but is not defined in the system")

            instance.Cause
            |> QPTL.allAtoms
            |> Set.iter (fun x ->
                match x with
                | AP a ->
                    if List.contains a instance.System.APs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the cause but not defined in the system"

                    if List.contains a instance.Inputs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the cause but not declared as an input"
                | QAP _ -> ())

            instance.Effect
            |> QPTL.allAtoms
            |> Set.iter (fun x ->
                match x with
                | AP a ->
                    if List.contains a instance.System.APs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the effect but not defined in the system"

                    if List.contains a instance.Inputs then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the effect but not declared as an output"
                | QAP _ -> ())


            instance.Lasso.APs
            |> List.iter (fun x ->
                if List.contains x instance.System.APs |> not then
                    raise
                    <| NotWellFormedException $"AP %s{x} is used in the lasso but not defined in the system")

            instance.System.APs
            |> List.iter (fun x ->
                if List.contains x instance.Lasso.APs |> not then
                    raise
                    <| NotWellFormedException $"AP %s{x} is used in the system but not defined in the lasso")
            
            None
        with
        | NotWellFormedException msg -> Some msg



type CauseSketchInstance = 
    {
        System : GNBA<int, String> 
        Lasso : LassoTrace<String>
        CauseSketch : SketchQPTL<String, String, list<String>> // The holes contain a list of APs
        Effect : QPTL<String, String> 
        Inputs : list<String>
    }
    
module CauseSketchInstance =
    exception private NotWellFormedException of String

    let findError (instance : CauseSketchInstance) =
        try
            if SketchQPTL.isClosed instance.CauseSketch |> not then 
                raise
                <| NotWellFormedException $"Cause sketch contains free propositional variables"

            if QPTL.isClosed instance.Effect |> not then 
                raise
                <| NotWellFormedException $"Effect formula contains free propositional variables"

            instance.Inputs
            |> List.iter (fun x ->
                if List.contains x instance.System.APs |> not then
                    raise
                    <| NotWellFormedException $"Input %s{x} was given but is not defined in the system")

            instance.CauseSketch
            |> SketchQPTL.allAtoms
            |> Set.iter (fun x ->
                match x with
                | AP a ->
                    if List.contains a instance.System.APs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the cause sketch but not defined in the system"

                    if List.contains a instance.Inputs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the cause but not declared as an input"
                | QAP _ -> ())

            instance.CauseSketch
            |> SketchQPTL.allHoles
            |> Set.iter (fun l ->
                l 
                |> List.iter (fun a -> 
                    if List.contains a instance.System.APs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used for a hole in the cause sketch but not defined in the system"

                    if List.contains a instance.Inputs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used for a hole in the cause sketch but not declared as an input"
                        
                    )
            )

            instance.Effect
            |> QPTL.allAtoms
            |> Set.iter (fun x ->
                match x with
                | AP a ->
                    if List.contains a instance.System.APs |> not then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the effect but not defined in the system"

                    if List.contains a instance.Inputs then
                        raise
                        <| NotWellFormedException $"AP %s{a} is used in the effect but not declared as an output"
                | QAP _ -> ())


            instance.Lasso.APs
            |> List.iter (fun x ->
                if List.contains x instance.System.APs |> not then
                    raise
                    <| NotWellFormedException $"AP %s{x} is used in the lasso but not defined in the system")

            instance.System.APs
            |> List.iter (fun x ->
                if List.contains x instance.Lasso.APs |> not then
                    raise
                    <| NotWellFormedException $"AP %s{x} is used in the system but not defined in the lasso")
            
            None
        with
        | NotWellFormedException msg -> Some msg

    let writeToString (instance : CauseSketchInstance) = 
        let sw = new System.IO.StringWriter()

        sw.WriteLine "[info]"
        sw.WriteLine ""

        sw.WriteLine "[inputs]"

        instance.Inputs
        |> List.iter (fun s -> 
            sw.Write "\""
            sw.Write s 
            sw.Write "\" ")
        sw.WriteLine ""
        sw.WriteLine ""


        sw.WriteLine "[cause]"
        sw.WriteLine (SketchQPTL.print (fun x -> "\"" + x + "\"") id (fun x -> x |> List.map (fun x -> "\"" + x + "\"") |> Util.combineStringsWithSeperator " ") instance.CauseSketch)
        sw.WriteLine ""

        sw.WriteLine "[effect]"
        sw.WriteLine (QPTL.print (fun x -> "\"" + x + "\"") id instance.Effect)
        sw.WriteLine ""

        sw.WriteLine "[lasso]"
        sw.WriteLine (LassoTrace.print id instance.Lasso)
        sw.WriteLine ""

        sw.WriteLine "[system]"
        sw.WriteLine (GNBA.toHoaString string id instance.System)
        sw.WriteLine ""


        sw.ToString()


module Parser =
    open FParsec
    
    let private infoParser =
        skipString "[info]" >>. spaces >>. charsTillString "[inputs]" false Int32.MaxValue
        
    let private inputsParser =
        skipString "[inputs]" >>. spaces >>. many1 (Util.ParserUtil.escapedStringParser .>> spaces)
        
    let private causeParser =
        skipString "[cause]" >>. spaces >>. QPTL.Parser.qptlParser Util.ParserUtil.escapedStringParser

    let private causeSketcheParser =
        skipString "[cause]" >>. spaces >>. SketchQPTL.Parser.sketchQptlParser Util.ParserUtil.escapedStringParser (SketchQPTL.Parser.apListSketchParser Util.ParserUtil.escapedStringParser)
        
    let private effectParser =
        skipString "[effect]" >>. spaces >>. QPTL.Parser.qptlParser Util.ParserUtil.escapedStringParser
    
    let private lassoParser =
        let apParser =
            skipString "AP:" >>. spaces >>. many1 (Util.ParserUtil.escapedStringParser .>> spaces)
            
        let intSetParser =
            between (skipChar '{' .>> spaces) (skipChar '}') (many (pint32 .>> spaces))
            |>> set
            
        let prefixParser =
            skipString "Prefix:" >>. spaces >>. many (intSetParser .>> spaces)
            
        let loopParser =
            skipString "Loop:" >>. spaces >>. many1 (intSetParser .>> spaces)
        
        let p = 
            pipe3
                (spaces >>. apParser)
                (spaces >>. prefixParser)
                (spaces >>. loopParser)
                (fun aps prefix loop -> {LassoTrace.APs = aps; Prefix = prefix; Loop = loop})
        
        skipString "[lasso]" >>. spaces >>. p
        
    let private systemStringParser =
        skipString "[system]" >>. spaces >>. many1Chars anyChar
        
        
    let private causeCheckInstanceParser (config: SolverConfiguration) = 
        pipe5
            ((infoParser .>> spaces) .>>. (inputsParser .>> spaces))
            (causeParser .>> spaces)
            (effectParser .>> spaces)
            (lassoParser .>> spaces)
            (systemStringParser .>> spaces)
            (fun (_, inputs) cause effect lasso systemString ->
               
                match
                    FsOmegaLib.Operations.AutomatonFromString.convertHoaStringToGNBA
                        false
                        config.GetMainPath
                        config.GetAutfiltPath
                        Effort.LOW
                        systemString
                with
                | FsOmegaLib.Operations.Success system ->
                    {
                        CauseCheckInstance.System = system
                        Lasso = lasso
                        Cause = cause
                        Effect = effect
                        Inputs = inputs
                    }
                    |> Result.Ok 
                | Fail msg -> Result.Error $"Failure when obtaining GNBA from string: %s{msg}"
                
                )

    let private causeSketchInstanceParser (config : SolverConfiguration) = 
        pipe5
            ((infoParser .>> spaces) .>>. (inputsParser .>> spaces))
            (causeSketcheParser .>> spaces)
            (effectParser .>> spaces)
            (lassoParser .>> spaces)
            (systemStringParser .>> spaces)
            (fun (_, inputs) causeSketch effect lasso systemString ->
                match
                    FsOmegaLib.Operations.AutomatonFromString.convertHoaStringToGNBA
                        false
                        config.GetMainPath
                        config.GetAutfiltPath
                        Effort.LOW
                        systemString
                with
                | FsOmegaLib.Operations.Success system ->
                    {
                        CauseSketchInstance.System = system
                        Lasso = lasso
                        CauseSketch = causeSketch
                        Effect = effect
                        Inputs = inputs
                    }
                    |> Result.Ok 
                | Fail msg -> Result.Error $"Failure when obtaining GNBA from string: %s{msg}"
                
                )
            

    let parseCauseCheckInstance config s =
        let full = causeCheckInstanceParser config .>> spaces .>> eof
        let res = run full s
        match res with
            | Success (res, _, _) -> res
            | Failure (err, _, _) -> Result.Error err

    let parseCauseSketchInstance config s =
        let full = causeSketchInstanceParser config .>> spaces .>> eof
        let res = run full s
        match res with
            | Success (res, _, _) -> res
            | Failure (err, _, _) -> Result.Error err
            