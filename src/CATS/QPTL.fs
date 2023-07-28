module CATS.QPTL

open System

open FsOmegaLib.LTL

type QPTLAtom<'T, 'L> = 
    | AP of 'T 
    | QAP of 'L

type QPTL<'T, 'L when 'T: comparison and 'L:comparison> = 
    | Atom of QPTLAtom<'T, 'L>
    | True
    | False 
    | And of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Or of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Implies of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Equiv of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Xor of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Not of QPTL<'T, 'L>
    | X of QPTL<'T, 'L>
    | F of QPTL<'T, 'L>
    | G of QPTL<'T, 'L>
    | U of QPTL<'T, 'L> * QPTL<'T, 'L>
    | W of QPTL<'T, 'L> * QPTL<'T, 'L>
    | M of QPTL<'T, 'L> * QPTL<'T, 'L>
    | R of QPTL<'T, 'L> * QPTL<'T, 'L>
    | Exists of 'L * QPTL<'T, 'L>
    | Forall of 'L * QPTL<'T, 'L>

module QPTL =
    let rec map f g (formula : QPTL<'T, 'L>) = 
        match formula with 
        | Atom (AP x) -> Atom (AP (f x))
        | Atom (QAP x) -> Atom (QAP (g x))
        | True -> True 
        | False -> False 
        | And(e1, e2) -> And(map f g e1, map f g e2)
        | Implies(e1, e2) -> Implies(map f g e1, map f g e2)
        | Equiv(e1, e2) -> Equiv(map f g e1, map f g e2)
        | Xor(e1, e2) -> Xor(map f g e1, map f g e2)
        | Or(e1, e2) -> Or(map f g e1, map f g e2)
        | U(e1, e2) -> U(map f g e1, map f g e2)
        | W(e1, e2) -> W(map f g e1, map f g e2)
        | M(e1, e2) -> M(map f g e1, map f g e2)
        | R(e1, e2) -> R(map f g e1, map f g e2)
        | F e -> F(map f g e)
        | G e -> G(map f g e)
        | X e -> X(map f g e)
        | Not e -> Not(map f g e)
        | Exists (q, e) -> Exists(g q, map f g e)
        | Forall (q, e) -> Forall(g q, map f g e)

    let rec allAtoms (formula : QPTL<'T, 'L>) = 
        match formula with 
        | Atom x -> Set.singleton x
        | True | False -> Set.empty 
        | And(e1, e2) | Implies(e1, e2) | Equiv(e1, e2) | Xor(e1, e2) | Or(e1, e2) | U(e1, e2) | W(e1, e2) | M(e1, e2) | R(e1, e2) -> Set.union (allAtoms e1) (allAtoms e2)
        | F e | G e | X e | Not e -> allAtoms e
        | Exists(q, e) | Forall(q, e) -> 
            Set.add (QAP q) (allAtoms e)
    
    let isClosed (formula : QPTL<'T, 'L>) = 
        let rec freeVars (formula : QPTL<'T, 'L>) = 
            match formula with 
            | Atom (AP _) -> Set.empty
            | Atom (QAP q) -> Set.singleton q
            | True | False -> Set.empty 
            | And(e1, e2) | Implies(e1, e2) | Equiv(e1, e2) | Xor(e1, e2) | Or(e1, e2) | U(e1, e2) | W(e1, e2) | M(e1, e2) | R(e1, e2) -> Set.union (freeVars e1) (freeVars e2)
            | F e | G e | X e | Not e -> freeVars e
            | Exists(q, e) | Forall(q, e) -> 
                Set.remove q (freeVars e)

        freeVars formula |> Set.isEmpty

    let rec fromLTL (formula : LTL<QPTLAtom<'T,'L>>) = 
        match formula with 
        | LTL.Atom x -> Atom x
        | LTL.True -> True 
        | LTL.False -> False 
        | LTL.And(e1, e2) -> And(fromLTL e1, fromLTL e2)
        | LTL.Implies(e1, e2) -> Implies(fromLTL e1, fromLTL e2)
        | LTL.Equiv(e1, e2) -> Equiv(fromLTL e1, fromLTL e2)
        | LTL.Xor(e1, e2) -> Xor(fromLTL e1, fromLTL e2)
        | LTL.Or(e1, e2) -> Or(fromLTL e1, fromLTL e2)
        | LTL.U(e1, e2) -> U(fromLTL e1, fromLTL e2)
        | LTL.W(e1, e2) -> W(fromLTL e1, fromLTL e2)
        | LTL.M(e1, e2) -> M(fromLTL e1, fromLTL e2)
        | LTL.R(e1, e2) -> R(fromLTL e1, fromLTL e2)
        | LTL.F e -> F(fromLTL e)
        | LTL.G e -> G(fromLTL e)
        | LTL.X e -> X(fromLTL e)
        | LTL.Not e -> Not(fromLTL e)

    let rec print (atomStringer : 'T -> String) (propStringer : 'L -> String) (formula : QPTL<'T, 'L>) =
        match formula with
            | Atom (AP x) -> atomStringer x
            | Atom (QAP x) -> propStringer x
            | True -> "1"
            | False -> "0"
            | And(e1, e2) -> "(" + print atomStringer propStringer e1 + " & " + print atomStringer propStringer e2 + ")"
            | Or(e1, e2) -> "(" + print atomStringer propStringer e1 + " | " + print atomStringer propStringer e2 + ")"
            | Implies(e1, e2) -> "(" + print atomStringer propStringer e1 + " -> " + print atomStringer propStringer e2 + ")"
            | Equiv(e1, e2) -> "(" + print atomStringer propStringer e1 + " <-> " + print atomStringer propStringer e2 + ")"
            | Xor(e1, e2) -> "(" + print atomStringer propStringer e1 + " xor " + print atomStringer propStringer e2 + ")"
            | Not e -> "(! " + print atomStringer propStringer e + ")"
            | X e -> "(X " + print atomStringer propStringer e + ")"
            | F e -> "(F " + print atomStringer propStringer e + ")"
            | G e -> "(G " + print atomStringer propStringer e + ")"
            | U(e1, e2) -> "(" + print atomStringer propStringer e1 + " U " + print atomStringer propStringer e2 + ")"
            | W(e1, e2) -> "(" + print atomStringer propStringer e1 + " W " + print atomStringer propStringer e2 + ")"
            | M(e1, e2) -> "(" + print atomStringer propStringer e1 + " M " + print atomStringer propStringer e2 + ")"
            | R(e1, e2) -> "(" + print atomStringer propStringer e1 + " R " + print atomStringer propStringer e2 + ")"
            | Exists(q, e) -> "(" + "E" + propStringer q + ". " + print atomStringer propStringer e + ")"
            | Forall(q, e) -> "(" + "A" + propStringer q + ". " + print atomStringer propStringer e + ")"


type QPTLQuantifier<'L> = 
    | QPTLForall of 'L
    | QPTLExists of 'L

type PrenexQPTL<'T, 'L when 'T: comparison and 'L: comparison> =
    {
        QuantifierPrefix : list<QPTLQuantifier<'L>>
        LTLMatrix : LTL<QPTLAtom<'T, 'L>>
    }

module PrenexQPTL =

    let print (varNames : 'T -> String) (qvarNames : 'L -> String) (formula : PrenexQPTL<'T, 'L>) =
        let rec printPrefix prefix = 
            match prefix with 
            | [] -> ""
            | QPTLForall p :: xs -> "A " + qvarNames p + ". " + printPrefix xs
            | QPTLExists p :: xs -> "E " + qvarNames p + ". " + printPrefix xs

        let prefixString = printPrefix formula.QuantifierPrefix

        let bodyString = 
            formula.LTLMatrix
            |> LTL.printInSpotFormat (fun x -> 
                match x with 
                | AP a -> varNames a 
                | QAP b -> qvarNames b
                )

        prefixString + bodyString

    
    let private quantifiedPropositions (formula : PrenexQPTL<'T, 'L>) = 
        formula.QuantifierPrefix
        |> List.map (fun x -> 
            match x with 
            | QPTLForall p | QPTLExists p -> p
            )
    
    let ensureDisjointNames (gen : unit -> 'L) (formula1 : PrenexQPTL<'T, 'L>) (formula2 : PrenexQPTL<'T, 'L>) = 
        let qp1 = quantifiedPropositions formula1
        let qp2 = quantifiedPropositions formula2

        let shared = 
            Set.intersect (set qp1) (set qp2)

        let mutable freshCandidates = Set.empty

        while freshCandidates.Count < shared.Count do 
            let c = gen() 
            if Set.contains c freshCandidates |> not && List.contains c (qp1 @ qp2) |> not then 
                freshCandidates <- Set.add c freshCandidates

        let renamingMap = 
            List.zip  (Set.toList shared) (Set.toList freshCandidates)
            |> Map.ofList

        let modf2 = 
            {
                PrenexQPTL.QuantifierPrefix = 
                    formula2.QuantifierPrefix
                    |> List.map (fun x -> 
                        match x with 
                        | QPTLForall q -> 
                            if renamingMap.ContainsKey q then 
                                renamingMap.[q]
                            else 
                                q 
                            |> QPTLForall
                        | QPTLExists q -> 
                            if renamingMap.ContainsKey q then 
                                renamingMap.[q]
                            else 
                                q 
                            |> QPTLExists
                    )

                LTLMatrix = 
                    formula2.LTLMatrix   
                    |> LTL.map (fun x -> 
                        match x with 
                        | AP a -> AP a 
                        | QAP b -> QAP (if renamingMap.ContainsKey b then renamingMap.[b] else b))
            }

        formula1, modf2


module Conversion =

    let convertPrenexQPTLToQPTL (formula : PrenexQPTL<'T, 'L>) : QPTL<'T, 'L> = 
        (formula.QuantifierPrefix, QPTL.fromLTL formula.LTLMatrix)
        ||> List.foldBack (fun x s -> 
            match x with 
            | QPTLForall q -> Forall(q, s)
            | QPTLExists q -> Exists(q, s)
            )

    let private flipPrefix (prefix : list<QPTLQuantifier<'L>>) = 
        prefix
        |> List.map (fun x -> 
            match x with 
            | QPTLForall q -> QPTLExists q
            | QPTLExists q -> QPTLForall q
            )


    exception ConversionFailedException

    let rec convertQPTLToPrenexQPTL (gen : unit -> 'L) (formula : QPTL<'T, 'L>) : PrenexQPTL<'T, 'L> = 
        match formula with 
        | Atom x -> {QuantifierPrefix = []; LTLMatrix = LTL.Atom x}
        | True -> {QuantifierPrefix = []; LTLMatrix = LTL.True} 
        | False -> {QuantifierPrefix = []; LTLMatrix = LTL.False} 
        | And(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            let f1, f2 = PrenexQPTL.ensureDisjointNames gen f1 f2
            {
                QuantifierPrefix = f1.QuantifierPrefix @ f2.QuantifierPrefix
                LTLMatrix = LTL.And (f1.LTLMatrix, f2.LTLMatrix)
            }
        | Or(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            let f1, f2 = PrenexQPTL.ensureDisjointNames gen f1 f2
            {
                QuantifierPrefix = f1.QuantifierPrefix @ f2.QuantifierPrefix
                LTLMatrix = LTL.Or (f1.LTLMatrix, f2.LTLMatrix)
            }

        | Implies(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            let f1, f2 = PrenexQPTL.ensureDisjointNames gen f1 f2

            {
                QuantifierPrefix = flipPrefix (f1.QuantifierPrefix) @ f2.QuantifierPrefix
                LTLMatrix = LTL.Implies (f1.LTLMatrix, f2.LTLMatrix)
            }

        | Equiv(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            
            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.Equiv (f1.LTLMatrix, f2.LTLMatrix)
            }
        | Xor(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
             
            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.Xor (f1.LTLMatrix, f2.LTLMatrix)
            }
        | U(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 

            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.U (f1.LTLMatrix, f2.LTLMatrix)
            }

        | W(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            
            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.W (f1.LTLMatrix, f2.LTLMatrix)
            }
        | M(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            
            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.M (f1.LTLMatrix, f2.LTLMatrix)
            }
        | R(e1, e2) -> 
            let f1 = convertQPTLToPrenexQPTL gen e1
            let f2 = convertQPTLToPrenexQPTL gen e2 
            
            if f1.QuantifierPrefix.IsEmpty |> not || f2.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException

            {
                QuantifierPrefix = []
                LTLMatrix = LTL.R (f1.LTLMatrix, f2.LTLMatrix)
            }
        | F e -> 
            let f = convertQPTLToPrenexQPTL gen e
            if f.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException
            {
                QuantifierPrefix = []
                LTLMatrix = LTL.F f.LTLMatrix
            }
        | G e -> 
            let f = convertQPTLToPrenexQPTL gen e
            if f.QuantifierPrefix.IsEmpty |> not then 
                raise ConversionFailedException
            {
                QuantifierPrefix = []
                LTLMatrix = LTL.G f.LTLMatrix
            }
        | X e -> 
            let f = convertQPTLToPrenexQPTL gen e
            {
                QuantifierPrefix = f.QuantifierPrefix
                LTLMatrix = LTL.X f.LTLMatrix
            }
        | Not e -> 
            let f = convertQPTLToPrenexQPTL gen e
            {
                QuantifierPrefix = flipPrefix f.QuantifierPrefix
                LTLMatrix = LTL.Not f.LTLMatrix
            }
        | Exists (q, e) -> 
            let f = convertQPTLToPrenexQPTL gen e
            {
                QuantifierPrefix = QPTLExists q :: f.QuantifierPrefix
                LTLMatrix = f.LTLMatrix
            }
        | Forall (q, e) -> 
            let f = convertQPTLToPrenexQPTL gen e
            {
                QuantifierPrefix = QPTLForall q :: f.QuantifierPrefix
                LTLMatrix = f.LTLMatrix
            }



module Parser = 
    open FParsec

    let qapParser = 
        pipe2 
            (skipChar '_')
            (many1Chars (letter <|> digit))
            (fun _ b -> b)
    
    let qptlParser (atomParser : Parser<'T, unit>) = 
        let qptlParser, qptlParserRef = createParserForwardedToRef()

        let trueParser = 
            stringReturn "1" True
            <|>
            stringReturn "true" True

        let falseParser = 
            stringReturn "0" False 
            <|>
            stringReturn "false" False

        let quantVarParser =
            qapParser
            |>> (fun x -> QAP x |> Atom) 

        let freeVarParser = 
            atomParser
            |>> (fun x -> AP x |> Atom)

        let parParser = 
            skipChar '(' >>. spaces >>. qptlParser .>> spaces .>> skipChar ')'

        let existsParser = 
            pipe2 
                (skipChar 'E' >>. spaces1 >>. qapParser) 
                (spaces >>. skipChar '.' >>. spaces >>. qptlParser )
                (fun q e -> Exists(q, e))

        let forallParser = 
            pipe2 
                (skipChar 'E' >>. spaces1 >>. qapParser) 
                (spaces >>. skipChar '.' >>. spaces >>. qptlParser )
                (fun q e -> Forall(q, e))


        let basicParser = 
            spaces >>. choice [ 
                trueParser
                falseParser
                existsParser
                forallParser
                parParser
                quantVarParser
                freeVarParser
            ] .>> spaces

        let oppQPTL = new OperatorPrecedenceParser<QPTL<'T, String>, unit, unit>()

        let addInfixOperator string precedence associativity f =
            oppQPTL.AddOperator(
                InfixOperator(string, spaces, precedence, associativity, f)
            )

        let addPrefixOperator string precedence associativity f =
            oppQPTL.AddOperator(
                PrefixOperator(string, spaces, precedence, associativity, f)
            )

        do
            oppQPTL.TermParser <- basicParser

            addInfixOperator "&" 5 Associativity.Left (fun x y -> And(x, y))
            addInfixOperator "|" 4 Associativity.Left (fun x y -> Or(x, y))
            addInfixOperator "->" 3 Associativity.Left (fun x y -> Implies(x, y))
            addInfixOperator "<->" 2 Associativity.None (fun x y -> Equiv(x, y))
            addInfixOperator "xor" 2 Associativity.None (fun x y -> Xor(x, y))
            addInfixOperator "U" 10 Associativity.Left (fun x y -> U(x, y))
            addInfixOperator "W" 10 Associativity.Left (fun x y -> W(x, y))
            addInfixOperator "M" 10 Associativity.Left (fun x y -> M(x, y))
            addInfixOperator "R" 10 Associativity.Left (fun x y -> R(x, y))

            addPrefixOperator "F" 20 true (fun x -> F x)
            addPrefixOperator "G" 20 false (fun x -> G x)
            addPrefixOperator "X" 30 true (fun x -> X x)
            addPrefixOperator "!" 40 true (fun x -> Not x)

        do 
            qptlParserRef.Value <- oppQPTL.ExpressionParser

        qptlParser

    let parseQPTL (atomParser : Parser<'T, unit>) s =    
        let full = qptlParser atomParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err


