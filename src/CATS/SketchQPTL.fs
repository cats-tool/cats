module CATS.SketchQPTL

open System

open QPTL

type SketchQPTL<'T, 'L, 'U when 'T: comparison and 'L:comparison and 'U:comparison> = 
    | Atom of QPTLAtom<'T, 'L>
    | Hole of 'U 
    | True
    | False 
    | And of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Or of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Implies of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Equiv of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Xor of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Not of SketchQPTL<'T, 'L, 'U>
    | X of SketchQPTL<'T, 'L, 'U>
    | F of SketchQPTL<'T, 'L, 'U>
    | G of SketchQPTL<'T, 'L, 'U>
    | U of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | W of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | M of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | R of SketchQPTL<'T, 'L, 'U> * SketchQPTL<'T, 'L, 'U>
    | Exists of 'L * SketchQPTL<'T, 'L, 'U>
    | Forall of 'L * SketchQPTL<'T, 'L, 'U>

module SketchQPTL =

    let rec allAtoms (formula : SketchQPTL<'T, 'L, 'U>) = 
        match formula with 
        | Atom x -> Set.singleton x
        | True | False | Hole _ -> Set.empty 
        | And(e1, e2) | Implies(e1, e2) | Equiv(e1, e2) | Xor(e1, e2) | Or(e1, e2) | U(e1, e2) | W(e1, e2) | M(e1, e2) | R(e1, e2) -> Set.union (allAtoms e1) (allAtoms e2)
        | F e | G e | X e | Not e -> allAtoms e
        | Exists(q, e) | Forall(q, e) -> 
            Set.add (QAP q) (allAtoms e)

    let rec allHoles (formula : SketchQPTL<'T, 'L, 'U>) = 
        match formula with 
        | Hole x -> Set.singleton x
        | True | False | Atom _ -> Set.empty 
        | And(e1, e2) | Implies(e1, e2) | Equiv(e1, e2) | Xor(e1, e2) | Or(e1, e2) | U(e1, e2) | W(e1, e2) | M(e1, e2) | R(e1, e2) -> Set.union (allHoles e1) (allHoles e2)
        | F e | G e | X e | Not e -> allHoles e
        | Exists(_, e) | Forall(_, e) -> allHoles e

    let isClosed (formula : SketchQPTL<'T, 'L, 'U>) = 
        let rec freeVars (formula : SketchQPTL<'T, 'L, 'U>) = 
            match formula with 
            | Atom (AP _) -> Set.empty
            | Atom (QAP q) -> Set.singleton q
            | True | False | Hole _ -> Set.empty 
            | And(e1, e2) | Implies(e1, e2) | Equiv(e1, e2) | Xor(e1, e2) | Or(e1, e2) | U(e1, e2) | W(e1, e2) | M(e1, e2) | R(e1, e2) -> Set.union (freeVars e1) (freeVars e2)
            | F e | G e | X e | Not e -> freeVars e
            | Exists(q, e) | Forall(q, e) -> 
                Set.remove q (freeVars e)

        freeVars formula |> Set.isEmpty

    let rec print (atomStringer : 'T -> String) (propStringer : 'L -> String) (holeStringer : 'U -> String) (formula : SketchQPTL<'T, 'L, 'U>) =
        match formula with
        | Atom (AP x) -> atomStringer x
        | Atom (QAP x) -> propStringer x
        | Hole x -> "?{" + holeStringer x + "}"
        | True -> "1"
        | False -> "0"
        | And(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " & " + print atomStringer propStringer holeStringer e2 + ")"
        | Or(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " | " + print atomStringer propStringer holeStringer e2 + ")"
        | Implies(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " -> " + print atomStringer propStringer holeStringer e2 + ")"
        | Equiv(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " <-> " + print atomStringer propStringer holeStringer e2 + ")"
        | Xor(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " xor " + print atomStringer propStringer holeStringer e2 + ")"
        | Not e -> "(! " + print atomStringer propStringer holeStringer e + ")"
        | X e -> "(X " + print atomStringer propStringer holeStringer e + ")"
        | F e -> "(F " + print atomStringer propStringer holeStringer e + ")"
        | G e -> "(G " + print atomStringer propStringer holeStringer e + ")"
        | U(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " U " + print atomStringer propStringer holeStringer e2 + ")"
        | W(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " W " + print atomStringer propStringer holeStringer e2 + ")"
        | M(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " M " + print atomStringer propStringer holeStringer e2 + ")"
        | R(e1, e2) -> "(" + print atomStringer propStringer holeStringer e1 + " R " + print atomStringer propStringer holeStringer e2 + ")"
        | Exists(q, e) -> "(" + "E" + propStringer q + ". " + print atomStringer propStringer holeStringer e + ")"
        | Forall(q, e) -> "(" + "A" + propStringer q + ". " + print atomStringer propStringer holeStringer e + ")"


    let rec generateConcretInstances (holeCandidates : 'U -> seq<QPTL<'T, 'L>>) (sketch : SketchQPTL<'T, 'L, 'U>) = 
        match sketch with 
        | Hole i -> holeCandidates i
        | Atom x -> QPTL.Atom x |> Seq.singleton
        | True -> QPTL.True |> Seq.singleton
        | False -> QPTL.False |> Seq.singleton
        | And(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.And(e1, e2))
            }
        | Implies(e1, e2) ->
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.Implies(e1, e2))
            }
        | Equiv(e1, e2) ->
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.Equiv(e1, e2))
            }
        | Xor(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.Xor(e1, e2))
            }
        | Or(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.Or(e1, e2))
            }
        | U(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.U(e1, e2))
            }
        | W(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.W(e1, e2))
            }
        | M(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.M(e1, e2))
            }
        | R(e1, e2) -> 
            seq {
                for e1 in generateConcretInstances holeCandidates e1 do 
                    for e2 in generateConcretInstances holeCandidates e2 do 
                        yield (QPTL.R(e1, e2))
            }
        | F e -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.F e)
            }
        | G e -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.G e)
            }
        | X e -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.X e)
            }
        | Not e -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.Not e)
            }
        | Exists (q, e) -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.Exists (q, e))
            }
        | Forall (q, e) -> 
            seq {
                for e in generateConcretInstances holeCandidates e do 
                        yield (QPTL.Forall (q, e))
            }
    
module Parser = 
    open FParsec

    let qapParser = 
        pipe2 
            (skipChar '_')
            (many1Chars (letter <|> digit))
            (fun _ b -> b)

    let apListSketchParser (atomParser : Parser<'T, unit>) = 
        between (skipChar '{' .>> spaces) (skipChar '}') (sepBy1 (atomParser .>> spaces) (skipChar ',' .>> spaces))
    
    let sketchQptlParser (atomParser : Parser<'T, unit>) (holeParser : Parser<'U, unit>) = 
        let sketchQptlParser, sketchQptlParserRef = createParserForwardedToRef()

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
            skipChar '(' >>. spaces >>. sketchQptlParser .>> spaces .>> skipChar ')'

        let existsParser = 
            pipe2 
                (skipChar 'E' >>. spaces1 >>. qapParser) 
                (spaces >>. skipChar '.' >>. spaces >>. sketchQptlParser )
                (fun q e -> Exists(q, e))

        let forallParser = 
            pipe2 
                (skipChar 'E' >>. spaces1 >>. qapParser) 
                (spaces >>. skipChar '.' >>. spaces >>. sketchQptlParser )
                (fun q e -> Forall(q, e))

        let holeParser = 
            (skipChar '?' >>. spaces >>. holeParser)
            |>> Hole 
            

        let basicParser = 
            spaces >>. choice [ 
                trueParser
                falseParser
                holeParser
                existsParser
                forallParser
                parParser
                quantVarParser
                freeVarParser
            ] .>> spaces

        let oppQPTL = new OperatorPrecedenceParser<SketchQPTL<'T, String, 'U>, unit, unit>()

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
            sketchQptlParserRef.Value <- oppQPTL.ExpressionParser

        sketchQptlParser

    let parseSketchQPTL (atomParser : Parser<'T, unit>) (holeParser : Parser<'U, unit>) s =    
        let full = sketchQptlParser atomParser holeParser .>> spaces .>> eof
        let res = run full s
        match res with
        | Success (res, _, _) -> Result.Ok res
        | Failure (err, _, _) -> Result.Error err


