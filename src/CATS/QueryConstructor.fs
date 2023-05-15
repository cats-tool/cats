module CATS.QueryConstructor

open System

open FsOmegaLib.LTL

open AutoHyperQCore.HyperQPTL

open QPTL

let indexVariable index variable = TraceAtom(variable, index)

// Constructs the LTL condition under which 'trace1' and 'trace2' are globally equivalent on the inputs
let inputEquality (trace1: TraceVariable) (trace2: TraceVariable) (inputs: List<'L>) =

    inputs
    |> List.map (fun input ->
        // Constructs (a_t <-> a_t'), i.e., trace 1 equals trace 2 in input a
        let i1 = AP(input, trace1)

        let i2 = AP(input, trace2)

        Equiv(Atom(i1), Atom(i2)))
    // Builds conjunction out of all the equivalences
    |> fun x ->
        if List.isEmpty x then
            True
        else
            List.reduce (fun x y -> And(x, y)) x
    |> G


// Constructs the LTL condition under which 'closerTrace' is strictly closer to 'referenceTrace' than 'fartherTrace'
let traceMinimality
    (referenceTrace: TraceVariable)
    (closerTrace: TraceVariable)
    (fartherTrace: TraceVariable)
    (inputs: List<'L>)
    =
    let neq input trace1 trace2 =
        Not(Equiv(Atom(AP(input, trace1)), Atom(AP(input, trace2)))) // Constructs !(a_t <-> a_t'), i.e., trace 1 differs from trace 2 in input

    let subsetOfChanges =
        inputs
        |> List.map (fun input -> Implies(neq input closerTrace referenceTrace, neq input fartherTrace referenceTrace) // Change between t and t' implies change in t and t''
        )
        // Builds conjunction out of all the implications
        |> fun x ->
            if List.isEmpty x then
                True
            else
                List.reduce (fun x y -> And(x, y)) x
        |> G

    let existsChange =
        inputs
        |> List.map (fun input ->
            // There has to be a position where the two traces are different for the strict relation to hold
            neq input closerTrace fartherTrace)
        |> fun x ->
            if List.isEmpty x then
                False
            else
                List.reduce (fun x y -> Or(x, y)) x
        |> F

    And(subsetOfChanges, existsChange)


type CausalityQuerie<'T when 'T: comparison> = 
    {
        PC1 : HyperQPTL<'T>
        PC2 : HyperQPTL<'T>
        PC31 : HyperQPTL<'T>
        PC32 : HyperQPTL<'T>
        PC4 : HyperQPTL<'T>
    }


// Constructs the full causality check query for the causal relationship between causeFormula and effectFormula on the referenceTrace
let constructQuery
    (referenceTrace)
    (causeFormula: QPTL<'T, String>)
    (effectFormula: QPTL<'T, String>)
    (inputs: List<'T>)
    =
    let sigma: TraceVariable = "A"
    let sigmap: TraceVariable = "B"
    let sigmapp: TraceVariable = "C"
    let pip: TraceVariable = "D"
    let pipp: TraceVariable = "E"

    let pc1Formula =
        And(causeFormula, effectFormula) |> QPTL.map (fun a -> (a, referenceTrace)) id

    let antecedentFormula =
        And(
            Not(QPTL.map (fun a -> (a, sigma)) id causeFormula),
            Or(
                QPTL.map (fun a -> (a, sigmap)) id causeFormula,
                Not(traceMinimality referenceTrace sigmap sigma inputs)
            )
        )

    let consequentpc2 =
        And(Not(QPTL.map (fun a -> (a, pipp)) id effectFormula), inputEquality pipp sigma inputs)

    let consequentpc3 =
        Implies(
            QPTL.map (fun a -> (a, sigmapp)) id causeFormula,
            Not(traceMinimality referenceTrace sigma sigmapp inputs)
        )

    let smallImplication =
        Implies(QPTL.map (fun a -> (a, pip)) id causeFormula, QPTL.map (fun a -> (a, pip)) id effectFormula)

    let toPrenexHyper (formula: QPTL<('T * TraceVariable),String>) (prefix: HyperQPTLQuantifier list) =
        let mutable num = 0

        let gen _ =
            num <- num + 1
            "q" + string (num)

        let prenex = QPTL.Conversion.convertQPTLToPrenexQPTL gen formula

        let mappedPrefix =
            prenex.QuantifierPrefix
            |> List.map (fun x ->
                match x with
                | QPTLForall q -> ForallProp q
                | QPTLExists q -> ExistsProp q)

        let mappedBody =
            prenex.LTLMatrix
            |> LTL.map (fun x ->
                match x with
                | AP(a, pi) -> TraceAtom(a, pi)
                | QAP b -> PropAtom b)

        { HyperQPTL.QuantifierPrefix = prefix @ mappedPrefix
          LTLMatrix = mappedBody }

    {
        PC1 = toPrenexHyper pc1Formula [ ForallTrace(referenceTrace) ]
        PC2 = toPrenexHyper (Implies(antecedentFormula, consequentpc2))
            [ ForallTrace(referenceTrace); ForallTrace(sigma); ExistsTrace(sigmap); ExistsTrace(pipp)]
        PC31 = toPrenexHyper (Implies(antecedentFormula, consequentpc3))
            [ ForallTrace(referenceTrace); ForallTrace(sigma); ForallTrace(sigmapp); ExistsTrace(sigmap) ]
        PC32 = toPrenexHyper smallImplication [ ForallTrace(referenceTrace); ForallTrace(pip)]
        PC4 = toPrenexHyper antecedentFormula [ ExistsTrace(sigma); ForallTrace(sigmap); ForallTrace(referenceTrace)]
    }
