namespace Gaia.Core

module Engine =

    let findFRByName (sigma: Sigma) name =
        sigma.FRs
        |> List.filter (fun fr -> fr.Name = name)
        |> List.map (fun fr -> fr.Id)

    let findDPsForFR (sigma: Sigma) frIds =
        sigma.FR_to_DP
        |> List.choose (fun (fr, dp) -> if List.contains fr frIds then Some dp else None)

    let findTFsForDP (sigma: Sigma) dpIds =
        sigma.DP_to_TF
        |> List.choose (fun (dp, tf) -> if List.contains dp dpIds then Some tf else None)

    let findCTQsForTF (sigma: Sigma) tfIds =
        sigma.TF_to_CTQ
        |> List.choose (fun (tf, ctq) -> if List.contains tf tfIds then Some ctq else None)

    let selectDerivationEntry (p: PhiParse) : DerivationEntry =
        if p.ResultRejected then
            GammaOnly
        elif p.ResultIndeterminate then
            GammaOnly
        elif p.FormalNoFormalization then
            GammaOnly
        elif p.Exposure.State <> "" then
            FromState
        elif p.Exposure.Interface <> "" then
            FromInterface
        elif p.Exposure.Mode <> "" then
            FromMode
        elif p.Exposure.Function <> "" then
            FromFR
        else
            FromParametric

    let executionPath = function
        | GammaOnly ->
            [ "Γ-only"
              "Stop; no Σ update" ]
        | FromFR ->
            [ "FR"
              "Context Diagram"
              "Modes Analysis"
              "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromMode ->
            [ "Modes Analysis"
              "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromInterface ->
            [ "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromState ->
            [ "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromParametric ->
            [ "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
    
    let private addIf flag value =
        if flag then [ value ] else []
    
    let deltaSigmaSummary (p: PhiParse) =
        [
            yield! addIf p.DeltaAdd "ADD"
            yield! addIf p.DeltaRemove "REMOVE" 
            yield! addIf p.DeltaConstrain "CONSTRAIN"
            yield! addIf p.DeltaSplit "SPLIT"
            yield! addIf p.DeltaRevealMissing "REVEAL MISSING"
        ]
        |> function
            | [] -> "No ΔΣ candidate."
            | xs -> "ΔΣ candidate(s): " + String.concat ", " xs
    
    let gammaSummary (p: PhiParse) =
        [
            yield! addIf p.GammaInconsistencyFlagged "INCONSISTENCY FLAGGED"
            yield! addIf p.GammaEvidenceNeeded "EVIDENCE NEEDED"
            yield! addIf p.GammaHypothesisLogged "HYPOTHESIS LOGGED"
            yield! addIf p.ResultIndeterminate "RESULT INDETERMINATE"
            yield! addIf p.ResultRejected "RESULT REJECTED"
            yield! addIf p.OutcomeHold "HOLD"
            yield! addIf p.OutcomeEscalate "ESCALATE"
        ]
        |> function
            | [] -> "No Γ event."
            | xs -> "Γ: " + String.concat "; " xs

    let buildDeltaCandidate (p: PhiParse) =
            {
                SourcePhiId = p.PhiId

                Transitions =
                    [
                        if p.DeltaAdd then
                            AddFunction "Placeholder"

                        if p.DeltaConstrain then
                            AddConstraint "Placeholder"

                        if p.DeltaRevealMissing then
                            RevealMissing "Placeholder"

                        if p.DeltaRemove then
                            RemoveElement "Placeholder"
                    ]
            }

    let resolveParse (sigma: Sigma) (p: PhiParse) : ResolutionView =
        let selected = selectDerivationEntry p
        let path = executionPath selected
        let delta = buildDeltaCandidate p

        let matchedFRs =
            if p.Exposure.Function = "" then []
            else findFRByName sigma p.Exposure.Function

        let matchedDPs = findDPsForFR sigma matchedFRs
        let matchedTFs = findTFsForDP sigma matchedDPs
        let matchedCTQs = findCTQsForTF sigma matchedTFs

        {
            SelectedEntry = Some selected
            ExecutionPath = path
            DeltaSigmaSummary = deltaSigmaSummary p
            DeltaCandidateSummary =
                if List.isEmpty delta.Transitions then
                    "No ΔΣ candidate."
                else
                    "ΔΣ candidate(s): " +
                    String.concat ", "
                        (delta.Transitions
                        |> List.map (function
                            | AddFunction f -> "ADD FUNCTION: " + f
                            | AddMode m -> "ADD MODE: " + m
                            | AddInterface i -> "ADD INTERFACE: " + i
                            | AddState s -> "ADD STATE: " + s
                            | AddConstraint c -> "ADD CONSTRAINT: " + c
                            | RevealMissing m -> "REVEAL MISSING: " + m
                            | RemoveElement e -> "REMOVE ELEMENT: " + e))
            GammaSummary = gammaSummary p
            MatchedFRs = matchedFRs
            MatchedDPs = matchedDPs
            MatchedTFs = matchedTFs
            MatchedCTQs = matchedCTQs
        }

    let parseIntake (intake: PhiIntake) : PhiParse =
        {
            PhiId = intake.PhiId
            Date = intake.Date
            Statement = intake.RawStatement
            InScope = "TBD"
            OutOfScope = ""
            Exposure =
                {
                    Function = ""
                    Mode = ""
                    Interface = ""
                    State = ""
                    HostCandidate = ""
                }
            ExposureNotes = "Generated by T2 v0."
            DeltaAdd = false
            DeltaRemove = false
            DeltaConstrain = false
            DeltaSplit = false
            DeltaRevealMissing = false
            DeltaNotes = ""
            GammaInconsistencyFlagged = false
            GammaEvidenceNeeded = true
            GammaHypothesisLogged = true
            GammaDetails = "T2 v0 requires semantic refinement."
            Falsifiable = true
            Traceable = true
            PhaseCorrect = true
            ContextBounded = true
            ResultValid = false
            ResultIndeterminate = true
            ResultRejected = false
            FormalNoFormalization = false
            OutcomeUpdateSigma = false
            OutcomeRecordGamma = true
            OutcomeEscalate = false
            OutcomeHold = true
            DerivationEntry = Some GammaOnly
        }
    