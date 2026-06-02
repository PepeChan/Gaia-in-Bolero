namespace Gaia.Core

open System

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

    let private requirementMarkers =
        [ "shall"; "must"; "needs to"; "should" ]

    let private conditionMarkers =
        [ "when"; "while"; "during"; "under"; "without"; "in case of"; "if" ]

    let private modeConditionMarkers =
        [ "while"; "during" ]

    let private interfaceMarkers =
        [ "with"; "through"; "via"; "using"; "between" ]

    let private riskMarkers =
        [ "missing"; "unavailable"; "unable"; "fails"; "cannot"; "not" ]

    let private phraseTrimChars =
        [| ' '; '\t'; '\r'; '\n'; '.'; ','; ';'; ':'; '-' |]

    let private sentenceStopChars =
        [| '.'; ';'; '\r'; '\n' |]

    let private phraseStopChars =
        [| '.'; ','; ';'; '\r'; '\n' |]

    let private joinIntakeText (intake: PhiIntake) =
        [
            intake.RawStatement
            intake.Context
            intake.Trigger
            intake.TypeText
        ]
        |> List.map (fun value -> if isNull value then "" else value.Trim())
        |> List.filter (fun value -> value <> "")
        |> String.concat ". "

    let private hasMarkerBoundary (text: string) index length =
        let before =
            index = 0 || not (Char.IsLetterOrDigit text.[index - 1])

        let afterIndex = index + length

        let after =
            afterIndex >= text.Length || not (Char.IsLetterOrDigit text.[afterIndex])

        before && after

    let private tryFindMarker (markers: string list) (text: string) =
        let rec findFrom marker start =
            let index = text.IndexOf(marker, start, StringComparison.OrdinalIgnoreCase)

            if index < 0 then
                None
            elif hasMarkerBoundary text index marker.Length then
                Some (index, marker)
            else
                findFrom marker (index + 1)

        markers
        |> List.choose (fun marker -> findFrom marker 0)
        |> List.sortBy fst
        |> List.tryHead

    let private tryFindMarkerIndex markers text =
        tryFindMarker markers text
        |> Option.map fst

    let private tryFindCharIndex (chars: char array) (text: string) =
        chars
        |> Array.choose (fun value ->
            let index = text.IndexOf(value)

            if index < 0 then
                None
            else
                Some index)
        |> Array.sort
        |> Array.tryHead

    let private trimPhrase (value: string) =
        value.Trim(phraseTrimChars)

    let private takeBefore markers stopChars text =
        [
            tryFindMarkerIndex markers text
            tryFindCharIndex stopChars text
        ]
        |> List.choose id
        |> List.sort
        |> List.tryHead
        |> function
            | Some stopIndex -> text.Substring(0, stopIndex)
            | None -> text
        |> trimPhrase

    let private tryExtractAfter markers stopMarkers stopChars text =
        tryFindMarker markers text
        |> Option.bind (fun (index, marker) ->
            let phrase =
                text.Substring(index + marker.Length)
                |> takeBefore stopMarkers stopChars

            if phrase = "" then
                None
            else
                Some (marker, phrase))

    let private hasAnyMarker markers text =
        tryFindMarker markers text
        |> Option.isSome

    let parseIntake (intake: PhiIntake) : PhiParse =
        let combinedText = joinIntakeText intake

        let functionCandidate =
            combinedText
            |> tryExtractAfter requirementMarkers conditionMarkers sentenceStopChars
            |> Option.map snd
            |> Option.defaultValue ""

        let conditionCandidate =
            combinedText
            |> tryExtractAfter conditionMarkers (requirementMarkers @ interfaceMarkers) phraseStopChars

        let interfaceCandidate =
            combinedText
            |> tryExtractAfter interfaceMarkers (requirementMarkers @ conditionMarkers) phraseStopChars
            |> Option.map snd
            |> Option.defaultValue ""

        let conditionMarker, conditionPhrase =
            conditionCandidate
            |> Option.defaultValue ("", "")

        let hasFunctionCandidate = functionCandidate <> ""
        let hasConditionCandidate = conditionPhrase <> ""
        let hasRequirementMarker = hasAnyMarker requirementMarkers combinedText
        let hasRiskMarker = hasAnyMarker riskMarkers combinedText
        let isModeCondition = List.contains conditionMarker modeConditionMarkers
        let isValid = hasRequirementMarker && hasFunctionCandidate && hasConditionCandidate
        let isIndeterminate = not isValid
        let shouldRevealMissing = hasRiskMarker && not hasFunctionCandidate

        let derivationEntry =
            if isIndeterminate then
                Some GammaOnly
            elif isModeCondition then
                Some FromMode
            elif hasConditionCandidate then
                Some FromState
            else
                Some GammaOnly

        {
            PhiId = intake.PhiId
            Date = intake.Date
            Statement = intake.RawStatement
            InScope = "Assumed in scope for T2 v0."
            OutOfScope = ""
            Exposure =
                {
                    Function = functionCandidate
                    Mode = if isModeCondition then conditionPhrase else ""
                    Interface = interfaceCandidate
                    State = conditionPhrase
                    HostCandidate = ""
                }
            ExposureNotes = "Generated by deterministic T2 v0 parser."
            DeltaAdd = false
            DeltaRemove = false
            DeltaConstrain = hasConditionCandidate
            DeltaSplit = false
            DeltaRevealMissing = shouldRevealMissing
            DeltaNotes = ""
            GammaInconsistencyFlagged = false
            GammaEvidenceNeeded = isIndeterminate || shouldRevealMissing
            GammaHypothesisLogged = isIndeterminate
            GammaDetails =
                if isIndeterminate then
                    "T2 v0 could not infer enough structure."
                else
                    ""
            Falsifiable = true
            Traceable = true
            PhaseCorrect = true
            ContextBounded = true
            ResultValid = isValid
            ResultIndeterminate = isIndeterminate
            ResultRejected = false
            FormalNoFormalization = false
            OutcomeUpdateSigma = isValid
            OutcomeRecordGamma = isIndeterminate
            OutcomeEscalate = false
            OutcomeHold = isIndeterminate
            DerivationEntry = derivationEntry
        }
