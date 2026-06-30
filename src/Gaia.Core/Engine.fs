namespace Gaia.Core

open System
open System.Text.RegularExpressions

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

    let private exposureAtomSeparator = "; "

    let private generatedConstraintAtomMarker = "GeneratedConstraintAtoms="

    let private constraintPhraseMarkers =
        [
            "physical contact"
            "contact"
            "contacts"
            "minimum"
            "maximum"
            "maintain"
            "thickness"
            "distance"
            "aperture"
            "limit"
            "limits"
            "non-slip"
            "non-slippery"
            "tactile"
            "low-fatigue"
            "thin large-format"
        ]

    let private functionConstraintQualifiers =
        [
            "tactile"
            "non-slip"
            "non-slippery"
            "low-fatigue"
            "high-control"
            "comfortable"
            "calm"
            "non-aggressive"
            "full-page"
        ]

    let private functionPhraseConstraintMarkers =
        [
            "minimum"
            "maximum"
            "maintain"
            "thickness"
            "distance"
            "aperture"
            "limit"
            "limits"
            "thin large-format"
        ]

    let private requirementOnlyConstraintMarkers =
        [
            "avoid"
            "avoiding"
            "minimize"
            "minimizing"
            "maintaining"
            "risk"
        ]

    let private requirementModeBlockers =
        [
            yield! constraintPhraseMarkers
            yield! requirementOnlyConstraintMarkers
        ]
        |> List.distinct

    let private functionModeQualifiers =
        [
            "handheld", "handheld use"
        ]

    let private vagueInterfaceMarkers =
        [
            "reading"
            "writing"
            "file work"
            "work context"
            "work contexts"
            "workflow"
            "workflows"
            "research"
        ]

    let private operatingRegimeMarkers =
        [
            "mode"
            "operation"
            "operating"
            "use"
            "sessions"
            "workflow"
            "workflows"
            "reading"
            "writing"
            "file work"
            "work sessions"
            "extended use"
        ]

    let private stateConditionMarkers =
        [
            "available"
            "unavailable"
            "plugged"
            "connected"
            "active"
            "inactive"
            "open"
            "closed"
            "enabled"
            "disabled"
        ]

    let private modeLeadIns =
        [
            "the user moves"
            "user moves"
            "moves"
            "operating in"
            "operates in"
            "operating"
            "working in"
            "using"
            "in"
        ]

    type private IntakeParserClass =
        | RequirementClass
        | ObservationClass
        | InquiryClass
        | DefaultClass

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

    let private normalizeSpaces (value: string) =
        if isNull value then
            ""
        else
            value.Split([| ' '; '\t'; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> String.concat " "

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

    let private cleanPhrase value =
        value
        |> normalizeSpaces
        |> trimPhrase

    let private replacePhraseIgnoreCase phrase (value: string) =
        if String.IsNullOrWhiteSpace(phrase) || String.IsNullOrWhiteSpace(value) then
            value
        else
            let pattern = "(^|\\s)" + Regex.Escape(phrase) + "(?=\\s|$)"
            Regex.Replace(value, pattern, " ", RegexOptions.IgnoreCase)

    let private removePhrases phrases value =
        phrases
        |> List.fold (fun working phrase -> replacePhraseIgnoreCase phrase working) value
        |> cleanPhrase

    let private containsText (marker: string) (value: string) =
        not (String.IsNullOrWhiteSpace(value))
        && value.IndexOf(marker, StringComparison.OrdinalIgnoreCase) >= 0

    let private containsAnyText (markers: string list) value =
        markers
        |> List.exists (fun marker -> containsText marker value)

    let private getOptionalText (value: string option) =
        value
        |> Option.defaultValue ""

    let private getIntakeClassText (intake: PhiIntake) =
        [
            getOptionalText intake.InputClass
            intake.TypeText
            intake.Source
        ]
        |> List.map cleanPhrase
        |> List.filter (fun value -> value <> "")
        |> String.concat " "

    let private classifyIntake (intake: PhiIntake) =
        let classText = getIntakeClassText intake

        if containsAnyText [ "forward inquiry"; "derived inquiry"; "t6 realization inquiry"; "derived-inquiry" ] classText then
            InquiryClass
        elif containsText "requirement" classText then
            RequirementClass
        elif containsText "observation" classText then
            ObservationClass
        else
            DefaultClass

    let private distinctClean values =
        values
        |> List.map cleanPhrase
        |> List.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
        |> List.fold
            (fun collected value ->
                if collected |> List.exists (fun existing -> String.Equals(existing, value, StringComparison.OrdinalIgnoreCase)) then
                    collected
                else
                    collected @ [ value ])
            []

    let private joinAtoms values =
        values
        |> distinctClean
        |> String.concat exposureAtomSeparator

    let private splitEnumerationAtoms value =
        let commaSeparated =
            Regex.Replace(value, "\\s+and\\s+", ",", RegexOptions.IgnoreCase)

        commaSeparated.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> distinctClean

    let private tryTextAfterMarker marker text =
        tryFindMarker [ marker ] text
        |> Option.map (fun (index, foundMarker) -> text.Substring(index + foundMarker.Length))

    let private extractBetweenAtoms phrase =
        phrase
        |> tryTextAfterMarker "between"
        |> Option.map splitEnumerationAtoms
        |> Option.defaultValue []

    let private extractFromToAtoms phrase =
        phrase
        |> tryTextAfterMarker "from"
        |> Option.bind (fun afterFrom ->
            tryFindMarker [ "to" ] afterFrom
            |> Option.map (fun (toIndex, toMarker) ->
                [
                    afterFrom.Substring(0, toIndex)
                    afterFrom.Substring(toIndex + toMarker.Length)
                ]
                |> List.collect splitEnumerationAtoms
                |> distinctClean))
        |> Option.defaultValue []

    let private extractTransitionOrEnumerationAtoms phrase =
        let betweenAtoms = extractBetweenAtoms phrase
        let fromToAtoms = extractFromToAtoms phrase

        if not (List.isEmpty betweenAtoms) then
            betweenAtoms
        elif not (List.isEmpty fromToAtoms) then
            fromToAtoms
        elif phrase.Contains(",") then
            splitEnumerationAtoms phrase
        else
            []

    let private stripModeLeadIn value =
        let trimmed = cleanPhrase value
        let lower = trimmed.ToLowerInvariant()

        modeLeadIns
        |> List.tryFind (fun leadIn -> lower = leadIn || lower.StartsWith(leadIn + " "))
        |> function
            | Some leadIn -> trimmed.Substring(leadIn.Length) |> cleanPhrase
            | None -> trimmed

    let private isConstraintLikePhrase phrase =
        containsAnyText constraintPhraseMarkers phrase

    let private isOperatingRegime phrase =
        containsAnyText operatingRegimeMarkers phrase

    let private isMomentaryState phrase =
        containsAnyText stateConditionMarkers phrase

    let private getFunctionQualifierConstraints functionPhrase =
        functionConstraintQualifiers
        |> List.filter (fun qualifier -> containsText qualifier functionPhrase)
        |> distinctClean

    let private getFunctionModeAtoms functionPhrase =
        functionModeQualifiers
        |> List.choose (fun (qualifier, modeAtom) ->
            if containsText qualifier functionPhrase then
                Some modeAtom
            else
                None)
        |> distinctClean

    let private getFunctionCore functionPhrase =
        let modeQualifiers =
            functionModeQualifiers
            |> List.map fst

        functionPhrase
        |> removePhrases functionConstraintQualifiers
        |> removePhrases modeQualifiers

    let private getConditionConstraintAtoms parserClass conditionPhrase =
        let constraintMarkers =
            match parserClass with
            | RequirementClass -> requirementModeBlockers
            | _ -> constraintPhraseMarkers

        if containsAnyText constraintMarkers conditionPhrase then
            [ conditionPhrase ] |> distinctClean
        else
            []

    let private getPhraseConstraintAtoms parserClass functionPhrase conditionPhrase =
        let functionMarkers =
            match parserClass with
            | RequirementClass ->
                [
                    yield! functionPhraseConstraintMarkers
                    yield! requirementOnlyConstraintMarkers
                ]
                |> List.distinct
            | _ -> functionPhraseConstraintMarkers

        [
            yield! getFunctionQualifierConstraints functionPhrase

            if containsAnyText functionMarkers functionPhrase then
                yield functionPhrase

            yield! getConditionConstraintAtoms parserClass conditionPhrase
        ]
        |> distinctClean

    let private getDefaultModeAtoms conditionMarker conditionPhrase functionPhrase =
        let fromFunction = getFunctionModeAtoms functionPhrase

        let fromCondition =
            if String.IsNullOrWhiteSpace(conditionPhrase) || isConstraintLikePhrase conditionPhrase then
                []
            else
                let transitionAtoms = extractTransitionOrEnumerationAtoms conditionPhrase

                if not (List.isEmpty transitionAtoms) then
                    transitionAtoms
                elif List.contains conditionMarker modeConditionMarkers || isOperatingRegime conditionPhrase then
                    [ stripModeLeadIn conditionPhrase ]
                else
                    []

        [ yield! fromFunction; yield! fromCondition ]
        |> distinctClean

    let private getRequirementModeAtoms conditionMarker conditionPhrase functionPhrase =
        let fromFunction = getFunctionModeAtoms functionPhrase

        let fromCondition =
            if String.IsNullOrWhiteSpace(conditionPhrase) || containsAnyText requirementModeBlockers conditionPhrase then
                []
            else
                let transitionAtoms = extractTransitionOrEnumerationAtoms conditionPhrase

                if not (List.isEmpty transitionAtoms) then
                    transitionAtoms
                elif containsAnyText [ "mode"; "operation"; "operating" ] conditionPhrase then
                    [ stripModeLeadIn conditionPhrase ]
                else
                    []

        [ yield! fromFunction; yield! fromCondition ]
        |> distinctClean

    let private getObservationModeAtoms conditionMarker conditionPhrase functionPhrase =
        let fromFunction = getFunctionModeAtoms functionPhrase

        let fromCondition =
            if String.IsNullOrWhiteSpace(conditionPhrase) || isConstraintLikePhrase conditionPhrase then
                []
            else
                let transitionAtoms = extractTransitionOrEnumerationAtoms conditionPhrase

                if not (List.isEmpty transitionAtoms) then
                    transitionAtoms
                elif isMomentaryState conditionPhrase && not (isOperatingRegime conditionPhrase) then
                    []
                elif List.contains conditionMarker modeConditionMarkers || isOperatingRegime conditionPhrase then
                    [ stripModeLeadIn conditionPhrase ]
                else
                    []

        [ yield! fromFunction; yield! fromCondition ]
        |> distinctClean

    let private getModeAtoms parserClass conditionMarker conditionPhrase functionPhrase =
        match parserClass with
        | RequirementClass -> getRequirementModeAtoms conditionMarker conditionPhrase functionPhrase
        | ObservationClass -> getObservationModeAtoms conditionMarker conditionPhrase functionPhrase
        | InquiryClass
        | DefaultClass -> getDefaultModeAtoms conditionMarker conditionPhrase functionPhrase

    let private getDefaultStateAtoms conditionMarker conditionPhrase =
        if String.IsNullOrWhiteSpace(conditionPhrase)
           || isConstraintLikePhrase conditionPhrase
           || List.contains conditionMarker modeConditionMarkers
           || (isOperatingRegime conditionPhrase && not (isMomentaryState conditionPhrase)) then
            []
        else
            let transitionAtoms = extractTransitionOrEnumerationAtoms conditionPhrase

            let stateAtoms =
                if not (List.isEmpty transitionAtoms) then
                    transitionAtoms
                else
                    [ conditionPhrase ]

            stateAtoms
            |> distinctClean

    let private getRequirementStateAtoms conditionPhrase =
        if String.IsNullOrWhiteSpace(conditionPhrase)
           || containsAnyText requirementModeBlockers conditionPhrase
           || isOperatingRegime conditionPhrase then
            []
        elif isMomentaryState conditionPhrase then
            [ conditionPhrase ] |> distinctClean
        else
            []

    let private getObservationStateAtoms conditionPhrase =
        if String.IsNullOrWhiteSpace(conditionPhrase) || isConstraintLikePhrase conditionPhrase then
            []
        else
            let transitionAtoms = extractTransitionOrEnumerationAtoms conditionPhrase

            if not (List.isEmpty transitionAtoms) && not (isOperatingRegime conditionPhrase) then
                transitionAtoms
            elif isMomentaryState conditionPhrase || not (isOperatingRegime conditionPhrase) then
                [ conditionPhrase ]
            else
                []
            |> distinctClean

    let private getStateAtoms parserClass conditionMarker conditionPhrase =
        match parserClass with
        | RequirementClass -> getRequirementStateAtoms conditionPhrase
        | ObservationClass -> getObservationStateAtoms conditionPhrase
        | InquiryClass
        | DefaultClass -> getDefaultStateAtoms conditionMarker conditionPhrase

    let private cleanInterfacePhrase phrase =
        phrase
        |> cleanPhrase
        |> fun value ->
            if value.StartsWith("the ", StringComparison.OrdinalIgnoreCase) then
                value.Substring(4)
            else
                value
        |> cleanPhrase

    let private hasTwoEntityInterfaceSignal phrase =
        [
            " and "
            " on "
            " with "
            " to "
            " into "
            " onto "
            "-to-"
            "->"
        ]
        |> List.exists (fun marker -> containsText marker phrase)

    let private isVagueInterfaceOnly phrase =
        containsAnyText vagueInterfaceMarkers phrase
        && not (hasTwoEntityInterfaceSignal phrase)

    let private getInterfaceCandidate interfacePhrase =
        let cleaned = cleanInterfacePhrase interfacePhrase

        if String.IsNullOrWhiteSpace(cleaned) || isVagueInterfaceOnly cleaned || not (hasTwoEntityInterfaceSignal cleaned) then
            ""
        else
            cleaned

    let private formatGeneratedConstraintNotes constraints =
        match constraints with
        | [] -> ""
        | values -> generatedConstraintAtomMarker + String.concat " || " values + ". "

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
        let parserClass = classifyIntake intake

        let rawFunctionCandidate =
            combinedText
            |> tryExtractAfter requirementMarkers conditionMarkers sentenceStopChars
            |> Option.map snd
            |> Option.defaultValue ""

        let conditionCandidate =
            combinedText
            |> tryExtractAfter conditionMarkers requirementMarkers sentenceStopChars

        let rawInterfaceCandidate =
            combinedText
            |> tryExtractAfter interfaceMarkers (requirementMarkers @ conditionMarkers) phraseStopChars
            |> Option.map snd
            |> Option.defaultValue ""

        let conditionMarker, conditionPhrase =
            conditionCandidate
            |> Option.defaultValue ("", "")

        let functionCandidate = getFunctionCore rawFunctionCandidate
        let generatedConstraintAtoms = getPhraseConstraintAtoms parserClass rawFunctionCandidate conditionPhrase
        let modeAtoms = getModeAtoms parserClass conditionMarker conditionPhrase rawFunctionCandidate
        let stateAtoms = getStateAtoms parserClass conditionMarker conditionPhrase
        let interfaceCandidate = getInterfaceCandidate rawInterfaceCandidate
        let hasFunctionCandidate = functionCandidate <> ""
        let hasConditionCandidate = conditionPhrase <> ""
        let hasRequirementMarker = hasAnyMarker requirementMarkers combinedText
        let hasRiskMarker = hasAnyMarker riskMarkers combinedText
        let hasModeCandidate = not (List.isEmpty modeAtoms)
        let hasStateCandidate = not (List.isEmpty stateAtoms)
        let isValid = hasRequirementMarker && hasFunctionCandidate && hasConditionCandidate
        let isIndeterminate = not isValid
        let shouldRevealMissing = hasRiskMarker && not hasFunctionCandidate

        let derivationEntry =
            if isIndeterminate then
                Some GammaOnly
            elif hasStateCandidate then
                Some FromState
            elif hasModeCandidate then
                Some FromMode
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
                    Mode = joinAtoms modeAtoms
                    Interface = interfaceCandidate
                    State = joinAtoms stateAtoms
                    HostCandidate = ""
                }
            ExposureNotes =
                "Generated by deterministic T2 v0 parser. "
                + formatGeneratedConstraintNotes generatedConstraintAtoms
            DeltaAdd = false
            DeltaRemove = false
            DeltaConstrain = hasConditionCandidate || not (List.isEmpty generatedConstraintAtoms)
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
