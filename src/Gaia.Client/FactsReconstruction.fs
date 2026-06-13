module Gaia.Client.FactsReconstruction

open System
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let private isBlank (value: string) =
    String.IsNullOrWhiteSpace(value)

let private clean (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private equalsText left right =
    String.Equals(clean left, clean right, StringComparison.OrdinalIgnoreCase)

let private containsText needle haystack =
    let needleValue = clean needle
    let haystackValue = if isNull haystack then "" else haystack

    needleValue <> ""
    && haystackValue.IndexOf(needleValue, StringComparison.OrdinalIgnoreCase) >= 0

let private distinctText values =
    values
    |> List.map clean
    |> List.filter (fun value -> value <> "")
    |> List.fold
        (fun collected value ->
            if collected |> List.exists (equalsText value) then
                collected
            else
                collected @ [ value ])
        []

let private joinOrNone values =
    match distinctText values with
    | [] -> "None"
    | values -> String.concat ", " values

let suggestFactsTargetKind question =
    if question = factsQuestionWhyHostKnown then
        factsTargetKindHost
    elif question = factsQuestionWhatChangedAfterPhiParsed
         || question = factsQuestionWhatContextAttachedToPhi
         || question = factsQuestionWhatDecisionsFromPhi then
        factsTargetKindPhi
    else
        factsTargetKindCandidate

let private getCandidateDecision candidateId (model: Model) : CandidateDecision option =
    model.candidateDecisions
    |> List.tryFind (fun decision -> decision.CandidateId = candidateId)

let private getCandidateDecisionValue candidateId model =
    getCandidateDecision candidateId model
    |> Option.map (fun decision -> decision.Decision)
    |> Option.defaultValue Pending

let private formatDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let private getIncludedSequencedParsedPhisForFacts model =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds

let private getCandidateGroupGovernance model candidate =
    buildCandidateGroupGovernance
        candidate
        model.candidateDecisions
        model.sigmaBasisItemDecisions
        (getIncludedSequencedParsedPhisForFacts model)

let private groupStatusMatchesDecision expectedDecision groupStatus =
    match expectedDecision, groupStatus with
    | Accepted, GroupAccepted -> true
    | Rejected, GroupRejected -> true
    | Held, GroupHeld -> true
    | Pending, GroupPending -> true
    | _ -> false

let private classDecisionText governance =
    match governance.ClassDecisionRecord with
    | None -> "No class-level decision recorded"
    | Some _ -> "Class-level decision: " + formatDecisionValue governance.ClassDecision

let private lowerDecisionText decision =
    formatDecisionValue decision
    |> fun value -> value.ToLowerInvariant()

let private basisItemDecisionLines governance =
    governance.BasisItems
    |> List.map (fun (basisItem, decision) ->
        "'"
        + basisItem.AtomValue
        + "' was "
        + lowerDecisionText decision)

let private candidateGovernanceFactLines governance =
    [
        yield classDecisionText governance
        yield "Basis-derived status: " + formatCandidateGroupStatus governance.Status
        yield "Basis item counts: " + formatCandidateGroupDecisionCounts governance
        yield governance.Explanation
        yield! basisItemDecisionLines governance
        match governance.ConflictExplanation with
        | None -> ()
        | Some conflict -> yield conflict
    ]

let private getCurrentCandidates model : CandidateDelta list =
    getCurrentCandidateDeltas model

let private tryFindCandidateById candidateId (candidates: CandidateDelta list) =
    candidates
    |> List.tryFind (fun candidate -> candidate.CandidateId = candidateId)

let private candidateLabel (candidate: CandidateDelta) =
    formatCandidateDeltaKind candidate.Kind
    + " | "
    + candidate.Target
    + " | "
    + candidate.CandidateId

let private phiLabel (phi: PhiIntake) =
    if isBlank phi.RawStatement then
        phi.PhiId
    else
        phi.PhiId + " | " + phi.RawStatement

let private contextEntryLabel (entry: PhiContextEntry) =
    entry.ContextId
    + " | "
    + entry.Kind
    + ": "
    + entry.Value
    + " | "
    + entry.PhiId

let private hostLabel (entry: SigmaContextEntry) =
    entry.Value
    + " | support "
    + string entry.SupportCount
    + " | Phi "
    + String.concat ", " entry.SupportingPhiIds

let getFactsReconstructionTargetOptionsForKind targetKind model =
    match targetKind with
    | value when value = factsTargetKindCandidate ->
        model
        |> getCurrentCandidates
        |> List.map (fun (candidate: CandidateDelta) -> candidate.CandidateId, candidateLabel candidate)
    | value when value = factsTargetKindPhi ->
        let parsedPhiIds =
            model.parsedPhis
            |> List.map (fun parse -> parse.PhiId)

        let intakeOptions =
            model.ingestedPhis
            |> List.map (fun phi -> phi.PhiId, phiLabel phi)

        let parsedOnlyOptions =
            model.parsedPhis
            |> List.filter (fun parse -> not (model.ingestedPhis |> List.exists (fun phi -> phi.PhiId = parse.PhiId)))
            |> List.map (fun parse -> parse.PhiId, parse.PhiId + " | " + parse.Statement)

        intakeOptions @ parsedOnlyOptions
        |> List.filter (fun (phiId, _) -> parsedPhiIds |> List.contains phiId || model.ingestedPhis |> List.exists (fun phi -> phi.PhiId = phiId))
    | value when value = factsTargetKindHost ->
        model
        |> getCurrentSigmaContext
        |> fun sigmaContext -> sigmaContext.Hosts
        |> List.map (fun entry -> entry.Value, hostLabel entry)
    | value when value = factsTargetKindContextEntry ->
        model.phiContextEntries
        |> List.map (fun entry -> entry.ContextId, contextEntryLabel entry)
    | _ ->
        []

let getFactsReconstructionTargetOptions model =
    getFactsReconstructionTargetOptionsForKind model.factsReconstructionTargetKind model

let private chooseSelectedTargetId targetKind model =
    let options = getFactsReconstructionTargetOptionsForKind targetKind model

    if not (isBlank model.factsReconstructionTargetId)
       && options |> List.exists (fun (targetId, _) -> targetId = model.factsReconstructionTargetId) then
        Some model.factsReconstructionTargetId
    else
        options
        |> List.tryHead
        |> Option.map fst

let private tryFindPhiText phiId model =
    model.ingestedPhis
    |> List.tryFind (fun phi -> phi.PhiId = phiId)
    |> Option.map (fun phi -> phi.RawStatement)
    |> Option.orElseWith (fun () ->
        model.parsedPhis
        |> List.tryFind (fun parse -> parse.PhiId = phiId)
        |> Option.map (fun parse -> parse.Statement))

let private getSourcePhiTexts phiIds model =
    phiIds
    |> distinctText
    |> List.map (fun phiId ->
        let text =
            tryFindPhiText phiId model
            |> Option.defaultValue "Source Phi text unavailable."

        phiId, text)

let private contextKindForTarget target =
    match target with
    | "Host" -> Some "HostHint"
    | "Interface" -> Some "InterfaceHint"
    | "Mode" -> Some "ModeHint"
    | "State" -> Some "StateHint"
    | "Constraint" -> Some "ConstraintHint"
    | _ -> None

let private contextEntriesForCandidate model (candidate: CandidateDelta) : PhiContextEntry list =
    let supportingPhiIds = getCandidateSupportingPhiIds candidate
    let atomValues = getCandidateAtomValues candidate
    let relevantContextKind = contextKindForTarget candidate.Target

    model.phiContextEntries
    |> List.filter (fun entry ->
        supportingPhiIds |> List.exists (equalsText entry.PhiId)
        && (atomValues |> List.exists (equalsText entry.Value)
            || relevantContextKind
               |> Option.exists (fun contextKind -> canonicalPhiContextKind entry.Kind = contextKind)))

let private relatedLedgerEvents targetIds model =
    let ids = distinctText targetIds

    model.LedgerEvents
    |> List.filter (fun ledgerEvent ->
        ids
        |> List.exists (fun targetId ->
            equalsText ledgerEvent.TargetId targetId
            || containsText targetId ledgerEvent.Summary
            || containsText targetId ledgerEvent.Detail))

let private provenanceLabels (candidates: CandidateDelta list) (contextEntries: PhiContextEntry list) extraLabels =
    [
        yield! candidates |> List.map (fun candidate -> candidate.Provenance)
        yield! contextEntries |> List.map (fun entry -> entry.Provenance)
        yield! extraLabels
    ]
    |> distinctText

let private buildResult
    question
    targetKind
    targetId
    summary
    factLines
    phiIds
    (contextEntries: PhiContextEntry list)
    (candidates: CandidateDelta list)
    (decisions: CandidateDecision list)
    (ledgerEvents: LedgerEvent list)
    provenance
    missing =
    let sourcePhiIds =
        [
            yield! phiIds
            yield! candidates |> List.collect getCandidateSupportingPhiIds
            yield! contextEntries |> List.map (fun entry -> entry.PhiId)
        ]
        |> distinctText

    {
        Question = question
        TargetKind = targetKind
        TargetId = targetId
        AnswerSummary = summary
        FactLines = factLines |> distinctText
        SourcePhiIds = sourcePhiIds
        SourcePhiTexts = []
        ContextEntriesUsed = contextEntries
        CandidateFacts = candidates
        GovernanceDecisions = decisions
        RelatedLedgerEvents = ledgerEvents
        ProvenanceLabels = provenance
        MissingOrUnresolvedItems = missing |> distinctText
    }

let private completeResult model result =
    { result with
        SourcePhiTexts = getSourcePhiTexts result.SourcePhiIds model }

let private emptyResult question targetKind targetId summary missing model =
    buildResult question targetKind targetId summary [] [] [] [] [] [] [] missing
    |> completeResult model

let private tryResolveCandidateForQuestion expectedDecision model =
    let candidates = getCurrentCandidates model

    let selectedCandidate =
        if model.factsReconstructionTargetKind = factsTargetKindCandidate
           && not (isBlank model.factsReconstructionTargetId) then
            tryFindCandidateById model.factsReconstructionTargetId candidates
        else
            None

    match selectedCandidate with
    | Some candidate -> Some candidate
    | None ->
        match expectedDecision with
        | Some decision ->
            candidates
            |> List.tryFind (fun candidate ->
                let governance = getCandidateGroupGovernance model candidate
                groupStatusMatchesDecision decision governance.Status)
            |> Option.orElseWith (fun () ->
                candidates
                |> List.tryFind (fun candidate -> getCandidateDecisionValue candidate.CandidateId model = decision))
        | None ->
            candidates
            |> List.tryFind (fun candidate -> candidate.Kind <> NoStructuralChange)
            |> Option.orElseWith (fun () -> candidates |> List.tryHead)

let private reconstructCandidateDecision expectedDecision question model =
    match tryResolveCandidateForQuestion (Some expectedDecision) model with
    | None ->
        emptyResult
            question
            factsTargetKindCandidate
            ""
            "No current candidate could be resolved for this question."
            [ "Select a candidate or create a current T4 candidate by parsing Phi." ]
            model
    | Some candidate ->
        let actualDecision = getCandidateDecisionValue candidate.CandidateId model
        let decisionRecord = getCandidateDecision candidate.CandidateId model
        let groupGovernance = getCandidateGroupGovernance model candidate
        let decisionText = formatDecisionValue expectedDecision
        let actualDecisionText = formatDecisionValue actualDecision
        let groupStatusText = formatCandidateGroupStatus groupGovernance.Status
        let groupMatchesExpected = groupStatusMatchesDecision expectedDecision groupGovernance.Status
        let supportingPhiIds = getCandidateSupportingPhiIds candidate
        let contextEntries = contextEntriesForCandidate model candidate
        let targetIds =
            [
                yield candidate.CandidateId
                yield! supportingPhiIds
                yield! contextEntries |> List.map (fun entry -> entry.ContextId)
            ]

        let summary =
            if groupMatchesExpected then
                "Candidate "
                + formatCandidateDeltaKind candidate.Kind
                + " is "
                + decisionText.ToLowerInvariant()
                + " at the basis-derived group level. "
                + groupGovernance.Explanation
            elif actualDecision = expectedDecision then
                "Candidate "
                + formatCandidateDeltaKind candidate.Kind
                + " is not fully "
                + decisionText.ToLowerInvariant()
                + ". Class-level decision was "
                + decisionText
                + ", but the basis-derived status is "
                + groupStatusText
                + ". "
                + groupGovernance.Explanation
            else
                "Candidate "
                + formatCandidateDeltaKind candidate.Kind
                + " is not "
                + decisionText.ToLowerInvariant()
                + " at the basis-derived group level. Basis-derived status is "
                + groupStatusText
                + "; class-level decision is "
                + actualDecisionText
                + ". "
                + groupGovernance.Explanation

        let missing =
            [
                if not groupMatchesExpected then
                    yield "Basis-derived status is " + groupStatusText + ", not " + decisionText + "."
                match groupGovernance.ConflictExplanation with
                | None -> ()
                | Some conflict -> yield conflict
                if groupGovernance.PendingCount > 0 then
                    yield string groupGovernance.PendingCount + " basis item(s) are still pending."
                if groupGovernance.HeldCount > 0 then
                    yield string groupGovernance.HeldCount + " basis item(s) are held."
                if List.isEmpty supportingPhiIds then
                    yield "No supporting Phi IDs were found in the candidate basis."
            ]

        let factLines =
            [
                yield "Candidate type: " + formatCandidateDeltaKind candidate.Kind
                yield "Candidate target: " + candidate.Target
                yield "Candidate basis: " + joinOrNone candidate.RelevantSigmaBasis
                yield "Candidate provenance: " + candidate.Provenance
                yield! candidateGovernanceFactLines groupGovernance
            ]

        buildResult
            question
            factsTargetKindCandidate
            candidate.CandidateId
            summary
            factLines
            supportingPhiIds
            contextEntries
            [ candidate ]
            (decisionRecord |> Option.toList)
            (relatedLedgerEvents targetIds model)
            (provenanceLabels [ candidate ] contextEntries [])
            missing
        |> completeResult model

let private reconstructCandidateFacts question model =
    match tryResolveCandidateForQuestion None model with
    | None ->
        emptyResult
            question
            factsTargetKindCandidate
            ""
            "No current candidate could be resolved for this question."
            [ "Parse Phi to create current T4 candidates." ]
            model
    | Some candidate ->
        let decisionRecord = getCandidateDecision candidate.CandidateId model
        let decisionValue = getCandidateDecisionValue candidate.CandidateId model
        let groupGovernance = getCandidateGroupGovernance model candidate
        let supportingPhiIds = getCandidateSupportingPhiIds candidate
        let contextEntries = contextEntriesForCandidate model candidate
        let targetIds =
            [
                yield candidate.CandidateId
                yield! supportingPhiIds
                yield! contextEntries |> List.map (fun entry -> entry.ContextId)
            ]

        let summary =
            "Candidate "
            + formatCandidateDeltaKind candidate.Kind
            + " is supported by "
            + string (List.length candidate.RelevantSigmaBasis)
            + " current Sigma basis item(s). It was proposed because "
            + candidate.Reason
            + " Its basis-derived group status is "
            + formatCandidateGroupStatus groupGovernance.Status
            + "."

        let missing =
            [
                if isCandidateGroupUnresolvedOrConflicted groupGovernance then
                    yield "This candidate group is not fully resolved: " + groupGovernance.Explanation
                match groupGovernance.ConflictExplanation with
                | None -> ()
                | Some conflict -> yield conflict
                if List.isEmpty supportingPhiIds then
                    yield "No supporting Phi IDs were found in the candidate basis."
            ]

        let factLines =
            [
                yield "Candidate type: " + formatCandidateDeltaKind candidate.Kind
                yield "Candidate target: " + candidate.Target
                yield "Proposed transition: " + candidate.ProposedTransition
                yield "Candidate basis: " + joinOrNone candidate.RelevantSigmaBasis
                yield "Candidate provenance: " + candidate.Provenance
                yield "Stored class decision: " + formatDecisionValue decisionValue
                yield! candidateGovernanceFactLines groupGovernance
            ]

        buildResult
            question
            factsTargetKindCandidate
            candidate.CandidateId
            summary
            factLines
            supportingPhiIds
            contextEntries
            [ candidate ]
            (decisionRecord |> Option.toList)
            (relatedLedgerEvents targetIds model)
            (provenanceLabels [ candidate ] contextEntries [])
            missing
        |> completeResult model

let private tryResolveHostEntry model =
    let hostEntries = (getCurrentSigmaContext model).Hosts

    if model.factsReconstructionTargetKind = factsTargetKindHost then
        chooseSelectedTargetId factsTargetKindHost model
        |> Option.bind (fun hostValue -> hostEntries |> List.tryFind (fun entry -> equalsText entry.Value hostValue))
        |> Option.orElseWith (fun () -> hostEntries |> List.tryHead)
    else
        hostEntries |> List.tryHead

let private reconstructHostKnown question model =
    match tryResolveHostEntry model with
    | None ->
        emptyResult
            question
            factsTargetKindHost
            ""
            "No host is currently known in the project state."
            [ "Parse Phi with a host exposure or attach a HostHint context entry to a Phi and parse it." ]
            model
    | Some hostEntry ->
        let supportingPhiIds = hostEntry.SupportingPhiIds
        let contextEntries =
            model.phiContextEntries
            |> List.filter (fun entry ->
                supportingPhiIds |> List.exists (equalsText entry.PhiId)
                && canonicalPhiContextKind entry.Kind = "HostHint"
                && equalsText entry.Value hostEntry.Value)

        let hostCandidates =
            model
            |> getCurrentCandidates
            |> List.filter (fun candidate ->
                candidate.Kind = AddHost
                && getCandidateAtomValues candidate |> List.exists (equalsText hostEntry.Value))

        let contextPhrase =
            match contextEntries with
            | entry :: _ ->
                "Phi "
                + entry.PhiId
                + " had a "
                + entry.Kind
                + " context entry: "
                + entry.Value
                + "."
            | [] ->
                "Phi "
                + joinOrNone supportingPhiIds
                + " exposed the host value in its parsed T2 result."

        let candidatePhrase =
            match hostCandidates with
            | candidate :: _ ->
                " Candidate "
                + formatCandidateDeltaKind candidate.Kind
                + " was proposed from that Host basis."
            | [] -> ""

        let summary =
            "Host "
            + hostEntry.Value
            + " is known because "
            + contextPhrase
            + candidatePhrase

        let targetIds =
            [
                yield hostEntry.Value
                yield! supportingPhiIds
                yield! contextEntries |> List.map (fun entry -> entry.ContextId)
                yield! hostCandidates |> List.map (fun candidate -> candidate.CandidateId)
            ]

        let decisions =
            hostCandidates
            |> List.choose (fun candidate -> getCandidateDecision candidate.CandidateId model)

        let missing =
            [
                if List.isEmpty contextEntries && not (equalsText hostEntry.Provenance "Text") then
                    "No matching HostHint context entry was found for this host."
                if List.isEmpty hostCandidates then
                    "No current ADD HOST candidate references this host."
            ]

        let factLines =
            [
                "Host: " + hostEntry.Value
                "Source Phi IDs: " + joinOrNone supportingPhiIds
                "Host provenance: " + hostEntry.Provenance
                "Support count: " + string hostEntry.SupportCount
            ]

        buildResult
            question
            factsTargetKindHost
            hostEntry.Value
            summary
            factLines
            supportingPhiIds
            contextEntries
            hostCandidates
            decisions
            (relatedLedgerEvents targetIds model)
            (provenanceLabels hostCandidates contextEntries [ hostEntry.Provenance ])
            missing
        |> completeResult model

let private allPhiIds model =
    [
        yield! model.ingestedPhis |> List.map (fun phi -> phi.PhiId)
        yield! model.parsedPhis |> List.map (fun parse -> parse.PhiId)
    ]
    |> distinctText

let private tryResolvePhiId model =
    if model.factsReconstructionTargetKind = factsTargetKindPhi then
        chooseSelectedTargetId factsTargetKindPhi model
    elif model.factsReconstructionTargetKind = factsTargetKindContextEntry then
        chooseSelectedTargetId factsTargetKindContextEntry model
        |> Option.bind (fun contextId ->
            model.phiContextEntries
            |> List.tryFind (fun entry -> entry.ContextId = contextId)
            |> Option.map (fun entry -> entry.PhiId))
    else
        model.phiContextEntries
        |> List.tryHead
        |> Option.map (fun entry -> entry.PhiId)
        |> Option.orElseWith (fun () -> allPhiIds model |> List.tryHead)

let private tryResolveContextPhiId model =
    if model.factsReconstructionTargetKind = factsTargetKindPhi
       && not (isBlank model.factsReconstructionTargetId) then
        chooseSelectedTargetId factsTargetKindPhi model
    elif model.factsReconstructionTargetKind = factsTargetKindContextEntry
         && not (isBlank model.factsReconstructionTargetId) then
        chooseSelectedTargetId factsTargetKindContextEntry model
        |> Option.bind (fun contextId ->
            model.phiContextEntries
            |> List.tryFind (fun entry -> entry.ContextId = contextId)
            |> Option.map (fun entry -> entry.PhiId))
    else
        model.phiContextEntries
        |> List.tryHead
        |> Option.map (fun entry -> entry.PhiId)
        |> Option.orElseWith (fun () -> tryResolvePhiId model)

let private reconstructPhiContext question model =
    match tryResolveContextPhiId model with
    | None ->
        emptyResult
            question
            factsTargetKindPhi
            ""
            "No Phi could be resolved for this question."
            [ "Ingest or load Phi before reconstructing attached context." ]
            model
    | Some phiId ->
        let contextEntries =
            model.phiContextEntries
            |> List.filter (fun entry -> entry.PhiId = phiId)

        let summary =
            if List.isEmpty contextEntries then
                "Phi " + phiId + " has no attached context entries."
            else
                "Phi "
                + phiId
                + " has "
                + string (List.length contextEntries)
                + " attached context entr"
                + (if List.length contextEntries = 1 then "y." else "ies.")

        let targetIds =
            [
                yield phiId
                yield! contextEntries |> List.map (fun entry -> entry.ContextId)
            ]

        let factLines =
            contextEntries
            |> List.map (fun entry -> entry.Kind + ": " + entry.Value + " [" + entry.Provenance + "]")

        let missing =
            [
                if List.isEmpty contextEntries then
                    "No PhiContextEntries are attached to " + phiId + "."
            ]

        buildResult
            question
            factsTargetKindPhi
            phiId
            summary
            factLines
            [ phiId ]
            contextEntries
            []
            []
            (relatedLedgerEvents targetIds model)
            (provenanceLabels [] contextEntries [])
            missing
        |> completeResult model

let private atomLines prefix atoms =
    atoms
    |> List.map (fun (kind, value) -> prefix + " " + kind + ": " + value)

let private flattenAtomGroups (atomGroups: DeltaSigmaAtomGroups) =
    [
        yield! atomGroups.FunctionAtoms |> List.map (fun value -> "Function", value)
        yield! atomGroups.ModeAtoms |> List.map (fun value -> "Mode", value)
        yield! atomGroups.InterfaceAtoms |> List.map (fun value -> "Interface", value)
        yield! atomGroups.StateAtoms |> List.map (fun value -> "State", value)
        yield! atomGroups.HostAtoms |> List.map (fun value -> "Host", value)
        yield! atomGroups.ConstraintAtoms |> List.map (fun value -> "Constraint", value)
    ]

let private reconstructPhiChange question model =
    match tryResolvePhiId model with
    | None ->
        emptyResult
            question
            factsTargetKindPhi
            ""
            "No Phi could be resolved for this question."
            [ "Ingest and parse Phi before reconstructing parse changes." ]
            model
    | Some phiId ->
        match model.parsedPhis |> List.tryFind (fun parse -> parse.PhiId = phiId) with
        | None ->
            emptyResult
                question
                factsTargetKindPhi
                phiId
                ("Phi " + phiId + " has not been parsed yet.")
                [ "No T2 parsed result exists for " + phiId + "." ]
                model
        | Some parse ->
            let sequencedParsedPhis = getSequencedParsedPhis model.parsedPhis
            let parseIndex =
                sequencedParsedPhis
                |> List.tryFind (fun (_, candidateParse) -> candidateParse.PhiId = phiId)
                |> Option.map fst
                |> Option.defaultValue 0

            let includedBefore =
                sequencedParsedPhis
                |> List.filter (fun (sequenceNumber, parse) ->
                    sequenceNumber < parseIndex
                    && not (isPhiExcluded model.excludedPhiIds parse.PhiId))

            let includedAfter =
                sequencedParsedPhis
                |> List.filter (fun (sequenceNumber, parse) ->
                    sequenceNumber <= parseIndex
                    && not (isPhiExcluded model.excludedPhiIds parse.PhiId))

            let beforeSigma = buildSigmaContextWithContextEntries model.phiContextEntries includedBefore
            let afterSigma = buildSigmaContextWithContextEntries model.phiContextEntries includedAfter
            let addedAtoms = buildDeltaSigmaAtomGroups beforeSigma afterSigma |> flattenAtomGroups
            let relatedCandidates =
                model
                |> getCurrentCandidates
                |> List.filter (fun candidate -> getCandidateSupportingPhiIds candidate |> List.exists (equalsText phiId))
            let contextEntries =
                model.phiContextEntries
                |> List.filter (fun entry -> entry.PhiId = phiId)

            let summary =
                if model.excludedPhiIds |> List.contains phiId then
                    "Phi "
                    + phiId
                    + " is parsed but currently excluded from replay, so its parsed facts do not contribute to the current Sigma context."
                elif List.isEmpty addedAtoms then
                    "After Phi "
                    + phiId
                    + " was parsed, no new current Sigma atoms were added."
                else
                    "After Phi "
                    + phiId
                    + " was parsed, current Sigma gained "
                    + string (List.length addedAtoms)
                    + " atom(s): "
                    + (addedAtoms |> List.map (fun (kind, value) -> kind + " " + value) |> String.concat "; ")
                    + "."

            let targetIds =
                [
                    yield phiId
                    yield! contextEntries |> List.map (fun entry -> entry.ContextId)
                    yield! relatedCandidates |> List.map (fun candidate -> candidate.CandidateId)
                ]

            let missing =
                [
                    if model.excludedPhiIds |> List.contains phiId then
                        "Phi " + phiId + " is excluded from replay."
                    if List.isEmpty addedAtoms then
                        "No new current Sigma atoms were added by this Phi."
                ]

            let factLines =
                [
                    yield "T2 statement: " + parse.Statement
                    yield "T2 host candidate: " + (if isBlank parse.Exposure.HostCandidate then "None" else parse.Exposure.HostCandidate)
                    yield "T2 interface: " + (if isBlank parse.Exposure.Interface then "None" else parse.Exposure.Interface)
                    yield "T2 mode: " + (if isBlank parse.Exposure.Mode then "None" else parse.Exposure.Mode)
                    yield "T2 state: " + (if isBlank parse.Exposure.State then "None" else parse.Exposure.State)
                    yield! atomLines "Added" addedAtoms
                ]

            let decisions =
                relatedCandidates
                |> List.choose (fun candidate -> getCandidateDecision candidate.CandidateId model)

            buildResult
                question
                factsTargetKindPhi
                phiId
                summary
                factLines
                [ phiId ]
                contextEntries
                relatedCandidates
                decisions
                (relatedLedgerEvents targetIds model)
                (provenanceLabels relatedCandidates contextEntries [])
                missing
            |> completeResult model

let private reconstructDecisionsFromPhi question model =
    match tryResolvePhiId model with
    | None ->
        emptyResult
            question
            factsTargetKindPhi
            ""
            "No Phi could be resolved for this question."
            [ "Ingest and parse Phi before reconstructing decisions from it." ]
            model
    | Some phiId ->
        let relatedCandidates =
            model
            |> getCurrentCandidates
            |> List.filter (fun candidate -> getCandidateSupportingPhiIds candidate |> List.exists (equalsText phiId))

        let decisions =
            relatedCandidates
            |> List.choose (fun candidate -> getCandidateDecision candidate.CandidateId model)

        let relatedGovernance =
            relatedCandidates
            |> List.map (fun candidate -> candidate, getCandidateGroupGovernance model candidate)

        let contextEntries =
            model.phiContextEntries
            |> List.filter (fun entry -> entry.PhiId = phiId)

        let unresolvedCandidates =
            relatedGovernance
            |> List.filter (fun (_, governance) -> isCandidateGroupUnresolvedOrConflicted governance)

        let summary =
            if List.isEmpty relatedCandidates then
                "No current T4 candidates are supported by Phi " + phiId + "."
            elif List.isEmpty decisions then
                "Phi "
                + phiId
                + " supports "
                + string (List.length relatedCandidates)
                + " current candidate(s), but no T5 decisions have been recorded for them."
            else
                "Phi "
                + phiId
                + " supports "
                + string (List.length relatedCandidates)
                + " current candidate(s), with "
                + string (List.length decisions)
                + " recorded T5 decision(s)."

        let factLines =
            relatedGovernance
            |> List.collect (fun (candidate, governance) ->
                [
                    yield
                        formatCandidateDeltaKind candidate.Kind
                        + " -> basis-derived "
                        + formatCandidateGroupStatus governance.Status
                        + "; "
                        + classDecisionText governance
                    yield! basisItemDecisionLines governance
                ])

        let missing =
            [
                if List.isEmpty relatedCandidates then
                    "No candidate basis currently references " + phiId + "."
                yield!
                    unresolvedCandidates
                    |> List.map (fun (candidate, governance) ->
                        "Candidate "
                        + candidate.CandidateId
                        + " needs governance reconciliation because "
                        + governance.Explanation)
                yield!
                    unresolvedCandidates
                    |> List.choose (fun (_, governance) -> governance.ConflictExplanation)
            ]

        let targetIds =
            [
                yield phiId
                yield! contextEntries |> List.map (fun entry -> entry.ContextId)
                yield! relatedCandidates |> List.map (fun candidate -> candidate.CandidateId)
            ]

        buildResult
            question
            factsTargetKindPhi
            phiId
            summary
            factLines
            [ phiId ]
            contextEntries
            relatedCandidates
            decisions
            (relatedLedgerEvents targetIds model)
            (provenanceLabels relatedCandidates contextEntries [])
            missing
        |> completeResult model

let private getCurrentBasisItems model (candidates: CandidateDelta list) =
    let sequencedParsedPhis =
        model.parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds

    candidates
    |> List.collect (fun candidate ->
        buildSigmaBasisItemReviews candidate sequencedParsedPhis
        |> List.map (fun basisItem -> candidate, basisItem))

let private reconstructUnresolved question model =
    let candidates =
        model
        |> getCurrentCandidates
        |> List.filter (fun candidate -> candidate.Kind <> NoStructuralChange)

    let candidateGovernance =
        candidates
        |> List.map (fun candidate -> candidate, getCandidateGroupGovernance model candidate)

    let unresolvedCandidateGroups =
        candidateGovernance
        |> List.filter (fun (_, governance) -> isCandidateGroupUnresolvedOrConflicted governance)

    let pendingOrHeldBasisItems =
        candidateGovernance
        |> List.collect (fun (candidate, governance) ->
            governance.BasisItems
            |> List.choose (fun (basisItem, decision) ->
                if decision = Pending || decision = Held then
                    Some (candidate, basisItem, decision)
                else
                    None))

    let pendingCandidates =
        candidates
        |> List.filter (fun candidate ->
            let governance = getCandidateGroupGovernance model candidate
            governance.Status = GroupPending || governance.Status = GroupPartiallyGoverned)

    let pendingBasisItems =
        pendingOrHeldBasisItems
        |> List.filter (fun (_, _, decision) -> decision = Pending)

    let sourcePhiIds =
        [
            yield! unresolvedCandidateGroups |> List.collect (fun (candidate, _) -> getCandidateSupportingPhiIds candidate)
            yield! pendingOrHeldBasisItems |> List.collect (fun (_, basisItem, _) -> basisItem.SupportingPhiIds)
        ]
        |> distinctText

    let contextEntries =
        model.phiContextEntries
        |> List.filter (fun entry -> sourcePhiIds |> List.exists (equalsText entry.PhiId))

    let summary =
        if List.isEmpty unresolvedCandidateGroups && List.isEmpty pendingOrHeldBasisItems then
            "No current candidate group or basis item is unresolved."
        else
            "Current project has "
            + string (List.length unresolvedCandidateGroups)
            + " candidate group(s) needing governance reconciliation and "
            + string (List.length pendingBasisItems)
            + " pending basis item(s)."

    let factLines =
        [
            yield!
                unresolvedCandidateGroups
                |> List.map (fun (candidate, governance) ->
                    "Candidate group: "
                    + formatCandidateDeltaKind candidate.Kind
                    + " | "
                    + candidate.Target
                    + " | "
                    + candidate.CandidateId
                    + " | basis-derived "
                    + formatCandidateGroupStatus governance.Status
                    + " | "
                    + classDecisionText governance)
            yield!
                pendingOrHeldBasisItems
                |> List.map (fun (candidate, basisItem, decision) ->
                    formatDecisionValue decision
                    + " basis item: "
                    + basisItem.Kind
                    + " "
                    + basisItem.AtomValue
                    + " | candidate "
                    + formatCandidateDeltaKind candidate.Kind)
        ]

    let missing =
        [
            yield!
                unresolvedCandidateGroups
                |> List.map (fun (candidate, governance) ->
                    "Candidate "
                    + candidate.CandidateId
                    + " is not fully resolved because "
                    + governance.Explanation)
            yield!
                unresolvedCandidateGroups
                |> List.choose (fun (_, governance) -> governance.ConflictExplanation)
            yield!
                pendingOrHeldBasisItems
                |> List.map (fun (candidate, basisItem, decision) ->
                    "Basis item "
                    + basisItem.Kind
                    + ": "
                    + basisItem.AtomValue
                    + " under "
                    + formatCandidateDeltaKind candidate.Kind
                    + " is still unresolved because its basis-item decision is "
                    + formatDecisionValue decision
                    + ".")
        ]

    let targetIds =
        [
            yield! unresolvedCandidateGroups |> List.map (fun (candidate, _) -> candidate.CandidateId)
            yield! pendingOrHeldBasisItems |> List.map (fun (_, basisItem, _) -> basisItem.Key)
            yield! sourcePhiIds
            yield! contextEntries |> List.map (fun entry -> entry.ContextId)
        ]

    buildResult
        question
        "Project"
        "Current project"
        summary
        factLines
        sourcePhiIds
        contextEntries
        (unresolvedCandidateGroups |> List.map fst)
        []
        (relatedLedgerEvents targetIds model)
        (provenanceLabels (unresolvedCandidateGroups |> List.map fst) contextEntries [])
        missing
    |> completeResult model

let reconstructFacts model =
    match model.factsReconstructionQuestion with
    | question when question = factsQuestionWhyCandidateAccepted ->
        reconstructCandidateDecision Accepted question model
    | question when question = factsQuestionWhyCandidateRejected ->
        reconstructCandidateDecision Rejected question model
    | question when question = factsQuestionWhyHostKnown ->
        reconstructHostKnown question model
    | question when question = factsQuestionWhatFactsSupportedCandidate ->
        reconstructCandidateFacts question model
    | question when question = factsQuestionWhatChangedAfterPhiParsed ->
        reconstructPhiChange question model
    | question when question = factsQuestionWhatContextAttachedToPhi ->
        reconstructPhiContext question model
    | question when question = factsQuestionWhatDecisionsFromPhi ->
        reconstructDecisionsFromPhi question model
    | question when question = factsQuestionWhatStillUnresolved ->
        reconstructUnresolved question model
    | question ->
        emptyResult
            question
            model.factsReconstructionTargetKind
            model.factsReconstructionTargetId
            "This stakeholder question is not recognized by Facts Reconstruction v1."
            [ "Select one of the predefined stakeholder questions." ]
            model
