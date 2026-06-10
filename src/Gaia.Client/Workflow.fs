module Gaia.Client.Workflow

open System
open Elmish
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Ledger
open Gaia.Client.AppState

let upsertParsedPhi parse parsedPhis =
    if parsedPhis |> List.exists (fun parsedPhi -> parsedPhi.PhiId = parse.PhiId) then
        parsedPhis
        |> List.map (fun parsedPhi ->
            if parsedPhi.PhiId = parse.PhiId then
                parse
            else
                parsedPhi)
    else
        parsedPhis @ [ parse ]

let isPhiExcluded excludedPhiIds phiId =
    excludedPhiIds
    |> List.contains phiId

let getSequencedParsedPhis parsedPhis =
    parsedPhis
    |> List.mapi (fun index parse -> index + 1, parse)

let getIncludedSequencedParsedPhis excludedPhiIds parsedPhis =
    parsedPhis
    |> getSequencedParsedPhis
    |> List.filter (fun (_, parse) -> not (isPhiExcluded excludedPhiIds parse.PhiId))

let buildSigmaContextEntries getValue sequencedParsedPhis =
    sequencedParsedPhis
    |> List.choose (fun (parseSequenceNumber, parse) ->
        let value = getValue parse

        if value = "" then
            None
        else
            Some
                {
                    Value = value
                    SourcePhiId = parse.PhiId
                    SourcePhiStatement = parse.Statement
                    ParseSequenceNumber = parseSequenceNumber
                    SupportCount = 1
                    SupportingPhiIds = [ parse.PhiId ]
                })
    |> List.groupBy (fun entry -> entry.Value)
    |> List.map (fun (_, entries) ->
        let firstEntry = entries |> List.head

        let supportingPhiIds =
            entries
            |> List.map (fun entry -> entry.SourcePhiId)
            |> List.distinct

        { firstEntry with
            SupportCount = List.length supportingPhiIds
            SupportingPhiIds = supportingPhiIds })

let buildSigmaContext sequencedParsedPhis =
    {
        Functions = buildSigmaContextEntries (fun parse -> parse.Exposure.Function) sequencedParsedPhis
        Modes = buildSigmaContextEntries (fun parse -> parse.Exposure.Mode) sequencedParsedPhis
        Interfaces = buildSigmaContextEntries (fun parse -> parse.Exposure.Interface) sequencedParsedPhis
        States = buildSigmaContextEntries (fun parse -> parse.Exposure.State) sequencedParsedPhis
        Hosts = buildSigmaContextEntries (fun parse -> parse.Exposure.HostCandidate) sequencedParsedPhis
    }

let getCurrentSigmaContext (model: Model) =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds
    |> buildSigmaContext

let formatPhiEvidenceTarget (phi: PhiIntake) =
    if String.IsNullOrWhiteSpace(phi.RawStatement) then
        phi.PhiId
    else
        phi.PhiId + " - " + phi.RawStatement

let formatSigmaEvidenceTarget atomKind (entry: SigmaContextEntry) =
    entry.Value
    + " ("
    + atomKind
    + "; support "
    + string entry.SupportCount
    + "; Phi "
    + String.concat ", " entry.SupportingPhiIds
    + ")"

let getEvidenceTargetOptionsForKind targetKind (model: Model) =
    let sigmaContext = getCurrentSigmaContext model

    match targetKind with
    | "Phi" ->
        model.ingestedPhis
        |> List.map (fun phi -> phi.PhiId, formatPhiEvidenceTarget phi)
    | "Function" ->
        sigmaContext.Functions
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "Function" entry)
    | "Mode" ->
        sigmaContext.Modes
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "Mode" entry)
    | "Interface" ->
        sigmaContext.Interfaces
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "Interface" entry)
    | "State" ->
        sigmaContext.States
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "State" entry)
    | "Host" ->
        sigmaContext.Hosts
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "Host" entry)
    | _ ->
        []

let getCurrentEvidenceTargetOptions (model: Model) =
    getEvidenceTargetOptionsForKind model.evidenceTargetKind model

let tryResolveEvidenceTargetLabel (model: Model) =
    model
    |> getCurrentEvidenceTargetOptions
    |> List.tryFind (fun (targetId, _) -> targetId = model.evidenceTargetId)
    |> Option.map snd

let createEvidenceId evidenceRecords =
    sprintf "EVD-%06d" (List.length evidenceRecords + 1)

let emptyDeltaSigmaAtomGroups =
    {
        FunctionAtoms = []
        ModeAtoms = []
        InterfaceAtoms = []
        StateAtoms = []
        HostAtoms = []
    }

let getPhiAtomGroups (parse: PhiParse) =
    let asAtom value =
        if value = "" then
            []
        else
            [ value ]

    {
        FunctionAtoms = asAtom parse.Exposure.Function
        ModeAtoms = asAtom parse.Exposure.Mode
        InterfaceAtoms = asAtom parse.Exposure.Interface
        StateAtoms = asAtom parse.Exposure.State
        HostAtoms = asAtom parse.Exposure.HostCandidate
    }

let getAddedAtomValues beforeEntries afterEntries =
    let beforeValues =
        beforeEntries
        |> List.map (fun entry -> entry.Value)
        |> Set.ofList

    afterEntries
    |> List.choose (fun entry ->
        if beforeValues |> Set.contains entry.Value then
            None
        else
            Some entry.Value)

let buildDeltaSigmaAtomGroups (beforeSigma: SigmaContext) (afterSigma: SigmaContext) =
    {
        FunctionAtoms = getAddedAtomValues beforeSigma.Functions afterSigma.Functions
        ModeAtoms = getAddedAtomValues beforeSigma.Modes afterSigma.Modes
        InterfaceAtoms = getAddedAtomValues beforeSigma.Interfaces afterSigma.Interfaces
        StateAtoms = getAddedAtomValues beforeSigma.States afterSigma.States
        HostAtoms = getAddedAtomValues beforeSigma.Hosts afterSigma.Hosts
    }

let getAlreadyKnownAtomValues sourcePhiId beforeEntries atomValues =
    atomValues
    |> List.filter (fun atomValue ->
        beforeEntries
        |> List.tryFind (fun entry -> entry.Value = atomValue)
        |> Option.exists (fun entry ->
            not (entry.SupportingPhiIds |> List.contains sourcePhiId)
            && (entry.SupportingPhiIds |> List.exists (fun supportingPhiId -> supportingPhiId <> sourcePhiId))))
    |> List.distinct

let buildAlreadyKnownAtomGroups sourcePhiId (beforeSigma: SigmaContext) (parse: PhiParse) =
    let parseAtoms = getPhiAtomGroups parse

    {
        FunctionAtoms = getAlreadyKnownAtomValues sourcePhiId beforeSigma.Functions parseAtoms.FunctionAtoms
        ModeAtoms = getAlreadyKnownAtomValues sourcePhiId beforeSigma.Modes parseAtoms.ModeAtoms
        InterfaceAtoms = getAlreadyKnownAtomValues sourcePhiId beforeSigma.Interfaces parseAtoms.InterfaceAtoms
        StateAtoms = getAlreadyKnownAtomValues sourcePhiId beforeSigma.States parseAtoms.StateAtoms
        HostAtoms = getAlreadyKnownAtomValues sourcePhiId beforeSigma.Hosts parseAtoms.HostAtoms
    }

let hasDeltaSigmaAtomChanges atomGroups =
    [
        atomGroups.FunctionAtoms
        atomGroups.ModeAtoms
        atomGroups.InterfaceAtoms
        atomGroups.StateAtoms
        atomGroups.HostAtoms
    ]
    |> List.exists (fun atoms -> not (List.isEmpty atoms))

let hasDeltaSigmaAnalysisChanges analysis =
    hasDeltaSigmaAtomChanges analysis.AddedAtoms
    || hasDeltaSigmaAtomChanges analysis.RemovedAtoms
    || hasDeltaSigmaAtomChanges analysis.AlreadyKnownAtoms

let createDeltaSigmaAnalysis action reason sourcePhiId sourceStatement alreadyKnownAtoms beforeSigma afterSigma =
    {
        Action = action
        SourcePhiId = sourcePhiId
        SourceStatement = sourceStatement
        Reason = reason
        AddedAtoms = buildDeltaSigmaAtomGroups beforeSigma afterSigma
        RemovedAtoms = buildDeltaSigmaAtomGroups afterSigma beforeSigma
        AlreadyKnownAtoms = alreadyKnownAtoms
    }

let buildReplayDeltaSigmaAnalysis phiId wasExcluded beforeSigma afterSigma parsedPhis =
    let sourceStatement =
        parsedPhis
        |> List.tryFind (fun parse -> parse.PhiId = phiId)
        |> Option.map (fun parse -> parse.Statement)
        |> Option.defaultValue "Source statement unavailable."

    let action =
        if wasExcluded then
            "Included " + phiId
        else
            "Excluded " + phiId

    let reason =
        if wasExcluded then
            "This Phi is now included in replay, so its exposed atoms can enter current Sigma."
        else
            "This Phi is now excluded from replay, so atoms only supplied by it can leave current Sigma."

    let alreadyKnownAtoms =
        if wasExcluded then
            parsedPhis
            |> List.tryFind (fun parse -> parse.PhiId = phiId)
            |> Option.map (buildAlreadyKnownAtomGroups phiId beforeSigma)
            |> Option.defaultValue emptyDeltaSigmaAtomGroups
        else
            emptyDeltaSigmaAtomGroups

    createDeltaSigmaAnalysis action reason phiId sourceStatement alreadyKnownAtoms beforeSigma afterSigma

let candidateDeltaKindKey = function
    | AddUnknownRevealMissingHost -> "AddUnknownRevealMissingHost"
    | AddInterface -> "AddInterface"
    | AddState -> "AddState"
    | AddMode -> "AddMode"
    | ReinforcedSigmaAtom -> "ReinforcedSigmaAtom"
    | NoStructuralChange -> "NoStructuralChange"

let formatCandidateDeltaKind = function
    | AddUnknownRevealMissingHost -> "ADD UNKNOWN / REVEAL MISSING HOST"
    | AddInterface -> "ADD INTERFACE"
    | AddState -> "ADD STATE"
    | AddMode -> "ADD MODE"
    | ReinforcedSigmaAtom -> "REINFORCED SIGMA ATOM"
    | NoStructuralChange -> "NO STRUCTURAL CHANGE"

let createCandidateId kind target =
    candidateDeltaKindKey kind + "::" + target

let createCandidateDelta kind target proposedTransition reason relevantSigmaBasis =
    {
        CandidateId = createCandidateId kind target
        Kind = kind
        Target = target
        ProposedTransition = proposedTransition
        Reason = reason
        RelevantSigmaBasis = relevantSigmaBasis
        Confidence = "Medium"
        Status = "Candidate only; not promoted"
    }

let formatSigmaBasis atomKind (entry: SigmaContextEntry) =
    atomKind
    + ": "
    + entry.Value
    + " (support "
    + string entry.SupportCount
    + "; Φ "
    + String.concat ", " entry.SupportingPhiIds
    + ")"

let formatSigmaBasisGroup atomKind entries =
    entries
    |> List.map (formatSigmaBasis atomKind)

let buildReinforcedCandidateDeltas atomKind entries =
    entries
    |> List.filter (fun entry -> entry.SupportCount > 1)
    |> List.map (fun entry ->
        createCandidateDelta
            ReinforcedSigmaAtom
            atomKind
            ("Recognize reinforced " + atomKind + " atom in the current Σ basis.")
            "Multiple Phi support the same reasoning atom."
            [ formatSigmaBasis atomKind entry ])

let formulateCandidateDeltas (sigmaContext: SigmaContext) =
    let candidates =
        [
            if not (List.isEmpty sigmaContext.Functions) && List.isEmpty sigmaContext.Hosts then
                yield
                    createCandidateDelta
                        AddUnknownRevealMissingHost
                        "Host"
                        "Add an unknown Host placeholder or reveal the missing Host candidate."
                        "Functions are known but no host candidate has been identified."
                        (formatSigmaBasisGroup "Function" sigmaContext.Functions)

            if not (List.isEmpty sigmaContext.Interfaces) then
                yield
                    createCandidateDelta
                        AddInterface
                        "Interface"
                        "Add exposed Interface atoms as candidate Σ structure."
                        "Interface-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "Interface" sigmaContext.Interfaces)

            if not (List.isEmpty sigmaContext.States) then
                yield
                    createCandidateDelta
                        AddState
                        "State"
                        "Add exposed State atoms as candidate Σ structure."
                        "State-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "State" sigmaContext.States)

            if not (List.isEmpty sigmaContext.Modes) then
                yield
                    createCandidateDelta
                        AddMode
                        "Mode"
                        "Add exposed Mode atoms as candidate Σ structure."
                        "Mode-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "Mode" sigmaContext.Modes)

            yield! buildReinforcedCandidateDeltas "Function" sigmaContext.Functions
            yield! buildReinforcedCandidateDeltas "Mode" sigmaContext.Modes
            yield! buildReinforcedCandidateDeltas "Interface" sigmaContext.Interfaces
            yield! buildReinforcedCandidateDeltas "State" sigmaContext.States
            yield! buildReinforcedCandidateDeltas "Host" sigmaContext.Hosts
        ]

    if List.isEmpty candidates then
        [
            createCandidateDelta
                NoStructuralChange
                "None"
                "Keep current Σ unchanged."
                "No actionable candidate Delta Sigma transition was detected."
                [ "No included Sigma atom produced a T4 structural candidate." ]
        ]
    else
        candidates

let getCurrentCandidateDeltas (model: Model) =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds
    |> buildSigmaContext
    |> formulateCandidateDeltas

let getCandidateDecisionRationale = function
    | Pending -> ""
    | Accepted -> "Candidate accepted for later Sigma promotion."
    | Rejected -> "Candidate rejected; no Sigma promotion should occur."
    | Held -> "Candidate held for later review."

let createCandidateDecision (decision: CandidateDecisionValue) (candidate: CandidateDelta) =
    {
        CandidateId = candidate.CandidateId
        CandidateType = formatCandidateDeltaKind candidate.Kind
        Target = candidate.Target
        Decision = decision
        Timestamp = DateTime.UtcNow
        Rationale = getCandidateDecisionRationale decision
    }

let upsertCandidateDecision (candidateDecision: CandidateDecision) (candidateDecisions: CandidateDecision list) =
    if candidateDecisions |> List.exists (fun decision -> decision.CandidateId = candidateDecision.CandidateId) then
        candidateDecisions
        |> List.map (fun decision ->
            if decision.CandidateId = candidateDecision.CandidateId then
                candidateDecision
            else
                decision)
    else
        candidateDecisions @ [ candidateDecision ]

let getTextAfter (marker: string) (value: string) =
    let index = value.IndexOf(marker, StringComparison.Ordinal)

    if index < 0 then
        ""
    else
        value.Substring(index + marker.Length)

let getTextBefore (marker: string) (value: string) =
    let index = value.IndexOf(marker, StringComparison.Ordinal)

    if index < 0 then
        value
    else
        value.Substring(0, index)

let extractAtomValueFromBasis basis =
    let withoutKind = getTextAfter ": " basis

    let source =
        if String.IsNullOrWhiteSpace(withoutKind) then
            basis
        else
            withoutKind

    getTextBefore " (support " source
    |> fun value -> value.Trim()

let extractSupportingPhiIdsFromBasis basis =
    let phiText =
        getTextAfter "; Φ " basis
        |> getTextBefore ")"

    if String.IsNullOrWhiteSpace(phiText) then
        []
    else
        phiText.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun value -> value.Trim())
        |> Array.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
        |> Array.toList

let getCandidateSupportingPhiIds candidate =
    candidate.RelevantSigmaBasis
    |> List.collect extractSupportingPhiIdsFromBasis
    |> List.distinct

let getCandidateAtomValues candidate =
    candidate.RelevantSigmaBasis
    |> List.map extractAtomValueFromBasis
    |> List.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
    |> List.distinct

type SigmaBasisItemReview =
    {
        Key: string
        Kind: string
        AtomValue: string
        SupportCount: int
        SupportingPhiIds: string list
        RawPhiPreview: string option
        SourceText: string
    }

let normalizeBasisItemKeyPart (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        "none"
    else
        value.Trim().ToLowerInvariant().Split([| ' '; '\t'; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> String.concat " "

let createSigmaBasisItemKey candidateId atomKind atomValue =
    candidateId
    + "::"
    + normalizeBasisItemKeyPart atomKind
    + "::"
    + normalizeBasisItemKeyPart atomValue

let extractAtomKindFromBasis fallbackKind (basis: string) =
    let index = basis.IndexOf(": ", StringComparison.Ordinal)

    if index > 0 then
        basis.Substring(0, index).Trim()
    else
        fallbackKind

let extractSupportCountFromBasis basis =
    let supportText =
        getTextAfter "(support " basis
        |> getTextBefore ";"
        |> fun value -> value.Trim()

    let mutable supportCount = 0

    if Int32.TryParse(supportText, &supportCount) then
        supportCount
    else
        0

let buildPhiStatementMap sequencedParsedPhis =
    sequencedParsedPhis
    |> List.map (fun (_, parse: PhiParse) -> parse.PhiId, parse.Statement)
    |> Map.ofList

let tryFindRawPhiPreview phiStatementById supportingPhiIds =
    supportingPhiIds
    |> List.tryPick (fun phiId ->
        phiStatementById
        |> Map.tryFind phiId
        |> Option.bind (fun statement ->
            if String.IsNullOrWhiteSpace(statement) then
                None
            else
                Some statement))

let buildSigmaBasisItemReviews (candidate: CandidateDelta) sequencedParsedPhis =
    let phiStatementById = buildPhiStatementMap sequencedParsedPhis

    candidate.RelevantSigmaBasis
    |> List.map (fun basis ->
        let atomKind = extractAtomKindFromBasis candidate.Target basis
        let atomValue = extractAtomValueFromBasis basis
        let supportingPhiIds = extractSupportingPhiIdsFromBasis basis
        let parsedSupportCount = extractSupportCountFromBasis basis
        let supportCount =
            if parsedSupportCount > 0 then
                parsedSupportCount
            else
                List.length supportingPhiIds

        {
            Key = createSigmaBasisItemKey candidate.CandidateId atomKind atomValue
            Kind = atomKind
            AtomValue = atomValue
            SupportCount = supportCount
            SupportingPhiIds = supportingPhiIds
            RawPhiPreview = tryFindRawPhiPreview phiStatementById supportingPhiIds
            SourceText = basis
        })

let getSigmaBasisItemDecisionValue basisItemKey sigmaBasisItemDecisions =
    sigmaBasisItemDecisions
    |> Map.tryFind basisItemKey
    |> Option.defaultValue Pending

type SigmaBasisItemLedgerContext =
    {
        Candidate: CandidateDelta
        BasisItem: SigmaBasisItemReview
    }

let getCurrentSigmaBasisItemLedgerContexts (model: Model) =
    let sequencedParsedPhis =
        model.parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds

    sequencedParsedPhis
    |> buildSigmaContext
    |> formulateCandidateDeltas
    |> List.collect (fun candidate ->
        buildSigmaBasisItemReviews candidate sequencedParsedPhis
        |> List.map (fun basisItem ->
            {
                Candidate = candidate
                BasisItem = basisItem
            }))

let tryFindCurrentSigmaBasisItemLedgerContext basisItemKey model =
    model
    |> getCurrentSigmaBasisItemLedgerContexts
    |> List.tryFind (fun context -> context.BasisItem.Key = basisItemKey)

let formatSigmaBasisItemDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let getSigmaBasisItemDecisionRationale = function
    | Pending -> ""
    | Accepted -> "Basis item accepted for later Sigma atom promotion."
    | Rejected -> "Basis item rejected; no Sigma atom promotion should occur."
    | Held -> "Basis item held for later review."

let getSigmaBasisItemDecisionLedgerKind = function
    | Accepted -> "SigmaBasisItemAccepted"
    | Rejected -> "SigmaBasisItemRejected"
    | Held -> "SigmaBasisItemHeld"
    | Pending -> "SigmaBasisItemReviewed"

let getSigmaBasisItemDecisionLedgerSummary = function
    | Accepted -> "Sigma basis item accepted"
    | Rejected -> "Sigma basis item rejected"
    | Held -> "Sigma basis item held"
    | Pending -> "Sigma basis item reviewed"

let formatSupportingPhiIds phiIds =
    match phiIds with
    | [] -> "None"
    | values -> String.concat ", " values

let formatSigmaBasisItemLedgerDetail actionScope decision context =
    let basisItem = context.BasisItem
    let candidate = context.Candidate
    let rationale = getSigmaBasisItemDecisionRationale decision

    "Candidate type: "
    + formatCandidateDeltaKind candidate.Kind
    + "; Candidate target: "
    + candidate.Target
    + "; Atom kind: "
    + basisItem.Kind
    + "; Atom value: "
    + basisItem.AtomValue
    + "; Supporting Phi IDs: "
    + formatSupportingPhiIds basisItem.SupportingPhiIds
    + "; Decision: "
    + formatSigmaBasisItemDecisionValue decision
    + "; Action scope: "
    + actionScope
    + "; Rationale: "
    + rationale

let appendSigmaBasisItemDecisionLedgerEvent actionScope decision context model =
    match decision with
    | Pending -> model
    | Accepted
    | Rejected
    | Held ->
        model
        |> appendLedgerEvent
            (getSigmaBasisItemDecisionLedgerKind decision)
            context.BasisItem.Key
            (getSigmaBasisItemDecisionLedgerSummary decision)
            (formatSigmaBasisItemLedgerDetail actionScope decision context)

let parsePhiIntoModel (phi: PhiIntake) (model: Model) =
    let beforeSigma =
        model.parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds
        |> buildSigmaContext

    let parse = Engine.parseIntake phi
    let resolution = Engine.resolveParse DemoData.demoSigma parse
    let parsedPhis = upsertParsedPhi parse model.parsedPhis

    let afterSigma =
        parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds
        |> buildSigmaContext

    let alreadyKnownAtoms =
        buildAlreadyKnownAtomGroups parse.PhiId beforeSigma parse

    let lastReplayAction =
        createDeltaSigmaAnalysis
            ("Parsed " + parse.PhiId)
            "This Phi was parsed, so its exposed atoms can enter current Sigma."
            parse.PhiId
            parse.Statement
            alreadyKnownAtoms
            beforeSigma
            afterSigma

    { model with
        selectedPhiId = Some phi.PhiId
        selectedPhiParse = Some parse
        selectedPhiResolution = Some resolution
        parsedPhis = parsedPhis
        lastReplayAction = Some lastReplayAction
        phiBatchParseStatus = None },
    parse

type PhiBatchParseCounts =
    {
        ParsedNew: int
        SkippedAlreadyParsed: int
        SkippedExcluded: int
    }

let emptyPhiBatchParseCounts =
    {
        ParsedNew = 0
        SkippedAlreadyParsed = 0
        SkippedExcluded = 0
    }

let formatPhiBatchParseDetail counts =
    sprintf
        "Parsed %d new Phi; skipped %d already parsed; skipped %d excluded."
        counts.ParsedNew
        counts.SkippedAlreadyParsed
        counts.SkippedExcluded

let formatPhiBatchParseStatus ingestedPhiCount counts =
    if ingestedPhiCount = 0 then
        "No Phi available to parse."
    elif counts.ParsedNew = 0 then
        sprintf
            "No new Phi to parse. Skipped %d already parsed. Excluded %d."
            counts.SkippedAlreadyParsed
            counts.SkippedExcluded
    else
        sprintf
            "Parsed %d new Φ. Skipped %d already parsed. Excluded %d."
            counts.ParsedNew
            counts.SkippedAlreadyParsed
            counts.SkippedExcluded

let getPhiBatchTargetId projectName =
    if String.IsNullOrWhiteSpace(projectName) then
        "PhiBatch"
    else
        projectName

let parseAllIncludedPhis (model: Model) =
    model.ingestedPhis
    |> List.fold
        (fun (workingModel, counts) phi ->
            if isPhiExcluded workingModel.excludedPhiIds phi.PhiId then
                workingModel, { counts with SkippedExcluded = counts.SkippedExcluded + 1 }
            elif workingModel.parsedPhis |> List.exists (fun parse -> parse.PhiId = phi.PhiId) then
                workingModel, { counts with SkippedAlreadyParsed = counts.SkippedAlreadyParsed + 1 }
            else
                let parsedModel, _ = parsePhiIntoModel phi workingModel

                parsedModel, { counts with ParsedNew = counts.ParsedNew + 1 })
        (model, emptyPhiBatchParseCounts)

let decideCandidate candidateId (decision: CandidateDecisionValue) (model: Model) =
    match getCurrentCandidateDeltas model |> List.tryFind (fun candidate -> candidate.CandidateId = candidateId) with
    | Some candidate ->
        let candidateDecision = createCandidateDecision decision candidate

        match decision with
        | Pending ->
            model, Cmd.none
        | Accepted
        | Rejected
        | Held ->
            let eventKind, summary =
                match decision with
                | Accepted -> "CandidateAccepted", "Candidate accepted"
                | Rejected -> "CandidateRejected", "Candidate rejected"
                | Held -> "CandidateHeld", "Candidate held"
                | Pending -> "", ""

            let detail =
                "Candidate type: "
                + candidateDecision.CandidateType
                + "; Target: "
                + candidateDecision.Target
                + "; Rationale: "
                + candidateDecision.Rationale

            { model with candidateDecisions = upsertCandidateDecision candidateDecision model.candidateDecisions }
            |> appendLedgerEvent eventKind candidateDecision.CandidateId summary detail
            |> fun updatedModel -> updatedModel, Cmd.none
    | None ->
        model, Cmd.none

