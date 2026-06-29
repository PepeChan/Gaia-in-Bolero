module Gaia.Client.Workflow

open System
open Elmish
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Ledger
open Gaia.Client.AppState

let upsertParsedPhi (parse: PhiParse) (parsedPhis: PhiParse list) =
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

let getSequencedParsedPhis (parsedPhis: PhiParse list) =
    parsedPhis
    |> List.mapi (fun index parse -> index + 1, parse)

let getIncludedSequencedParsedPhis excludedPhiIds (parsedPhis: PhiParse list) =
    parsedPhis
    |> getSequencedParsedPhis
    |> List.filter (fun (_, parse) -> not (isPhiExcluded excludedPhiIds parse.PhiId))

let splitTags (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        []
    else
        value.Split([| ','; ';'; '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun tag -> tag.Trim())
        |> Array.filter (fun tag -> tag <> "")
        |> Array.toList

let private normalizeMarkerText (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        ""
    else
        value.Trim().ToLowerInvariant()

let private hasTag (marker: string) (value: string) =
    value
    |> splitTags
    |> List.exists (fun tag -> normalizeMarkerText tag = normalizeMarkerText marker)

let isDerivedInquiryPhi (phi: PhiIntake) =
    String.Equals(phi.Source, t6RealizationInquirySource, StringComparison.OrdinalIgnoreCase)
    || hasTag derivedInquiryTag phi.TypeText

let buildPhiContext phi phiContextEntries =
    {
        Phi = phi
        ExistingTags = splitTags phi.TypeText
        PhiContextEntries =
            phiContextEntries
            |> List.filter (fun entry -> entry.PhiId = phi.PhiId)
    }

let private normalizeContextKey (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        ""
    else
        value.Trim().ToLowerInvariant().Replace(" ", "").Replace("-", "").Replace("_", "")

let canonicalPhiContextKind (kind: string) =
    match normalizeContextKey kind with
    | "host"
    | "hosthint" -> "HostHint"
    | "interface"
    | "interfacehint" -> "InterfaceHint"
    | "mode"
    | "modehint" -> "ModeHint"
    | "state"
    | "statehint" -> "StateHint"
    | "function"
    | "functionhint" -> "FunctionHint"
    | "constraint"
    | "constrainthint" -> "ConstraintHint"
    | "note" -> "Note"
    | "assumption" -> "Assumption"
    | "concern" -> "Concern"
    | "risk"
    | "riskhint" -> "RiskHint"
    | "allocation"
    | "allocationhint" -> "AllocationHint"
    | "evidence"
    | "evidenceref" -> "EvidenceRef"
    | "tag" -> "Tag"
    | _ ->
        if String.IsNullOrWhiteSpace(kind) then
            "Tag"
        else
            kind.Trim()

let createPhiContextId phiId (sequenceNumber: int) =
    phiId + "-CTX-" + sequenceNumber.ToString("0000")

let createPhiContextEntry contextId phiId kind value provenance =
    {
        ContextId = contextId
        PhiId = phiId
        Kind = canonicalPhiContextKind kind
        Value = value
        Provenance = provenance
    }

let parsePhiContextSnipLines phiId startingSequence provenance (snipText: string) =
    if String.IsNullOrWhiteSpace(snipText) then
        []
    else
        snipText.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun line -> line.Trim())
        |> Array.filter (fun line -> line <> "")
        |> Array.mapi (fun index line ->
            let separatorIndex = line.IndexOf("=")

            let kind, value =
                if separatorIndex > 0 then
                    line.Substring(0, separatorIndex).Trim(), line.Substring(separatorIndex + 1).Trim()
                else
                    "Tag", line.Trim()

            createPhiContextEntry (createPhiContextId phiId (startingSequence + index)) phiId kind value provenance)
        |> Array.filter (fun entry -> not (String.IsNullOrWhiteSpace(entry.Value)))
        |> Array.toList

let createNextPhiContextEntry phiId kind value provenance existingEntries =
    let nextSequence =
        existingEntries
        |> List.filter (fun entry -> entry.PhiId = phiId)
        |> List.length
        |> (+) 1

    createPhiContextEntry (createPhiContextId phiId nextSequence) phiId kind value provenance

let formatPhiContextEntryLedgerDetail (entry: PhiContextEntry) =
    "PhiId: "
    + entry.PhiId
    + "; ContextId: "
    + entry.ContextId
    + "; Kind: "
    + entry.Kind
    + "; Value: "
    + entry.Value
    + "; Provenance: "
    + entry.Provenance

let appendPhiContextEntryLedgerEvent entry model =
    model
    |> appendLedgerEvent
        "PhiContextEntryCreated"
        entry.ContextId
        "Phi context entry created"
        (formatPhiContextEntryLedgerDetail entry)

let appendPhiContextEntryLedgerEvents entries model =
    entries
    |> List.fold (fun workingModel entry -> appendPhiContextEntryLedgerEvent entry workingModel) model

let tryFindContextEntryValue kind (phiContext: PhiContext) =
    phiContext.PhiContextEntries
    |> List.tryFind (fun entry -> canonicalPhiContextKind entry.Kind = kind && not (String.IsNullOrWhiteSpace(entry.Value)))
    |> Option.map (fun entry -> entry.Value.Trim())

let tryFindTagValue kind (phiContext: PhiContext) =
    let normalizedKind = normalizeContextKey kind

    phiContext.ExistingTags
    |> List.tryPick (fun tag ->
        let separatorIndex = tag.IndexOf("=")

        if separatorIndex <= 0 then
            None
        else
            let key = tag.Substring(0, separatorIndex) |> normalizeContextKey
            let value = tag.Substring(separatorIndex + 1).Trim()

            if key = normalizedKind && value <> "" then
                Some value
            else
                None)

let chooseContextAwareValue baseValue contextValue tagValue =
    if not (String.IsNullOrWhiteSpace(baseValue)) then
        let provenance =
            if Option.isSome contextValue || Option.isSome tagValue then
                "Combined"
            else
                "Text"

        baseValue, provenance
    else
        match contextValue, tagValue with
        | Some value, _ -> value, "ContextEntry"
        | None, Some value -> value, "Tag"
        | None, None -> "", "Text"

let parsePhiContext (phiContext: PhiContext) =
    let baseParse = Engine.parseIntake phiContext.Phi
    let isDerivedInquiry = isDerivedInquiryPhi phiContext.Phi

    let hostValue, hostProvenance =
        chooseContextAwareValue
            baseParse.Exposure.HostCandidate
            (tryFindContextEntryValue "HostHint" phiContext)
            (tryFindTagValue "host" phiContext)

    let interfaceValue, interfaceProvenance =
        chooseContextAwareValue
            baseParse.Exposure.Interface
            (tryFindContextEntryValue "InterfaceHint" phiContext)
            (tryFindTagValue "interface" phiContext)

    let modeValue, modeProvenance =
        chooseContextAwareValue
            baseParse.Exposure.Mode
            (tryFindContextEntryValue "ModeHint" phiContext)
            (tryFindTagValue "mode" phiContext)

    let stateValue, stateProvenance =
        chooseContextAwareValue
            baseParse.Exposure.State
            (tryFindContextEntryValue "StateHint" phiContext)
            (tryFindTagValue "state" phiContext)

    let hasConstraintContext =
        phiContext.PhiContextEntries
        |> List.exists (fun entry -> canonicalPhiContextKind entry.Kind = "ConstraintHint" && not (String.IsNullOrWhiteSpace(entry.Value)))

    let provenanceSummary =
        [
            "Function=Text"
            "Host=" + hostProvenance
            "Interface=" + interfaceProvenance
            "Mode=" + modeProvenance
            "State=" + stateProvenance
            if hasConstraintContext then
                "Constraint=ContextEntry"
        ]
        |> String.concat "; "

    { baseParse with
        Exposure =
            { baseParse.Exposure with
                HostCandidate = hostValue
                Interface = interfaceValue
                Mode = modeValue
                State = stateValue }
        ExposureNotes =
            baseParse.ExposureNotes
            + " Context-aware T2 v1 parsed PhiContext. Candidate provenance: "
            + provenanceSummary
            + "."
            + (if isDerivedInquiry then
                   " DerivedInquiry=True."
               else
                   "")
        DeltaConstrain = baseParse.DeltaConstrain || hasConstraintContext
        ContextBounded = baseParse.ContextBounded || not (List.isEmpty phiContext.PhiContextEntries) || not (List.isEmpty phiContext.ExistingTags) }

let private isDerivedInquiryParse (parse: PhiParse) =
    not (isNull parse.ExposureNotes)
    && parse.ExposureNotes.IndexOf("DerivedInquiry=True", StringComparison.OrdinalIgnoreCase) >= 0

let getExposureProvenance atomKind (parse: PhiParse) =
    let marker = atomKind + "="
    let notes = if isNull parse.ExposureNotes then "" else parse.ExposureNotes
    let index = notes.LastIndexOf(marker, StringComparison.OrdinalIgnoreCase)

    if index < 0 then
        "Text"
    else
        let startIndex = index + marker.Length
        let remaining = notes.Substring(startIndex)
        let stopIndexes =
            [ remaining.IndexOf(";"); remaining.IndexOf(".") ]
            |> List.filter (fun stopIndex -> stopIndex >= 0)

        let value =
            match stopIndexes with
            | [] -> remaining
            | indexes -> remaining.Substring(0, List.min indexes)

        let trimmed = value.Trim()

        if trimmed = "" then
            "Text"
        else
            trimmed

let getExposureAtomValue atomKind (parse: PhiParse) =
    match atomKind with
    | "Function" -> parse.Exposure.Function
    | "Mode" -> parse.Exposure.Mode
    | "Interface" -> parse.Exposure.Interface
    | "State" -> parse.Exposure.State
    | "Host" -> parse.Exposure.HostCandidate
    | _ -> ""

let updateExposureAtomValue atomKind value (parse: PhiParse) =
    let exposure =
        match atomKind with
        | "Function" ->
            { parse.Exposure with Function = value }
        | "Mode" ->
            { parse.Exposure with Mode = value }
        | "Interface" ->
            { parse.Exposure with Interface = value }
        | "State" ->
            { parse.Exposure with State = value }
        | "Host" ->
            { parse.Exposure with HostCandidate = value }
        | _ ->
            parse.Exposure

    { parse with Exposure = exposure }

let private combineProvenance values =
    let distinct =
        values
        |> List.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
        |> List.distinct

    match distinct with
    | [] -> "Text"
    | [ value ] -> value
    | values when values |> List.exists (fun value -> value.IndexOf("DerivedInquiry", StringComparison.OrdinalIgnoreCase) >= 0) ->
        String.concat ", " values
    | values when values |> List.contains "Combined" -> "Combined"
    | values when values |> List.contains "Text" && values.Length > 1 -> "Combined"
    | _ -> String.concat ", " distinct

let private addDerivedInquiryProvenance (provenance: string) isDerivedInquiry =
    if isDerivedInquiry then
        combineProvenance [ provenance; "DerivedInquiry" ]
    else
        provenance

let private isDerivedSigmaEntry (entry: SigmaContextEntry) =
    not (String.IsNullOrWhiteSpace(entry.Provenance))
    && entry.Provenance.IndexOf("DerivedInquiry", StringComparison.OrdinalIgnoreCase) >= 0

let private chooseIndependentSupportEntries entries =
    let nonDerivedEntries =
        entries
        |> List.filter (fun entry -> not (isDerivedSigmaEntry entry))

    if List.isEmpty nonDerivedEntries then
        entries |> List.truncate 1
    else
        nonDerivedEntries

let buildSigmaContextEntries atomKind (getValue: PhiParse -> string) (sequencedParsedPhis: (int * PhiParse) list) =
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
                    Provenance = addDerivedInquiryProvenance (getExposureProvenance atomKind parse) (isDerivedInquiryParse parse)
                })
    |> List.groupBy (fun entry -> entry.Value)
    |> List.map (fun (_, entries) ->
        let supportEntries = chooseIndependentSupportEntries entries
        let firstEntry = supportEntries |> List.head

        let supportingPhiIds =
            supportEntries
            |> List.map (fun entry -> entry.SourcePhiId)
            |> List.distinct

        { firstEntry with
            SupportCount = List.length supportingPhiIds
            SupportingPhiIds = supportingPhiIds
            Provenance = supportEntries |> List.map (fun entry -> entry.Provenance) |> combineProvenance })

let buildSigmaContext (sequencedParsedPhis: (int * PhiParse) list) =
    {
        Functions = buildSigmaContextEntries "Function" (fun parse -> parse.Exposure.Function) sequencedParsedPhis
        Modes = buildSigmaContextEntries "Mode" (fun parse -> parse.Exposure.Mode) sequencedParsedPhis
        Interfaces = buildSigmaContextEntries "Interface" (fun parse -> parse.Exposure.Interface) sequencedParsedPhis
        States = buildSigmaContextEntries "State" (fun parse -> parse.Exposure.State) sequencedParsedPhis
        Hosts = buildSigmaContextEntries "Host" (fun parse -> parse.Exposure.HostCandidate) sequencedParsedPhis
        Constraints = []
    }

let buildConstraintContextEntries (contextEntries: PhiContextEntry list) (sequencedParsedPhis: (int * PhiParse) list) =
    let includedPhiIds =
        sequencedParsedPhis
        |> List.map (fun (_, parse) -> parse.PhiId)
        |> Set.ofList

    let parseByPhiId =
        sequencedParsedPhis
        |> List.map (fun (parseSequenceNumber, parse) -> parse.PhiId, (parseSequenceNumber, parse))
        |> Map.ofList

    contextEntries
    |> List.choose (fun entry ->
        if canonicalPhiContextKind entry.Kind <> "ConstraintHint" || String.IsNullOrWhiteSpace(entry.Value) || not (Set.contains entry.PhiId includedPhiIds) then
            None
        else
            parseByPhiId
            |> Map.tryFind entry.PhiId
            |> Option.map (fun (parseSequenceNumber, parse) ->
                let provenance =
                    addDerivedInquiryProvenance "ContextEntry" (isDerivedInquiryParse parse)

                {
                    Value = entry.Value
                    SourcePhiId = entry.PhiId
                    SourcePhiStatement = parse.Statement
                    ParseSequenceNumber = parseSequenceNumber
                    SupportCount = 1
                    SupportingPhiIds = [ entry.PhiId ]
                    Provenance = provenance
                }))
    |> List.groupBy (fun entry -> entry.Value)
    |> List.map (fun (_, entries) ->
        let supportEntries = chooseIndependentSupportEntries entries
        let firstEntry = supportEntries |> List.head
        let supportingPhiIds = supportEntries |> List.map (fun entry -> entry.SourcePhiId) |> List.distinct

        { firstEntry with
            SupportCount = List.length supportingPhiIds
            SupportingPhiIds = supportingPhiIds
            Provenance = supportEntries |> List.map (fun entry -> entry.Provenance) |> combineProvenance })

let buildSigmaContextWithContextEntries (contextEntries: PhiContextEntry list) (sequencedParsedPhis: (int * PhiParse) list) =
    let sigmaContext = buildSigmaContext sequencedParsedPhis

    { sigmaContext with
        Constraints = buildConstraintContextEntries contextEntries sequencedParsedPhis }

let getCurrentSigmaContext (model: Model) =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds
    |> buildSigmaContextWithContextEntries model.phiContextEntries

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

let formatRealizationEvidenceTarget objectKind objectId objectName =
    if String.IsNullOrWhiteSpace(objectName) then
        objectKind + ": " + objectId
    else
        objectKind + ": " + objectId + " - " + objectName

let getEvidenceTargetOptionsForKind targetKind (model: Model) =
    let sigmaContext = getCurrentSigmaContext model
    let state = model.realizationState

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
    | "Constraint" ->
        sigmaContext.Constraints
        |> List.map (fun entry -> entry.Value, formatSigmaEvidenceTarget "Constraint" entry)
    | "FR" ->
        state.Sigma.FRs
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "FR" item.Id item.Name)
    | "DP" ->
        state.Sigma.DPs
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "DP" item.Id item.Name)
    | "TF" ->
        state.Sigma.TFs
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "TF" item.Id item.Name)
    | "CTQ" ->
        state.Sigma.CTQs
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "CTQ" item.Id item.Name)
    | "VV" ->
        state.VVItems
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "VV" item.Id item.Name)
    | "Part" ->
        state.Sigma.Parts
        |> List.map (fun item -> item.Id, formatRealizationEvidenceTarget "Part" item.Id item.Name)
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
        ConstraintAtoms = []
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
        ConstraintAtoms = []
    }

let getAddedAtomValues (beforeEntries: SigmaContextEntry list) (afterEntries: SigmaContextEntry list) =
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
        ConstraintAtoms = getAddedAtomValues beforeSigma.Constraints afterSigma.Constraints
    }

let getAlreadyKnownAtomValues sourcePhiId (beforeEntries: SigmaContextEntry list) atomValues =
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
        ConstraintAtoms = []
    }

let hasDeltaSigmaAtomChanges atomGroups =
    [
        atomGroups.FunctionAtoms
        atomGroups.ModeAtoms
        atomGroups.InterfaceAtoms
        atomGroups.StateAtoms
        atomGroups.HostAtoms
        atomGroups.ConstraintAtoms
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

let buildReplayDeltaSigmaAnalysis phiId wasExcluded beforeSigma afterSigma (parsedPhis: PhiParse list) =
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
    | AddHost -> "AddHost"
    | AddConstraint -> "AddConstraint"
    | ReinforcedSigmaAtom -> "ReinforcedSigmaAtom"
    | NoStructuralChange -> "NoStructuralChange"

let formatCandidateDeltaKind = function
    | AddUnknownRevealMissingHost -> "ADD UNKNOWN / REVEAL MISSING HOST"
    | AddInterface -> "ADD INTERFACE"
    | AddState -> "ADD STATE"
    | AddMode -> "ADD MODE"
    | AddHost -> "ADD HOST"
    | AddConstraint -> "ADD CONSTRAINT"
    | ReinforcedSigmaAtom -> "REINFORCED SIGMA ATOM"
    | NoStructuralChange -> "NO STRUCTURAL CHANGE"

let createCandidateId kind target =
    candidateDeltaKindKey kind + "::" + target

let createCandidateDelta kind target proposedTransition reason relevantSigmaBasis provenance =
    {
        CandidateId = createCandidateId kind target
        Kind = kind
        Target = target
        ProposedTransition = proposedTransition
        Reason = reason
        RelevantSigmaBasis = relevantSigmaBasis
        Provenance = provenance
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
    + ") ["
    + entry.Provenance
    + "]"

let formatSigmaBasisGroup atomKind entries =
    entries
    |> List.map (formatSigmaBasis atomKind)

let summarizeEntryProvenance entries =
    entries
    |> List.map (fun (entry: SigmaContextEntry) -> entry.Provenance)
    |> combineProvenance

let buildReinforcedCandidateDeltas atomKind entries =
    entries
    |> List.filter (fun entry -> entry.SupportCount > 1)
    |> List.map (fun entry ->
        createCandidateDelta
            ReinforcedSigmaAtom
            atomKind
            ("Recognize reinforced " + atomKind + " atom in the current Σ basis.")
            "Multiple Phi support the same reasoning atom."
            [ formatSigmaBasis atomKind entry ]
            entry.Provenance)

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
                        (summarizeEntryProvenance sigmaContext.Functions)

            if not (List.isEmpty sigmaContext.Interfaces) then
                yield
                    createCandidateDelta
                        AddInterface
                        "Interface"
                        "Add exposed Interface atoms as candidate Σ structure."
                        "Interface-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "Interface" sigmaContext.Interfaces)
                        (summarizeEntryProvenance sigmaContext.Interfaces)

            if not (List.isEmpty sigmaContext.States) then
                yield
                    createCandidateDelta
                        AddState
                        "State"
                        "Add exposed State atoms as candidate Σ structure."
                        "State-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "State" sigmaContext.States)
                        (summarizeEntryProvenance sigmaContext.States)

            if not (List.isEmpty sigmaContext.Modes) then
                yield
                    createCandidateDelta
                        AddMode
                        "Mode"
                        "Add exposed Mode atoms as candidate Σ structure."
                        "Mode-relevant atoms were exposed by parsed Phi."
                        (formatSigmaBasisGroup "Mode" sigmaContext.Modes)
                        (summarizeEntryProvenance sigmaContext.Modes)

            if not (List.isEmpty sigmaContext.Hosts) then
                yield
                    createCandidateDelta
                        AddHost
                        "Host"
                        "Add exposed Host atoms as candidate Σ structure."
                        "Host-relevant atoms were exposed by parsed Phi or attached Phi Context."
                        (formatSigmaBasisGroup "Host" sigmaContext.Hosts)
                        (summarizeEntryProvenance sigmaContext.Hosts)

            if not (List.isEmpty sigmaContext.Constraints) then
                yield
                    createCandidateDelta
                        AddConstraint
                        "Constraint"
                        "Add exposed Constraint atoms as candidate Σ structure."
                        "Constraint-relevant context entries were attached to parsed Phi."
                        (formatSigmaBasisGroup "Constraint" sigmaContext.Constraints)
                        (summarizeEntryProvenance sigmaContext.Constraints)

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
                "Text"
        ]
    else
        candidates

let getCurrentCandidateDeltas (model: Model) =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds
    |> buildSigmaContextWithContextEntries model.phiContextEntries
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

type CandidateGroupGovernance =
    {
        ClassDecision: CandidateDecisionValue
        ClassDecisionRecord: CandidateDecision option
        BasisItems: (SigmaBasisItemReview * CandidateDecisionValue) list
        Status: CandidateGroupStatus
        TotalBasisItems: int
        PendingCount: int
        AcceptedCount: int
        RejectedCount: int
        HeldCount: int
        Explanation: string
        ConflictExplanation: string option
    }

let formatCandidateGroupStatus = function
    | GroupPending -> "Pending"
    | GroupAccepted -> "Accepted"
    | GroupRejected -> "Rejected"
    | GroupHeld -> "Held"
    | GroupMixed -> "Mixed"
    | GroupPartiallyAccepted -> "Partially accepted"
    | GroupPartiallyGoverned -> "Partially governed"

let private tryFindStoredCandidateDecision candidateId (candidateDecisions: CandidateDecision list) =
    candidateDecisions
    |> List.tryFind (fun decision -> decision.CandidateId = candidateId)

let private deriveCandidateGroupStatus basisItemDecisions =
    let total = List.length basisItemDecisions
    let count decisionValue =
        basisItemDecisions
        |> List.filter (fun decision -> decision = decisionValue)
        |> List.length

    let pendingCount = count Pending
    let acceptedCount = count Accepted
    let rejectedCount = count Rejected
    let heldCount = count Held

    if total = 0 || pendingCount = total then
        GroupPending
    elif pendingCount > 0 then
        GroupPartiallyGoverned
    elif acceptedCount = total then
        GroupAccepted
    elif rejectedCount = total then
        GroupRejected
    elif heldCount = total then
        GroupHeld
    elif acceptedCount > 0 && (rejectedCount > 0 || heldCount > 0) then
        GroupMixed
    elif rejectedCount > 0 && heldCount > 0 then
        GroupMixed
    else
        GroupMixed

let private pluralizeBasisItem count =
    if count = 1 then
        "basis item"
    else
        "basis items"

let formatCandidateGroupDecisionCounts governance =
    [
        "accepted " + string governance.AcceptedCount
        "rejected " + string governance.RejectedCount
        "held " + string governance.HeldCount
        "pending " + string governance.PendingCount
    ]
    |> String.concat ", "

let private describeCandidateGroupStatus status total pendingCount acceptedCount rejectedCount heldCount =
    match status with
    | GroupPending when total = 0 ->
        "This candidate group is pending because it has no relevant basis items to govern."
    | GroupPending ->
        "This candidate group is pending because no basis item has been accepted, rejected, or held."
    | GroupAccepted ->
        "This candidate group is fully accepted because all "
        + string total
        + " "
        + pluralizeBasisItem total
        + " were accepted."
    | GroupRejected ->
        "This candidate group is fully rejected because all "
        + string total
        + " "
        + pluralizeBasisItem total
        + " were rejected."
    | GroupHeld ->
        "This candidate group is held because all "
        + string total
        + " "
        + pluralizeBasisItem total
        + " were held."
    | GroupMixed ->
        "This candidate group is not fully accepted because "
        + string acceptedCount
        + " "
        + pluralizeBasisItem acceptedCount
        + " were accepted, "
        + string rejectedCount
        + " were rejected, and "
        + string heldCount
        + " were held."
    | GroupPartiallyAccepted ->
        "This candidate group is partially accepted because some, but not all, basis items were accepted."
    | GroupPartiallyGoverned ->
        let decidedCount = acceptedCount + rejectedCount + heldCount

        "This candidate group is partially governed because "
        + string decidedCount
        + " "
        + pluralizeBasisItem decidedCount
        + " have a decision and "
        + string pendingCount
        + " remain pending."

let private classDecisionMatchesGroupStatus classDecision groupStatus =
    match classDecision, groupStatus with
    | Pending, _ -> true
    | Accepted, GroupAccepted -> true
    | Rejected, GroupRejected -> true
    | Held, GroupHeld -> true
    | _ -> false

let describeCandidateGroupClassConflict governance =
    match governance.ClassDecisionRecord with
    | None ->
        None
    | Some _ ->
        if classDecisionMatchesGroupStatus governance.ClassDecision governance.Status then
            None
        else
            Some
                ("Class-level decision was "
                 + formatSigmaBasisItemDecisionValue governance.ClassDecision
                 + ", but the basis-derived status is "
                 + formatCandidateGroupStatus governance.Status
                 + " because one or more basis items do not match the class-level decision.")

let buildCandidateGroupGovernance
    (candidate: CandidateDelta)
    (candidateDecisions: CandidateDecision list)
    (sigmaBasisItemDecisions: Map<string, CandidateDecisionValue>)
    sequencedParsedPhis =
    let classDecisionRecord = tryFindStoredCandidateDecision candidate.CandidateId candidateDecisions
    let classDecision =
        classDecisionRecord
        |> Option.map (fun decision -> decision.Decision)
        |> Option.defaultValue Pending

    let basisItems =
        buildSigmaBasisItemReviews candidate sequencedParsedPhis
        |> List.map (fun basisItem -> basisItem, getSigmaBasisItemDecisionValue basisItem.Key sigmaBasisItemDecisions)

    let basisItemDecisions =
        basisItems
        |> List.map snd

    let count decisionValue =
        basisItemDecisions
        |> List.filter (fun decision -> decision = decisionValue)
        |> List.length

    let pendingCount = count Pending
    let acceptedCount = count Accepted
    let rejectedCount = count Rejected
    let heldCount = count Held
    let totalBasisItems = List.length basisItems
    let status = deriveCandidateGroupStatus basisItemDecisions
    let explanation =
        describeCandidateGroupStatus status totalBasisItems pendingCount acceptedCount rejectedCount heldCount

    let governance =
        {
            ClassDecision = classDecision
            ClassDecisionRecord = classDecisionRecord
            BasisItems = basisItems
            Status = status
            TotalBasisItems = totalBasisItems
            PendingCount = pendingCount
            AcceptedCount = acceptedCount
            RejectedCount = rejectedCount
            HeldCount = heldCount
            Explanation = explanation
            ConflictExplanation = None
        }

    { governance with ConflictExplanation = describeCandidateGroupClassConflict governance }

let isCandidateGroupUnresolvedOrConflicted governance =
    match governance.Status with
    | GroupPending
    | GroupHeld
    | GroupMixed
    | GroupPartiallyAccepted
    | GroupPartiallyGoverned -> true
    | GroupAccepted
    | GroupRejected -> Option.isSome governance.ConflictExplanation

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
    let isDerivedInquiry = isDerivedInquiryPhi phi
    let beforeSigma =
        model.parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds
        |> buildSigmaContextWithContextEntries model.phiContextEntries

    let phiContext = buildPhiContext phi model.phiContextEntries
    let parse = parsePhiContext phiContext
    let resolution = Engine.resolveParse DemoData.demoSigma parse
    let parsedPhis = upsertParsedPhi parse model.parsedPhis

    let afterSigma =
        parsedPhis
        |> getIncludedSequencedParsedPhis model.excludedPhiIds
        |> buildSigmaContextWithContextEntries model.phiContextEntries

    let alreadyKnownAtoms =
        if isDerivedInquiry then
            emptyDeltaSigmaAtomGroups
        else
            buildAlreadyKnownAtomGroups parse.PhiId beforeSigma parse

    let replayReason =
        if isDerivedInquiry then
            "This derived inquiry Phi was parsed; repeated Sigma-derived context is not counted as independent reinforcement."
        else
            "This Phi was parsed, so its exposed atoms can enter current Sigma."

    let lastReplayAction =
        createDeltaSigmaAnalysis
            ("Parsed " + parse.PhiId)
            replayReason
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

