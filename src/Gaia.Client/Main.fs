module Gaia.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Gaia.Core

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Probe

type TopNavigationTab =
    | GaiaProbeTab
    | DetailsTab
    | DemoToolsTab

type SigmaContextEntry =
    {
        Value: string
        SourcePhiId: string
        SourcePhiStatement: string
        ParseSequenceNumber: int
        SupportCount: int
        SupportingPhiIds: string list
    }

type SigmaContext =
    {
        Functions: SigmaContextEntry list
        Modes: SigmaContextEntry list
        Interfaces: SigmaContextEntry list
        States: SigmaContextEntry list
        Hosts: SigmaContextEntry list
    }

type DeltaSigmaAtomGroups =
    {
        FunctionAtoms: string list
        ModeAtoms: string list
        InterfaceAtoms: string list
        StateAtoms: string list
        HostAtoms: string list
    }

type DeltaSigmaAnalysis =
    {
        Action: string
        SourcePhiId: string
        SourceStatement: string
        Reason: string
        AddedAtoms: DeltaSigmaAtomGroups
        RemovedAtoms: DeltaSigmaAtomGroups
        AlreadyKnownAtoms: DeltaSigmaAtomGroups
    }

type CandidateDeltaKind =
    | AddUnknownRevealMissingHost
    | AddInterface
    | AddState
    | AddMode
    | ReinforcedSigmaAtom
    | NoStructuralChange

type CandidateDecisionValue =
    | Pending
    | Accepted
    | Rejected
    | Held

type CandidateDecision =
    {
        CandidateId: string
        CandidateType: string
        Target: string
        Decision: CandidateDecisionValue
        Timestamp: DateTime
        Rationale: string
    }

type CandidateDelta =
    {
        CandidateId: string
        Kind: CandidateDeltaKind
        Target: string
        ProposedTransition: string
        Reason: string
        RelevantSigmaBasis: string list
        Confidence: string
        Status: string
    }

/// The Elmish application's model.
type Model =
    {
        page: Page
        activeTopNavigationTab: TopNavigationTab
        error: string option
        selectedScenarioId: string option
        scenarioResolution: ResolutionView option
        phiDraftRawStatement: string
        phiDraftTriggerContext: string
        phiDraftSource: string
        phiDraftQuickTags: string
        phiDraftConfidence: string
        ingestedPhis: PhiIntake list
        parsedPhis: PhiParse list
        excludedPhiIds: string list
        selectedPhiId: string option
        selectedPhiParse: PhiParse option
        selectedPhiResolution: ResolutionView option
        lastReplayAction: DeltaSigmaAnalysis option
        candidateDecisions: CandidateDecision list
    }

let demoScenarios = DemoData.demoScenarios

let tryFindScenario scenarioId =
    demoScenarios
    |> List.tryFind (fun scenario -> scenario.Id = scenarioId)

let resolveScenario scenarioId =
    tryFindScenario scenarioId
    |> Option.map (fun scenario -> Engine.resolveParse DemoData.demoSigma scenario.Parse)

let initialScenario =
    demoScenarios
    |> List.tryHead

let initModel =
    let selectedScenarioId =
        initialScenario
        |> Option.map (fun scenario -> scenario.Id)

    let scenarioResolution =
        selectedScenarioId
        |> Option.bind resolveScenario

    {
        page = Probe
        activeTopNavigationTab = GaiaProbeTab
        error = None
        selectedScenarioId = selectedScenarioId
        scenarioResolution = scenarioResolution
        phiDraftRawStatement = ""
        phiDraftTriggerContext = ""
        phiDraftSource = ""
        phiDraftQuickTags = ""
        phiDraftConfidence = "Medium"
        ingestedPhis = DemoData.demoPhiIntakes
        parsedPhis = []
        excludedPhiIds = []
        selectedPhiId = None
        selectedPhiParse = None
        selectedPhiResolution = None
        lastReplayAction = None
        candidateDecisions = []
    }
/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SelectTopNavigationTab of TopNavigationTab
    | SelectScenario of string
    | Error of exn
    | ClearError
    | SetPhiDraftRawStatement of string
    | SetPhiDraftTriggerContext of string
    | SetPhiDraftSource of string
    | SetPhiDraftQuickTags of string
    | SetPhiDraftConfidence of string
    | IngestPhiDraft
    | ParseIngestedPhi of string
    | ToggleExcludeParsedPhi of string
    | AcceptCandidate of string
    | RejectCandidate of string
    | HoldCandidate of string

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

let decideCandidate candidateId (decision: CandidateDecisionValue) (model: Model) =
    match getCurrentCandidateDeltas model |> List.tryFind (fun candidate -> candidate.CandidateId = candidateId) with
    | Some candidate ->
        let candidateDecision = createCandidateDecision decision candidate

        { model with candidateDecisions = upsertCandidateDecision candidateDecision model.candidateDecisions }, Cmd.none
    | None ->
        model, Cmd.none

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | SelectTopNavigationTab tab ->
        { model with activeTopNavigationTab = tab }, Cmd.none
    | SelectScenario scenarioId ->
        match tryFindScenario scenarioId with
        | Some scenario ->
            { model with
                selectedScenarioId = Some scenario.Id
                scenarioResolution = Some (Engine.resolveParse DemoData.demoSigma scenario.Parse) }, Cmd.none
        | None ->
            model, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    | SetPhiDraftRawStatement value ->
        { model with phiDraftRawStatement = value }, Cmd.none

    | SetPhiDraftTriggerContext value ->
        { model with phiDraftTriggerContext = value }, Cmd.none

    | SetPhiDraftSource value ->
        { model with phiDraftSource = value }, Cmd.none

    | SetPhiDraftQuickTags value ->
        { model with phiDraftQuickTags = value }, Cmd.none

    | SetPhiDraftConfidence value ->
        { model with phiDraftConfidence = value }, Cmd.none

    | ParseIngestedPhi phiId ->
        match model.ingestedPhis |> List.tryFind (fun phi -> phi.PhiId = phiId) with
        | Some phi ->
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
                lastReplayAction = Some lastReplayAction }, Cmd.none

        | None ->
            model, Cmd.none

    | ToggleExcludeParsedPhi phiId ->
        let beforeSigma =
            model.parsedPhis
            |> getIncludedSequencedParsedPhis model.excludedPhiIds
            |> buildSigmaContext

        let wasExcluded = model.excludedPhiIds |> List.contains phiId

        let excludedPhiIds =
            if wasExcluded then
                model.excludedPhiIds
                |> List.filter (fun excludedPhiId -> excludedPhiId <> phiId)
            else
                phiId :: model.excludedPhiIds

        let afterSigma =
            model.parsedPhis
            |> getIncludedSequencedParsedPhis excludedPhiIds
            |> buildSigmaContext

        let lastReplayAction =
            buildReplayDeltaSigmaAnalysis phiId wasExcluded beforeSigma afterSigma model.parsedPhis

        { model with
            excludedPhiIds = excludedPhiIds
            lastReplayAction = Some lastReplayAction }, Cmd.none

    | AcceptCandidate candidateId ->
        decideCandidate candidateId Accepted model

    | RejectCandidate candidateId ->
        decideCandidate candidateId Rejected model

    | HoldCandidate candidateId ->
        decideCandidate candidateId Held model

    | IngestPhiDraft ->
        let intake =
            {
                PhiId = "PHI-" + DateTime.UtcNow.ToString("yyyyMMdd-HHmmss")
                Date = DateTime.UtcNow.ToString("yyyy-MM-dd")
                Source = model.phiDraftSource
                Context = model.phiDraftTriggerContext
                Confidence = model.phiDraftConfidence
                Status = "Ingested"
                RawStatement = model.phiDraftRawStatement
                Trigger = model.phiDraftTriggerContext
                Claim = ""
                About = ""
                Condition = ""
                Assumption = ""
                TypeText = ""
                Impact = ""
                UnresolvedSignal = ""
            }

        { model with
            ingestedPhis = intake :: model.ingestedPhis
            phiDraftRawStatement = ""
            phiDraftTriggerContext = ""
            phiDraftSource = ""
            phiDraftQuickTags = ""
            phiDraftConfidence = "Medium" }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate

let tryGetSelectedScenario model =
    model.selectedScenarioId
    |> Option.bind tryFindScenario
    |> Option.orElse initialScenario

let getAdmissibilityResult (parse: PhiParse) =
    if parse.OutcomeEscalate then
        Escalate
    elif parse.ResultRejected then
        Reject
    elif parse.OutcomeHold || parse.ResultIndeterminate then
        Hold
    elif parse.ResultValid then
        Admit
    else
        Hold

let formatAdmissibilityResult = function
    | Admit -> "ADMIT"
    | Hold -> "HOLD"
    | Reject -> "REJECT"
    | Escalate -> "ESCALATE"

let admissibilityBadgeClass = function
    | Admit -> "tag is-success is-medium"
    | Hold -> "tag is-warning is-medium"
    | Reject -> "tag is-danger is-medium"
    | Escalate -> "tag is-black is-medium"

let formatDerivationEntry = function
    | Some FromFR -> "From FR"
    | Some FromMode -> "From Mode"
    | Some FromInterface -> "From Interface"
    | Some FromState -> "From State"
    | Some FromParametric -> "From Parametric"
    | Some GammaOnly -> "Gamma Only"
    | None -> "Not resolved"

let mapIdsToNames getId getName items ids =
    ids
    |> List.map (fun id ->
        items
        |> List.tryFind (fun item -> getId item = id)
        |> Option.map getName
        |> Option.defaultValue id)

let renderMatchedGroup title names =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        match names with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No matches"
            }
        | xs ->
            div {
                attr.``class`` "tags"
                forEach xs <| fun name ->
                    span {
                        attr.``class`` "tag is-info is-light"
                        text name
                    }
            }
    }

let renderKnownContextGroup title entries =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        match entries with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No known items yet."
            }
        | items ->
            div {
                forEach items <| fun entry ->
                    div {
                        attr.``class`` "mb-4"
                        p {
                            strong { text ("Value: " + entry.Value + " [" + string entry.SupportCount + "]") }
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-1"
                            text ("SourcePhiId: " + entry.SourcePhiId)
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-1"
                            text ("SourcePhiStatement: " + entry.SourcePhiStatement)
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey"
                            text ("Parse sequence number: " + string entry.ParseSequenceNumber)
                        }
                    }
            }
    }

let renderSigmaSnapshotMetric label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderSigmaSnapshotCounts parsedPhiCount sigmaContext =
    div {
        attr.``class`` "tags are-medium mb-4"
        renderSigmaSnapshotMetric "Included parsed Φ" parsedPhiCount
        renderSigmaSnapshotMetric "Functions" (List.length sigmaContext.Functions)
        renderSigmaSnapshotMetric "Modes" (List.length sigmaContext.Modes)
        renderSigmaSnapshotMetric "Interfaces" (List.length sigmaContext.Interfaces)
        renderSigmaSnapshotMetric "States" (List.length sigmaContext.States)
        renderSigmaSnapshotMetric "Hosts" (List.length sigmaContext.Hosts)
    }

let renderParsedPhiLedgerPanel parsedPhis excludedPhiIds dispatch =
    let sequencedParsedPhis = getSequencedParsedPhis parsedPhis

    let excludedPhiCount =
        sequencedParsedPhis
        |> List.filter (fun (_, parse) -> isPhiExcluded excludedPhiIds parse.PhiId)
        |> List.length

    let totalParsedPhiCount = List.length sequencedParsedPhis
    let includedPhiCount = totalParsedPhiCount - excludedPhiCount

    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Replay Engine Lite"
        }

        h2 {
            attr.``class`` "title is-5"
            text "Parsed Φ Ledger / Replay Control"
        }

        div {
            attr.``class`` "tags are-medium mb-4"
            renderSigmaSnapshotMetric "Total parsed Φ" totalParsedPhiCount
            renderSigmaSnapshotMetric "Included Φ" includedPhiCount
            renderSigmaSnapshotMetric "Excluded Φ" excludedPhiCount
        }

        match sequencedParsedPhis with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "Parse a Φ to make it available for replay control."
            }

        | phis ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-hoverable"

                    thead {
                        tr {
                            th { text "#" }
                            th { text "PhiId" }
                            th { text "Statement" }
                            th { text "Status" }
                            th { text "Replay" }
                        }
                    }

                    tbody {
                        forEach phis <| fun (parseSequenceNumber, parse) ->
                            let isExcluded = isPhiExcluded excludedPhiIds parse.PhiId

                            tr {
                                td { text (string parseSequenceNumber) }
                                td {
                                    code { text parse.PhiId }
                                }
                                td { text parse.Statement }
                                td {
                                    span {
                                        attr.``class`` (
                                            if isExcluded then
                                                "tag is-warning"
                                            else
                                                "tag is-success is-light")
                                        text (
                                            if isExcluded then
                                                "Excluded"
                                            else
                                                "Included")
                                    }
                                }
                                td {
                                    button {
                                        attr.``class`` (
                                            if isExcluded then
                                                "button is-small is-success is-light"
                                            else
                                                "button is-small is-warning is-light")
                                        attr.``type`` "button"
                                        on.click (fun _ -> dispatch (ToggleExcludeParsedPhi parse.PhiId))
                                        text (
                                            if isExcluded then
                                                "Include"
                                            else
                                                "Exclude")
                                    }
                                }
                            }
                    }
                }
            }
    }

let renderDeltaSigmaAtomGroup title atoms =
    div {
        attr.``class`` "mb-3"
        h4 {
            attr.``class`` "title is-6 mb-2"
            text title
        }

        match atoms with
        | [] ->
            p {
                attr.``class`` "has-text-grey is-size-7"
                text "None."
            }
        | values ->
            ul {
                forEach values <| fun value ->
                    li { text value }
            }
    }

let renderDeltaSigmaAtomColumn title (atomGroups: DeltaSigmaAtomGroups) =
    div {
        attr.``class`` "column is-4"

        h3 {
            attr.``class`` "title is-6"
            text title
        }

        renderDeltaSigmaAtomGroup "Functions" atomGroups.FunctionAtoms
        renderDeltaSigmaAtomGroup "Modes" atomGroups.ModeAtoms
        renderDeltaSigmaAtomGroup "Interfaces" atomGroups.InterfaceAtoms
        renderDeltaSigmaAtomGroup "States" atomGroups.StateAtoms
        renderDeltaSigmaAtomGroup "Hosts" atomGroups.HostAtoms
    }

let renderDeltaSigmaAnalysisPanel (lastReplayAction: DeltaSigmaAnalysis option) =
    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Replay Engine Lite"
        }

        h2 {
            attr.``class`` "title is-5"
            text "ΔΣ Analysis — Last Replay Action"
        }

        match lastReplayAction with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "No replay action yet."
            }

        | Some analysis ->
            p {
                strong { text "Action: " }
                text analysis.Action
            }

            p {
                strong { text "Source statement: " }
                text analysis.SourceStatement
            }

            p {
                attr.``class`` "mb-4"
                strong { text "Why: " }
                text analysis.Reason
            }

            if hasDeltaSigmaAnalysisChanges analysis then
                div {
                    attr.``class`` "columns"
                    renderDeltaSigmaAtomColumn "Added atoms" analysis.AddedAtoms
                    renderDeltaSigmaAtomColumn "Removed atoms" analysis.RemovedAtoms
                    renderDeltaSigmaAtomColumn "Already known / reinforced atoms" analysis.AlreadyKnownAtoms
                }
            else
                p {
                    attr.``class`` "has-text-grey"
                    text "No Sigma atoms changed."
                }
    }

let renderCandidateDeltaBasis basis =
    match basis with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text "No relevant Sigma basis."
        }
    | values ->
        ul {
            forEach values <| fun value ->
                li { text value }
        }

let tryFindCandidateDecision candidateId (candidateDecisions: CandidateDecision list) =
    candidateDecisions
    |> List.tryFind (fun decision -> decision.CandidateId = candidateId)

let getCandidateDecisionValue candidateId candidateDecisions =
    tryFindCandidateDecision candidateId candidateDecisions
    |> Option.map (fun decision -> decision.Decision)
    |> Option.defaultValue Pending

let formatCandidateDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let candidateDecisionTagClass = function
    | Pending -> "tag is-light"
    | Accepted -> "tag is-success is-light"
    | Rejected -> "tag is-danger is-light"
    | Held -> "tag is-warning is-light"

let candidateDecisionButtonClass decisionValue activeDecision buttonStyle =
    if decisionValue = activeDecision then
        "button is-small " + buttonStyle
    else
        "button is-small " + buttonStyle + " is-light"

let renderCandidateGovernanceActions (candidate: CandidateDelta) decisionValue dispatch =
    div {
        attr.``class`` "buttons are-small mb-0"
        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Accepted "is-success")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (AcceptCandidate candidate.CandidateId))
            text "Accept"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Rejected "is-danger")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (RejectCandidate candidate.CandidateId))
            text "Reject"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Held "is-warning")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (HoldCandidate candidate.CandidateId))
            text "Hold"
        }
    }

let formatCandidateDecisionTimestamp (timestamp: DateTime) =
    timestamp.ToString("yyyy-MM-dd HH:mm:ss") + " UTC"

let renderCandidateDecisionMetadata (candidateDecision: CandidateDecision option) =
    match candidateDecision with
    | None ->
        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "No T5 decision recorded yet."
        }
    | Some decision ->
        div {
            p {
                attr.``class`` "is-size-7 has-text-grey mb-1"
                strong { text "Decision timestamp: " }
                text (formatCandidateDecisionTimestamp decision.Timestamp)
            }

            p {
                attr.``class`` "is-size-7 has-text-grey"
                strong { text "Rationale: " }
                text decision.Rationale
            }
        }

let renderCandidateDeltaCard (candidate: CandidateDelta) (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDecision = tryFindCandidateDecision candidate.CandidateId candidateDecisions
    let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions

    div {
        attr.``class`` "card mb-4"

        div {
            attr.``class`` "card-content"

            p {
                attr.``class`` "heading"
                text "Candidate type"
            }

            h3 {
                attr.``class`` "title is-6"
                text (formatCandidateDeltaKind candidate.Kind)
            }

            p {
                attr.``class`` "is-size-7 has-text-grey mb-3"
                strong { text "Candidate ID: " }
                code { text candidate.CandidateId }
            }

            div {
                attr.``class`` "columns is-multiline"

                div {
                    attr.``class`` "column is-6"
                    p {
                        strong { text "Target of change: " }
                        text candidate.Target
                    }
                }

                div {
                    attr.``class`` "column is-6"
                    p {
                        strong { text "Confidence: " }
                        text candidate.Confidence
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Proposed transition: " }
                        text candidate.ProposedTransition
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Why this candidate exists: " }
                        text candidate.Reason
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Relevant Sigma basis" }
                    }

                    renderCandidateDeltaBasis candidate.RelevantSigmaBasis
                }

                div {
                    attr.``class`` "column is-12"
                    span {
                        attr.``class`` "tag is-warning is-light"
                        text candidate.Status
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "T5 governance decision: " }
                        span {
                            attr.``class`` (candidateDecisionTagClass decisionValue)
                            text (formatCandidateDecisionValue decisionValue)
                        }
                    }

                    div {
                        attr.``class`` "mt-2 mb-2"
                        renderCandidateGovernanceActions candidate decisionValue dispatch
                    }

                    renderCandidateDecisionMetadata candidateDecision
                }
            }
        }
    }

let renderCandidateDeltaSigmaPanel sigmaContext (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "T4 — Candidate ΔΣ Formulation"
        }

        p {
            attr.``class`` "notification is-info is-light"
            text "T4 formulates candidate changes only. T5 records governance decisions here without Σ promotion."
        }

        forEach candidateDeltas <| fun candidateDelta ->
            renderCandidateDeltaCard candidateDelta candidateDecisions dispatch
    }

let renderSummaryBox title body =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        p {
            text body
        }
    }

let renderSummaryCard title body =
    div {
        attr.``class`` "card mb-4"
        div {
            attr.``class`` "card-content"
            p {
                attr.``class`` "heading"
                text title
            }
            p {
                text body
            }
        }
    }

let renderExecutionPathCard steps =
    div {
        attr.``class`` "card"
        div {
            attr.``class`` "card-content"
            h3 {
                attr.``class`` "title is-6"
                text "Execution path"
            }
            ol {
                forEach steps <| fun step ->
                    li { text step }
            }
        }
    }

let renderExposureChain (parse: PhiParse) =
    let chain =
        [
            "Function", parse.Exposure.Function
            "Mode", parse.Exposure.Mode
            "Interface", parse.Exposure.Interface
            "State", parse.Exposure.State
            "Host", parse.Exposure.HostCandidate
        ]
        |> List.map (fun (label, value) -> label, value, value = "")
        |> List.mapi (fun index step -> index, step)

    let lastIndex = List.length chain - 1

    div {
        attr.``class`` "card mb-4"

        div {
            attr.``class`` "card-content"

            h3 {
                attr.``class`` "title is-6"
                text "Exposure chain"
            }

            div {
                attr.``class`` "is-flex is-align-items-stretch is-flex-wrap-nowrap"

                forEach chain <| fun (index, (label, value, isMissing)) ->
                    div {
                        attr.``class`` "is-flex is-align-items-stretch is-flex-grow-1"

                        div {
                            attr.``class`` "box p-0 mb-0 is-flex-grow-1"

                            div {
                                attr.``class`` "has-background-black-ter has-text-white has-text-weight-semibold px-3 py-2"
                                text label
                            }

                            div {
                                attr.``class`` (
                                    if isMissing then
                                        "has-background-warning-light has-text-warning-dark has-text-weight-semibold px-3 py-3"
                                    else
                                        "has-background-white-ter has-text-dark px-3 py-3")
                                text (
                                    if isMissing then
                                        "Missing"
                                    else
                                        value)
                            }
                        }

                        if index < lastIndex then
                            div {
                                attr.``class`` "is-flex is-align-items-center px-2 has-text-grey has-text-weight-bold"
                                text "->"
                            }
                    }
            }
        }
    }

let renderRelevantSigmaContextPanel sequencedParsedPhis selectedPhiParse selectedPhiResolution =
    let sigmaContext = buildSigmaContext sequencedParsedPhis

    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Current Σ Snapshot"
        }

        renderSigmaSnapshotCounts (List.length sequencedParsedPhis) sigmaContext

        h2 {
            attr.``class`` "title is-5"
            text "T3 — Relevant Σ Context"
        }

        match sequencedParsedPhis with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "Include a parsed Φ to reconstruct relevant Σ context."
            }

        | _ ->
            div {
                attr.``class`` "columns is-multiline"

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Functions" sigmaContext.Functions
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Modes" sigmaContext.Modes
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Interfaces" sigmaContext.Interfaces
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known States" sigmaContext.States
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Hosts" sigmaContext.Hosts
                }
            }

            match selectedPhiParse, selectedPhiResolution with
            | Some parse, Some resolution ->
                let missingContext =
                    [
                        if parse.Exposure.Function <> "" && List.isEmpty resolution.MatchedFRs then
                            yield "Function not found in current Σ."

                        if parse.Exposure.Interface <> "" then
                            yield "Interface parsed; explicit interface storage is not yet modeled in Σ."

                        if parse.Exposure.State <> "" then
                            yield "State parsed; explicit state storage is not yet modeled in Σ."

                        if parse.Exposure.Mode <> "" then
                            yield "Mode parsed; explicit mode storage is not yet modeled in Σ."

                        if parse.Exposure.HostCandidate = "" then
                            yield "Host candidate missing."
                    ]

                div {
                    attr.``class`` "box"
                    h3 {
                        attr.``class`` "title is-6"
                        text "Missing / unresolved context"
                    }

                    match missingContext with
                    | [] ->
                        p {
                            attr.``class`` "has-text-grey"
                            text "No missing or unresolved context."
                        }
                    | messages ->
                        ul {
                            forEach messages <| fun message ->
                                li { text message }
                        }
                }

            | _ ->
                empty()
    }

let renderParseDetailsPanel selectedPhiParse selectedPhiResolution =
    div {
        attr.``class`` "box"
        h2 {
            attr.``class`` "title is-5"
            text "T2: Parse"
        }

        match selectedPhiParse, selectedPhiResolution with
        | Some parse, Some resolution ->
            let admissibility = getAdmissibilityResult parse

            p {
                attr.``class`` "is-size-7 has-text-grey"
                text parse.PhiId
            }

            h3 {
                attr.``class`` "title is-6"
                text "Selected Φ"
            }
            p { text parse.Statement }

            div {
                attr.``class`` "mb-4"
                span {
                    attr.``class`` (admissibilityBadgeClass admissibility)
                    text (formatAdmissibilityResult admissibility)
                }
            }

            renderExposureChain parse

            renderSummaryCard "ΔΣ" resolution.DeltaSigmaSummary
            renderSummaryCard "Γ" resolution.GammaSummary
            renderExecutionPathCard resolution.ExecutionPath

        | _ ->
            p {
                attr.``class`` "has-text-grey"
                text "Select an ingested Φ to prepare a structural parse."
            }
    }

let renderCurrentSigmaSnapshotPanel sequencedParsedPhis sigmaContext =
    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Current Σ Snapshot"
        }

        renderSigmaSnapshotCounts (List.length sequencedParsedPhis) sigmaContext
    }

let getReinforcedAtomCount entries =
    entries
    |> List.filter (fun entry -> entry.SupportCount > 1)
    |> List.length

let getSigmaSummaryRows (sigmaContext: SigmaContext) =
    [
        "Functions", sigmaContext.Functions
        "Modes", sigmaContext.Modes
        "Interfaces", sigmaContext.Interfaces
        "States", sigmaContext.States
        "Hosts", sigmaContext.Hosts
    ]

let hasExposureValue value =
    value <> ""

let hasFunctionExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Function

let hasModeExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Mode

let hasInterfaceExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Interface

let hasStateExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.State

let hasAnyStructuralExposure (parse: PhiParse) =
    hasFunctionExposure parse
    || hasModeExposure parse
    || hasInterfaceExposure parse
    || hasStateExposure parse

let countIncludedParsedPhisWith (predicate: PhiParse -> bool) (sequencedParsedPhis: (int * PhiParse) list) =
    sequencedParsedPhis
    |> List.map snd
    |> List.filter predicate
    |> List.length

let interpretMissingContextCount count =
    if count = 0 then
        "No immediate gap detected."
    elif count <= 2 then
        "Low architectural gap."
    elif count <= 5 then
        "Medium architectural gap."
    else
        "High architectural gap."

let interpretPressure count =
    if count = 0 then
        "None"
    elif count <= 2 then
        "Low"
    elif count <= 5 then
        "Medium"
    else
        "High"

let getMissingContextSummaryRows (sequencedParsedPhis: (int * PhiParse) list) =
    [
        "Hosts",
        countIncludedParsedPhisWith
            (fun parse -> parse.Exposure.HostCandidate = "" && hasAnyStructuralExposure parse)
            sequencedParsedPhis

        "Interfaces",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.Interface = ""
                && hasFunctionExposure parse
                && (hasModeExposure parse || hasStateExposure parse))
            sequencedParsedPhis

        "Modes",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.Mode = ""
                && hasFunctionExposure parse
                && (hasStateExposure parse || hasInterfaceExposure parse))
            sequencedParsedPhis

        "States",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.State = ""
                && hasFunctionExposure parse
                && (hasModeExposure parse || hasInterfaceExposure parse))
            sequencedParsedPhis
    ]

let getArchitecturalPressureRows (sigmaContext: SigmaContext) =
    let hostBasisCount =
        if List.isEmpty sigmaContext.Hosts then
            List.length sigmaContext.Functions + List.length sigmaContext.States
        else
            0

    [
        "Host",
        hostBasisCount,
        "Functions/states exist but no host candidates are present."

        "Interface",
        List.length sigmaContext.Interfaces,
        "Interface atoms are available for boundary reasoning."

        "State",
        List.length sigmaContext.States,
        "State atoms are available for condition and behavior reasoning."

        "Mode",
        List.length sigmaContext.Modes,
        "Mode atoms are available for operational-context reasoning."
    ]

let getReinforcedAtoms (sigmaContext: SigmaContext) =
    getSigmaSummaryRows sigmaContext
    |> List.collect (fun (kind, entries) ->
        entries
        |> List.filter (fun entry -> entry.SupportCount > 1)
        |> List.map (fun entry -> kind, entry))

let countDeltaSigmaAtoms (atomGroups: DeltaSigmaAtomGroups) =
    [
        atomGroups.FunctionAtoms
        atomGroups.ModeAtoms
        atomGroups.InterfaceAtoms
        atomGroups.StateAtoms
        atomGroups.HostAtoms
    ]
    |> List.sumBy (fun atoms -> List.length atoms)

let renderCurrentSigmaSummaryTable sigmaContext =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Current Sigma Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Atom kind" }
                        th { text "Count" }
                        th { text "Reinforced atoms" }
                    }
                }

                tbody {
                    forEach (getSigmaSummaryRows sigmaContext) <| fun (kind, entries) ->
                        tr {
                            td { text kind }
                            td { text (string (List.length entries)) }
                            td { text (string (getReinforcedAtomCount entries)) }
                        }
                }
            }
        }
    }

let renderMissingContextSummaryTable sequencedParsedPhis =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Missing Context Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Missing area" }
                        th { text "Count" }
                        th { text "Interpretation" }
                    }
                }

                tbody {
                    forEach (getMissingContextSummaryRows sequencedParsedPhis) <| fun (missingArea, count) ->
                        tr {
                            td { text missingArea }
                            td { text (string count) }
                            td { text (interpretMissingContextCount count) }
                        }
                }
            }
        }
    }

let renderArchitecturalPressureSummaryTable sigmaContext =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Architectural Pressure Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate target" }
                        th { text "Basis count" }
                        th { text "Pressure" }
                        th { text "Meaning" }
                    }
                }

                tbody {
                    forEach (getArchitecturalPressureRows sigmaContext) <| fun (target, basisCount, meaning) ->
                        tr {
                            td { text target }
                            td { text (string basisCount) }
                            td { text (interpretPressure basisCount) }
                            td { text meaning }
                        }
                }
            }
        }
    }

let renderTopReinforcedAtomsTable sigmaContext =
    let reinforcedAtoms = getReinforcedAtoms sigmaContext

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Top Reinforced Atoms"
        }

        match reinforcedAtoms with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No reinforced atoms yet."
            }
        | atoms ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Kind" }
                            th { text "Atom" }
                            th { text "Support" }
                            th { text "Supporting Phi" }
                        }
                    }

                    tbody {
                        forEach atoms <| fun (kind, entry) ->
                            tr {
                                td { text kind }
                                td { text entry.Value }
                                td { text (string entry.SupportCount) }
                                td { text (String.concat ", " entry.SupportingPhiIds) }
                            }
                    }
                }
            }
    }

let renderT4CandidateSummaryTable sigmaContext =
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "T4 Candidate Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate type" }
                        th { text "Target" }
                        th { text "Basis count" }
                        th { text "Status" }
                    }
                }

                tbody {
                    forEach candidateDeltas <| fun candidate ->
                        tr {
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { text (string (List.length candidate.RelevantSigmaBasis)) }
                            td { text "Candidate only" }
                        }
                }
            }
        }
    }

let countCandidateDecisions decisionValue (candidateDeltas: CandidateDelta list) (candidateDecisions: CandidateDecision list) =
    candidateDeltas
    |> List.sumBy (fun candidate ->
        if getCandidateDecisionValue candidate.CandidateId candidateDecisions = decisionValue then
            1
        else
            0)

let renderCandidateDecisionCount label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderCandidateDecisionTag decisionValue =
    span {
        attr.``class`` (candidateDecisionTagClass decisionValue)
        text (formatCandidateDecisionValue decisionValue)
    }

let renderT5GovernanceSummaryTable sigmaContext (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDeltas = formulateCandidateDeltas sigmaContext
    let pendingCount = countCandidateDecisions Pending candidateDeltas candidateDecisions
    let acceptedCount = countCandidateDecisions Accepted candidateDeltas candidateDecisions
    let rejectedCount = countCandidateDecisions Rejected candidateDeltas candidateDecisions
    let heldCount = countCandidateDecisions Held candidateDeltas candidateDecisions

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "T5 Governance Summary"
        }

        div {
            attr.``class`` "tags mb-3"
            renderCandidateDecisionCount "Pending" pendingCount
            renderCandidateDecisionCount "Accepted" acceptedCount
            renderCandidateDecisionCount "Rejected" rejectedCount
            renderCandidateDecisionCount "Held" heldCount
        }

        div {
            attr.``class`` "table-container mb-2"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate type" }
                        th { text "Target" }
                        th { text "Basis count" }
                        th { text "Decision" }
                        th { text "Action" }
                    }
                }

                tbody {
                    forEach candidateDeltas <| fun candidate ->
                        let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions

                        tr {
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { text (string (List.length candidate.RelevantSigmaBasis)) }
                            td { renderCandidateDecisionTag decisionValue }
                            td { renderCandidateGovernanceActions candidate decisionValue dispatch }
                        }
                }
            }
        }

        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "T5 records governance decisions only. Candidate promotion to Sigma is intentionally not performed here."
        }
    }

let renderLatestDeltaSummaryTable lastReplayAction =
    div {
        h3 {
            attr.``class`` "title is-6"
            text "Latest Delta Summary"
        }

        match lastReplayAction with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "No Sigma-changing action yet."
            }
        | Some analysis ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Last action" }
                            th { text "Added atom count" }
                            th { text "Removed atom count" }
                            th { text "Reinforced atom count" }
                        }
                    }

                    tbody {
                        tr {
                            td { text analysis.Action }
                            td { text (string (countDeltaSigmaAtoms analysis.AddedAtoms)) }
                            td { text (string (countDeltaSigmaAtoms analysis.RemovedAtoms)) }
                            td { text (string (countDeltaSigmaAtoms analysis.AlreadyKnownAtoms)) }
                        }
                    }
                }
            }
    }

let renderT5DecisionHistoryPanel (candidateDecisions: CandidateDecision list) =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "T5 — Decision History"
        }

        match candidateDecisions with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No T5 decisions recorded yet."
            }
        | decisions ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "CandidateId" }
                            th { text "Candidate type" }
                            th { text "Target" }
                            th { text "Decision" }
                            th { text "Timestamp" }
                            th { text "Rationale" }
                        }
                    }

                    tbody {
                        forEach decisions <| fun decision ->
                            tr {
                                td { code { text decision.CandidateId } }
                                td { text decision.CandidateType }
                                td { text decision.Target }
                                td { renderCandidateDecisionTag decision.Decision }
                                td { text (formatCandidateDecisionTimestamp decision.Timestamp) }
                                td { text decision.Rationale }
                            }
                    }
                }
            }
    }

let renderOperationalSummaryTablesPanel sequencedParsedPhis sigmaContext lastReplayAction (candidateDecisions: CandidateDecision list) dispatch =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "Summary Tables"
        }

        renderCurrentSigmaSummaryTable sigmaContext

        renderMissingContextSummaryTable sequencedParsedPhis

        renderArchitecturalPressureSummaryTable sigmaContext

        renderTopReinforcedAtomsTable sigmaContext

        renderT4CandidateSummaryTable sigmaContext

        renderT5GovernanceSummaryTable sigmaContext candidateDecisions dispatch

        renderLatestDeltaSummaryTable lastReplayAction
    }

let renderTopNavigation activeTab dispatch =
    div {
        attr.``class`` "tabs is-toggle mb-5"
        ul {
            li {
                attr.``class`` (
                    if activeTab = GaiaProbeTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab GaiaProbeTab))
                    text "Gaia Probe"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = DetailsTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab DetailsTab))
                    text "Details"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = DemoToolsTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab DemoToolsTab))
                    text "Demo Tools"
                }
            }
        }
    }

let homePage model dispatch =
    match tryGetSelectedScenario model, model.scenarioResolution with
    | Some scenario, Some resolution ->
        let admissibility = getAdmissibilityResult scenario.Parse
        let matchedFrNames = mapIdsToNames (fun (fr: FR) -> fr.Id) (fun fr -> fr.Name) DemoData.demoSigma.FRs resolution.MatchedFRs
        let matchedDpNames = mapIdsToNames (fun (dp: DP) -> dp.Id) (fun dp -> dp.Name) DemoData.demoSigma.DPs resolution.MatchedDPs
        let matchedTfNames = mapIdsToNames (fun (tf: TF) -> tf.Id) (fun tf -> tf.Name) DemoData.demoSigma.TFs resolution.MatchedTFs
        let matchedCtqNames = mapIdsToNames (fun (ctq: CTQ) -> ctq.Id) (fun ctq -> ctq.Name) DemoData.demoSigma.CTQs resolution.MatchedCTQs
        let includedSequencedParsedPhis = getIncludedSequencedParsedPhis model.excludedPhiIds model.parsedPhis
        let currentSigmaContext = buildSigmaContext includedSequencedParsedPhis

        div {
            attr.``class`` "content"
            h1 {
                attr.``class`` "title"
                text "Gaia Probe Dashboard"
            }
            p {
                attr.``class`` "subtitle is-6"
                text "Probe demo scenarios, resolve them through Gaia.Core, and inspect the resulting path and matches."
            }

            renderTopNavigation model.activeTopNavigationTab dispatch

            match model.activeTopNavigationTab with
            | GaiaProbeTab -> div {
                attr.``class`` "mb-6 pb-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Live Gaia Workflow"
                }

                div {
                    attr.``class`` "tags are-medium mb-5"
                    span {
                        attr.``class`` "tag is-link"
                        text "T1"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "Φ Set"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "Replay Control"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "Current Σ Snapshot"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T4 Candidate ΔΣ"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T5 Governance"
                    }
                }

                div {
                    attr.``class`` "columns is-variable is-5"

                    div {
                        attr.``class`` "column is-4"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "T1 — Φ Ingestion"
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Raw statement / observation"
                                }
                                div {
                                    attr.``class`` "control"
                                    textarea {
                                        attr.``class`` "textarea"
                                        attr.placeholder "Write the Φ as provided..."
                                        bind.input.string model.phiDraftRawStatement (fun v -> dispatch (SetPhiDraftRawStatement v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Trigger context"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "Why did this matter?"
                                        bind.input.string model.phiDraftTriggerContext (fun v -> dispatch (SetPhiDraftTriggerContext v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Source"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "User, observation, requirement, review..."
                                        bind.input.string model.phiDraftSource (fun v -> dispatch (SetPhiDraftSource v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Quick tags"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "function, mode, interface, state, unknown..."
                                        bind.input.string model.phiDraftQuickTags (fun v -> dispatch (SetPhiDraftQuickTags v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Confidence"
                                }
                                div {
                                    attr.``class`` "control"
                                    div {
                                        attr.``class`` "select is-fullwidth"
                                        select {
                                            bind.input.string model.phiDraftConfidence (fun v -> dispatch (SetPhiDraftConfidence v))
                                            option { text "High" }
                                            option { text "Medium" }
                                            option { text "Low" }
                                        }
                                    }
                                }
                            }

                            button {
                                attr.``class`` "button is-link is-fullwidth"
                                attr.``type`` "button"
                                on.click (fun _ -> dispatch IngestPhiDraft)
                                text "Ingest Φ"
                            }
                        }

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "Φ Set"
                            }

                            match model.ingestedPhis with
                            | [] ->
                                p {
                                    attr.``class`` "has-text-grey"
                                    text "No Φ ingested yet."
                                }
                            | phis ->
                                div {
                                    attr.``class`` "content"
                                    forEach phis <| fun phi ->
                                        div {
                                            attr.``class`` "box"
                                            p {
                                                strong { text phi.PhiId }
                                            }
                                            p {
                                                text phi.RawStatement
                                            }
                                            p {
                                                attr.``class`` "is-size-7 has-text-grey"
                                                text ("Source: " + phi.Source + " | Confidence: " + phi.Confidence)
                                            }
                                            button {
                                                attr.``class`` "button is-small is-link is-light"
                                                attr.``type`` "button"
                                                on.click (fun _ -> dispatch (ParseIngestedPhi phi.PhiId))
                                                text "Parse Φ"
                                            }
                                        }
                                }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"

                        renderCurrentSigmaSnapshotPanel includedSequencedParsedPhis currentSigmaContext

                        renderOperationalSummaryTablesPanel includedSequencedParsedPhis currentSigmaContext model.lastReplayAction model.candidateDecisions dispatch

                        renderParsedPhiLedgerPanel model.parsedPhis model.excludedPhiIds dispatch
                    }
                }
                }

            | DetailsTab -> div {
                attr.``class`` "mb-6 pb-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Reasoning Details"
                }

                div {
                    attr.``class`` "tags are-medium mb-5"
                    span {
                        attr.``class`` "tag is-link"
                        text "T2 Parse"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "ΔΣ Analysis"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T3 Relevant Σ Context"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T4 Candidate ΔΣ"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T5 Governance"
                    }
                }

                renderParseDetailsPanel model.selectedPhiParse model.selectedPhiResolution

                renderDeltaSigmaAnalysisPanel model.lastReplayAction

                renderRelevantSigmaContextPanel includedSequencedParsedPhis model.selectedPhiParse model.selectedPhiResolution

                renderCandidateDeltaSigmaPanel currentSigmaContext model.candidateDecisions dispatch

                renderT5DecisionHistoryPanel model.candidateDecisions
                }

            | DemoToolsTab -> div {
                attr.``class`` "pt-2"

                h2 {
                    attr.``class`` "title is-4"
                    text "Legacy Examples"
                }

                div {
                    attr.``class`` "columns is-variable is-5"

                    div {
                        attr.``class`` "column is-4"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "Demo Scenarios / Legacy Examples"
                            }
                            div {
                                attr.``class`` "buttons"
                                forEach demoScenarios <| fun candidate ->
                                    button {
                                        attr.``class`` (
                                            if Some candidate.Id = model.selectedScenarioId then
                                                "button is-link is-fullwidth"
                                            else
                                                "button is-fullwidth")
                                        attr.``type`` "button"
                                        on.click (fun _ -> dispatch (SelectScenario candidate.Id))
                                        text candidate.Title
                                    }
                            }
                            p {
                                attr.``class`` "has-text-grey"
                                text scenario.Description
                            }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-4"
                                text "Legacy Scenario Resolution"
                            }
                            p {
                                attr.``class`` "is-size-7 has-text-grey"
                                text scenario.Parse.PhiId
                            }
                            div {
                                attr.``class`` "mb-4"
                                h3 {
                                    attr.``class`` "title is-6"
                                    text "Admissibility Result"
                                }
                                span {
                                    attr.``class`` (admissibilityBadgeClass admissibility)
                                    text (formatAdmissibilityResult admissibility)
                                }
                            }

                            h3 {
                                attr.``class`` "title is-6"
                                text "Φ statement"
                            }
                            p {
                                text scenario.Parse.Statement
                            }
                        }

                        div {
                            attr.``class`` "columns"

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "Selected derivation entry"
                                    (formatDerivationEntry resolution.SelectedEntry)
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "DeltaSigmaSummary"
                                    resolution.DeltaSigmaSummary
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "Delta Candidate"
                                    resolution.DeltaCandidateSummary
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "GammaSummary"
                                    resolution.GammaSummary
                            }
                        }

                        div {
                            attr.``class`` "box"
                            h3 {
                                attr.``class`` "title is-6"
                                text "Execution path"
                            }
                            ol {
                                forEach resolution.ExecutionPath <| fun step ->
                                    li {
                                        text step
                                    }
                            }
                        }

                        div {
                            attr.``class`` "columns is-multiline"

                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched FR names" matchedFrNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched DP names" matchedDpNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched TF names" matchedTfNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched CTQ names" matchedCtqNames
                            }
                        }
                    }
                }
            }
        }
    | _ ->
        div {
            attr.``class`` "notification is-warning"
            text "No demo scenarios are available."
        }

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model Probe "Gaia Probe"            
        })
        .Body(
            cond model.page <| function
            | Probe -> homePage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
