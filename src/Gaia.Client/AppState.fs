module Gaia.Client.AppState

open System
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Persistence
open Gaia.Client.Ledger

/// The Elmish application's model.
type Model =
    {
        page: Page
        activeTopNavigationTab: TopNavigationTab
        error: string option
        projectName: string
        exportJson: string
        importJson: string
        persistenceStatus: string option
        selectedScenarioId: string option
        scenarioResolution: ResolutionView option
        phiDraftRawStatement: string
        phiDraftTriggerContext: string
        phiDraftSource: string
        phiDraftQuickTags: string
        phiDraftConfidence: string
        phiContextSnipDraft: string
        existingPhiContextTargetId: string
        phiContextEntryDraftKind: string
        phiContextEntryDraftValue: string
        phiBatchParseStatus: string option
        cognitionReviewTargetFilter: string
        cognitionReviewDecisionFilter: string
        cognitionReviewTextFilter: string
        ingestedPhis: PhiIntake list
        phiContextEntries: PhiContextEntry list
        parsedPhis: PhiParse list
        excludedPhiIds: string list
        selectedPhiId: string option
        selectedPhiParse: PhiParse option
        selectedPhiResolution: ResolutionView option
        lastReplayAction: DeltaSigmaAnalysis option
        candidateDecisions: CandidateDecision list
        sigmaBasisItemDecisions: Map<string, CandidateDecisionValue>
        LedgerEvents: LedgerEvent list
        ReplayPreviewSequence: int option
        factsReconstructionQuestion: string
        factsReconstructionTargetKind: string
        factsReconstructionTargetId: string
        factsReconstructionDisplayMode: string
        factsReconstructionResult: FactsReconstructionResult option
        evidenceRecords: EvidenceRecord list
        evidenceCaptureKind: string
        evidenceTargetKind: string
        evidenceTargetId: string
        evidenceTitle: string
        evidenceNotes: string
        evidenceContentRef: string
        evidenceStatus: string option
        realizationState: RealizationState
        realizationObjectKindDraft: string
        realizationObjectIdDraft: string
        realizationObjectNameDraft: string
        realizationObjectDescriptionDraft: string
        realizationObjectSourceNoteDraft: string
        realizationLinkKindDraft: string
        realizationLinkSourceIdDraft: string
        realizationLinkTargetIdDraft: string
        realizationStatus: string option
    }

let demoScenarios = DemoData.demoScenarios
let defaultProjectName = "Untitled Project"
let evidenceCaptureKinds = [ "Manual note"; "Screenshot placeholder"; "File reference"; "External observation" ]
let evidenceTargetKinds = [ "Phi"; "Function"; "Mode"; "Interface"; "State"; "Host"; "Constraint" ]
let defaultEvidenceCaptureKind = "Manual note"
let defaultEvidenceTargetKind = "Phi"
let defaultCognitionReviewTargetFilter = "All"
let defaultCognitionReviewDecisionFilter = "All"
let factsQuestionWhyCandidateAccepted = "Why was this candidate accepted?"
let factsQuestionWhyCandidateRejected = "Why was this candidate rejected?"
let factsQuestionWhyHostKnown = "Why is this host known?"
let factsQuestionWhatFactsSupportedCandidate = "What facts supported this candidate?"
let factsQuestionWhatChangedAfterPhiParsed = "What changed after this Phi was parsed?"
let factsQuestionWhatContextAttachedToPhi = "What context was attached to this Phi?"
let factsQuestionWhatDecisionsFromPhi = "What decisions were made from this Phi?"
let factsQuestionWhatStillUnresolved = "What is still unresolved?"
let factsReconstructionQuestions =
    [
        factsQuestionWhyCandidateAccepted
        factsQuestionWhyCandidateRejected
        factsQuestionWhyHostKnown
        factsQuestionWhatFactsSupportedCandidate
        factsQuestionWhatChangedAfterPhiParsed
        factsQuestionWhatContextAttachedToPhi
        factsQuestionWhatDecisionsFromPhi
        factsQuestionWhatStillUnresolved
    ]

let factsTargetKindCandidate = "Candidate"
let factsTargetKindPhi = "Phi"
let factsTargetKindHost = "Host"
let factsTargetKindContextEntry = "Context entry"
let factsTargetKinds =
    [
        factsTargetKindCandidate
        factsTargetKindPhi
        factsTargetKindHost
        factsTargetKindContextEntry
    ]

let defaultFactsReconstructionQuestion = factsQuestionWhyCandidateAccepted
let defaultFactsReconstructionTargetKind = factsTargetKindCandidate
let factsReconstructionDisplayModeCard = "Card"
let factsReconstructionDisplayModeFullReport = "Full report"
let factsReconstructionDisplayModes =
    [
        factsReconstructionDisplayModeCard
        factsReconstructionDisplayModeFullReport
    ]
let defaultFactsReconstructionDisplayMode = factsReconstructionDisplayModeCard

let buildSigmaBasisItemDecisionsFromLedger ledgerEvents =
    ledgerEvents
    |> List.fold
        (fun decisions ledgerEvent ->
            match ledgerEvent.EventKind with
            | "SigmaBasisItemAccepted" ->
                decisions |> Map.add ledgerEvent.TargetId Accepted
            | "SigmaBasisItemRejected" ->
                decisions |> Map.add ledgerEvent.TargetId Rejected
            | "SigmaBasisItemHeld" ->
                decisions |> Map.add ledgerEvent.TargetId Held
            | _ ->
                decisions)
        Map.empty<string, CandidateDecisionValue>

let phiContextEntryKinds =
    [
        "HostHint"
        "InterfaceHint"
        "ModeHint"
        "StateHint"
        "ConstraintHint"
        "Assumption"
        "Concern"
        "RiskHint"
        "AllocationHint"
        "EvidenceRef"
        "Tag"
    ]

let defaultPhiContextEntryKind = "HostHint"

let buildProjectSnapshot (model: Model) =
    {
        SnapshotVersion = projectSnapshotVersion
        SavedAtUtc = getUtcTimestampString ()
        ProjectName = model.projectName
        PhiIntakes = model.ingestedPhis
        PhiContextEntries = model.phiContextEntries
        ParsedPhis = model.parsedPhis
        ExcludedPhiIds = model.excludedPhiIds
        CandidateDecisions = model.candidateDecisions
        LedgerEvents = model.LedgerEvents
        EvidenceRecords = model.evidenceRecords
        RealizationState = model.realizationState
    }

let restoreProjectSnapshot (snapshot: ProjectSnapshot) (model: Model) =
    {
        model with
            projectName =
                if String.IsNullOrWhiteSpace(snapshot.ProjectName) then
                    defaultProjectName
                else
                    snapshot.ProjectName
            ingestedPhis = snapshot.PhiIntakes
            phiContextEntries = snapshot.PhiContextEntries
            parsedPhis = snapshot.ParsedPhis
            excludedPhiIds = snapshot.ExcludedPhiIds
            selectedPhiId = None
            selectedPhiParse = None
            selectedPhiResolution = None
            lastReplayAction = None
            phiBatchParseStatus = None
            cognitionReviewTargetFilter = defaultCognitionReviewTargetFilter
            cognitionReviewDecisionFilter = defaultCognitionReviewDecisionFilter
            cognitionReviewTextFilter = ""
            candidateDecisions = snapshot.CandidateDecisions
            sigmaBasisItemDecisions = buildSigmaBasisItemDecisionsFromLedger snapshot.LedgerEvents
            LedgerEvents = snapshot.LedgerEvents
            ReplayPreviewSequence = None
            factsReconstructionQuestion = defaultFactsReconstructionQuestion
            factsReconstructionTargetKind = defaultFactsReconstructionTargetKind
            factsReconstructionTargetId = ""
            factsReconstructionDisplayMode = defaultFactsReconstructionDisplayMode
            factsReconstructionResult = None
            evidenceRecords = snapshot.EvidenceRecords
            evidenceTargetId = ""
            evidenceTitle = ""
            evidenceNotes = ""
            evidenceContentRef = ""
            evidenceStatus = None
            realizationState = snapshot.RealizationState
            realizationObjectKindDraft = defaultRealizationObjectKind
            realizationObjectIdDraft = ""
            realizationObjectNameDraft = ""
            realizationObjectDescriptionDraft = ""
            realizationObjectSourceNoteDraft = ""
            realizationLinkKindDraft = defaultRealizationLinkKind
            realizationLinkSourceIdDraft = ""
            realizationLinkTargetIdDraft = ""
            realizationStatus = None
            phiContextSnipDraft = ""
            existingPhiContextTargetId = ""
            phiContextEntryDraftKind = defaultPhiContextEntryKind
            phiContextEntryDraftValue = ""
    }

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
        projectName = defaultProjectName
        exportJson = ""
        importJson = ""
        persistenceStatus = None
        selectedScenarioId = selectedScenarioId
        scenarioResolution = scenarioResolution
        phiDraftRawStatement = ""
        phiDraftTriggerContext = ""
        phiDraftSource = ""
        phiDraftQuickTags = ""
        phiDraftConfidence = "Medium"
        phiContextSnipDraft = ""
        existingPhiContextTargetId = ""
        phiContextEntryDraftKind = defaultPhiContextEntryKind
        phiContextEntryDraftValue = ""
        phiBatchParseStatus = None
        cognitionReviewTargetFilter = defaultCognitionReviewTargetFilter
        cognitionReviewDecisionFilter = defaultCognitionReviewDecisionFilter
        cognitionReviewTextFilter = ""
        ingestedPhis = []
        phiContextEntries = []
        parsedPhis = []
        excludedPhiIds = []
        selectedPhiId = None
        selectedPhiParse = None
        selectedPhiResolution = None
        lastReplayAction = None
        candidateDecisions = []
        sigmaBasisItemDecisions = Map.empty
        LedgerEvents = []
        ReplayPreviewSequence = None
        factsReconstructionQuestion = defaultFactsReconstructionQuestion
        factsReconstructionTargetKind = defaultFactsReconstructionTargetKind
        factsReconstructionTargetId = ""
        factsReconstructionDisplayMode = defaultFactsReconstructionDisplayMode
        factsReconstructionResult = None
        evidenceRecords = []
        evidenceCaptureKind = defaultEvidenceCaptureKind
        evidenceTargetKind = defaultEvidenceTargetKind
        evidenceTargetId = ""
        evidenceTitle = ""
        evidenceNotes = ""
        evidenceContentRef = ""
        evidenceStatus = None
        realizationState = emptyRealizationState
        realizationObjectKindDraft = defaultRealizationObjectKind
        realizationObjectIdDraft = ""
        realizationObjectNameDraft = ""
        realizationObjectDescriptionDraft = ""
        realizationObjectSourceNoteDraft = ""
        realizationLinkKindDraft = defaultRealizationLinkKind
        realizationLinkSourceIdDraft = ""
        realizationLinkTargetIdDraft = ""
        realizationStatus = None
    }

let clearProjectModel (model: Model) =
    {
        model with
            projectName = defaultProjectName
            exportJson = ""
            importJson = ""
            persistenceStatus = None
            phiDraftRawStatement = ""
            phiDraftTriggerContext = ""
            phiDraftSource = ""
            phiDraftQuickTags = ""
            phiDraftConfidence = "Medium"
            phiContextSnipDraft = ""
            existingPhiContextTargetId = ""
            phiContextEntryDraftKind = defaultPhiContextEntryKind
            phiContextEntryDraftValue = ""
            phiBatchParseStatus = None
            cognitionReviewTargetFilter = defaultCognitionReviewTargetFilter
            cognitionReviewDecisionFilter = defaultCognitionReviewDecisionFilter
            cognitionReviewTextFilter = ""
            ingestedPhis = []
            phiContextEntries = []
            parsedPhis = []
            excludedPhiIds = []
            selectedPhiId = None
            selectedPhiParse = None
            selectedPhiResolution = None
            lastReplayAction = None
            candidateDecisions = []
            sigmaBasisItemDecisions = Map.empty
            LedgerEvents = []
            ReplayPreviewSequence = None
            factsReconstructionQuestion = defaultFactsReconstructionQuestion
            factsReconstructionTargetKind = defaultFactsReconstructionTargetKind
            factsReconstructionTargetId = ""
            factsReconstructionDisplayMode = defaultFactsReconstructionDisplayMode
            factsReconstructionResult = None
            evidenceRecords = []
            evidenceCaptureKind = defaultEvidenceCaptureKind
            evidenceTargetKind = defaultEvidenceTargetKind
            evidenceTargetId = ""
            evidenceTitle = ""
            evidenceNotes = ""
            evidenceContentRef = ""
            evidenceStatus = None
            realizationState = emptyRealizationState
            realizationObjectKindDraft = defaultRealizationObjectKind
            realizationObjectIdDraft = ""
            realizationObjectNameDraft = ""
            realizationObjectDescriptionDraft = ""
            realizationObjectSourceNoteDraft = ""
            realizationLinkKindDraft = defaultRealizationLinkKind
            realizationLinkSourceIdDraft = ""
            realizationLinkTargetIdDraft = ""
            realizationStatus = None
    }

let buildSphynxSampleSnapshot () =
    let coverGlassContextEntry =
        {
            ContextId = "PHI-SPHYNX-SEED-005-CTX-0001"
            PhiId = "PHI-SPHYNX-SEED-005"
            Kind = "HostHint"
            Value = "Cover Glass"
            Provenance = "OneSecSnip"
        }

    let sampleLedgerEvents =
        []
        |> appendLedgerEventToList
            "PhiContextEntryCreated"
            coverGlassContextEntry.ContextId
            "Phi context entry created"
            ("PhiId: "
             + coverGlassContextEntry.PhiId
             + "; ContextId: "
             + coverGlassContextEntry.ContextId
             + "; Kind: "
             + coverGlassContextEntry.Kind
             + "; Value: "
             + coverGlassContextEntry.Value
             + "; Provenance: "
             + coverGlassContextEntry.Provenance)

    {
        SnapshotVersion = projectSnapshotVersion
        SavedAtUtc = getUtcTimestampString ()
        ProjectName = "Sphynx Sample Project"
        PhiIntakes = DemoData.demoPhiIntakes
        PhiContextEntries = [ coverGlassContextEntry ]
        ParsedPhis = []
        ExcludedPhiIds = []
        CandidateDecisions = []
        LedgerEvents = sampleLedgerEvents
        EvidenceRecords = []
        RealizationState = emptyRealizationState
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
    | SetPhiContextSnipDraft of string
    | SetExistingPhiContextTargetId of string
    | SetPhiContextEntryDraftKind of string
    | SetPhiContextEntryDraftValue of string
    | AddContextEntryToExistingPhi
    | IngestPhiDraft
    | ParseIngestedPhi of string
    | ParseAllIncludedPhi
    | ToggleExcludeParsedPhi of string
    | SetCognitionReviewTargetFilter of string
    | SetCognitionReviewDecisionFilter of string
    | SetCognitionReviewTextFilter of string
    | AcceptCandidate of string
    | RejectCandidate of string
    | HoldCandidate of string
    | SetSigmaBasisItemDecision of string * CandidateDecisionValue
    | SetSigmaBasisItemDecisions of string list * CandidateDecisionValue
    | SelectReplayPreview of int
    | ClearReplayPreview
    | SetFactsReconstructionQuestion of string
    | SetFactsReconstructionTargetKind of string
    | SetFactsReconstructionTargetId of string
    | SetFactsReconstructionDisplayMode of string
    | RunFactsReconstruction
    | SetProjectName of string
    | SaveProjectFile
    | ProjectFileSaved of string
    | ProjectFileSaveFailed of string
    | OpenProjectFile
    | ProjectFileOpened of string
    | ProjectFileOpenCancelled
    | ProjectFileOpenFailed of string
    | ExportProjectJson
    | SetImportJson of string
    | ImportProjectJson
    | LoadSphynxSampleProject
    | ClearProject
    | SetEvidenceCaptureKind of string
    | SetEvidenceTargetKind of string
    | SetEvidenceTargetId of string
    | SetEvidenceTitle of string
    | SetEvidenceNotes of string
    | SetEvidenceContentRef of string
    | CreateEvidenceRecord
    | SetRealizationObjectKindDraft of string
    | SetRealizationObjectIdDraft of string
    | SetRealizationObjectNameDraft of string
    | SetRealizationObjectDescriptionDraft of string
    | SetRealizationObjectSourceNoteDraft of string
    | CreateRealizationObject
    | SetRealizationLinkKindDraft of string
    | SetRealizationLinkSourceIdDraft of string
    | SetRealizationLinkTargetIdDraft of string
    | CreateRealizationLink

let appendLedgerEvent eventKind targetId summary detail (model: Model) =
    { model with
        LedgerEvents = appendLedgerEventToList eventKind targetId summary detail model.LedgerEvents }
