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
        evidenceRecords: EvidenceRecord list
        evidenceCaptureKind: string
        evidenceTargetKind: string
        evidenceTargetId: string
        evidenceTitle: string
        evidenceNotes: string
        evidenceContentRef: string
        evidenceStatus: string option
    }

let demoScenarios = DemoData.demoScenarios
let defaultProjectName = "Untitled Project"
let evidenceCaptureKinds = [ "Manual note"; "Screenshot placeholder"; "File reference"; "External observation" ]
let evidenceTargetKinds = [ "Phi"; "Function"; "Mode"; "Interface"; "State"; "Host"; "Constraint" ]
let defaultEvidenceCaptureKind = "Manual note"
let defaultEvidenceTargetKind = "Phi"
let defaultCognitionReviewTargetFilter = "All"
let defaultCognitionReviewDecisionFilter = "All"
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
            sigmaBasisItemDecisions = Map.empty
            LedgerEvents = snapshot.LedgerEvents
            ReplayPreviewSequence = None
            evidenceRecords = snapshot.EvidenceRecords
            evidenceTargetId = ""
            evidenceTitle = ""
            evidenceNotes = ""
            evidenceContentRef = ""
            evidenceStatus = None
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
        evidenceRecords = []
        evidenceCaptureKind = defaultEvidenceCaptureKind
        evidenceTargetKind = defaultEvidenceTargetKind
        evidenceTargetId = ""
        evidenceTitle = ""
        evidenceNotes = ""
        evidenceContentRef = ""
        evidenceStatus = None
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
            evidenceRecords = []
            evidenceCaptureKind = defaultEvidenceCaptureKind
            evidenceTargetKind = defaultEvidenceTargetKind
            evidenceTargetId = ""
            evidenceTitle = ""
            evidenceNotes = ""
            evidenceContentRef = ""
            evidenceStatus = None
    }

let buildSphynxSampleSnapshot () =
    {
        SnapshotVersion = projectSnapshotVersion
        SavedAtUtc = getUtcTimestampString ()
        ProjectName = "Sphynx Sample Project"
        PhiIntakes = DemoData.demoPhiIntakes
        PhiContextEntries = []
        ParsedPhis = []
        ExcludedPhiIds = []
        CandidateDecisions = []
        LedgerEvents = []
        EvidenceRecords = []
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

let appendLedgerEvent eventKind targetId summary detail (model: Model) =
    { model with
        LedgerEvents = appendLedgerEventToList eventKind targetId summary detail model.LedgerEvents }
