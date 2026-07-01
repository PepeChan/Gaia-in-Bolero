module Gaia.Client.Types

open System
open Bolero
open Gaia.Core

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Probe

type TopNavigationTab =
    | GaiaProbeTab
    | DesignRealizationTab
    | FactsReconstructionTab
    | EvidenceTab
    | PersistenceTab
    | LedgerTab

type InquiryMode =
    | ForwardInquiry
    | ReverseInquiry

type InquiryKind =
    | Requirement
    | Observation
    | Concern
    | Proposal
    | Constraint
    | Question
    | StatusRequest
    | ExplanationRequest
    | ReviewFinding
    | Decision
    | Evidence

type Inquiry =
    {
        InquiryId: string
        Mode: InquiryMode
        Kind: InquiryKind
        SourceLabel: string
        SourceId: string
        Text: string
        TargetKind: string option
        TargetId: string option
        UnderlyingMechanism: string list
    }

type SigmaContextEntry =
    {
        Value: string
        SourcePhiId: string
        SourcePhiStatement: string
        ParseSequenceNumber: int
        SupportCount: int
        SupportingPhiIds: string list
        Provenance: string
    }

type SigmaContext =
    {
        Functions: SigmaContextEntry list
        Modes: SigmaContextEntry list
        Interfaces: SigmaContextEntry list
        States: SigmaContextEntry list
        Hosts: SigmaContextEntry list
        Constraints: SigmaContextEntry list
    }

type DeltaSigmaAtomGroups =
    {
        FunctionAtoms: string list
        ModeAtoms: string list
        InterfaceAtoms: string list
        StateAtoms: string list
        HostAtoms: string list
        ConstraintAtoms: string list
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
    | AddFunction
    | AddInterface
    | AddState
    | AddMode
    | AddHost
    | AddConstraint
    | ReinforcedSigmaAtom
    | NoStructuralChange

type CandidateDecisionValue =
    | Pending
    | Accepted
    | Rejected
    | Held

type CandidateGroupStatus =
    | GroupPending
    | GroupAccepted
    | GroupRejected
    | GroupHeld
    | GroupMixed
    | GroupPartiallyAccepted
    | GroupPartiallyGoverned

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
        Provenance: string
        Confidence: string
        Status: string
    }

type PhiContextEntry =
    {
        ContextId: string
        PhiId: string
        Kind: string
        Value: string
        Provenance: string
    }

type PhiContext =
    {
        Phi: PhiIntake
        ExistingTags: string list
        PhiContextEntries: PhiContextEntry list
    }

type PhiDraftPrefill =
    {
        RawStatement: string
        TriggerContext: string
        Source: string
        QuickTags: string
        Confidence: string
        ContextSnip: string
        StatusMessage: string
    }

type ParseAmendmentDraft =
    {
        SourcePhiId: string
        OriginalAtomKind: string
        OriginalAtomText: string
        SourcePhiStatement: string
        Provenance: string
        ProposedAtomKind: string
        ProposedAtomText: string
        Reason: string
        PreviewRequested: bool
    }

let parsedExposureAtomKinds =
    [
        "Function"
        "Mode"
        "Interface"
        "State"
        "Host"
        "Constraint"
    ]

let formatModelFittingAtomKindLabel = function
    | "Function" -> "Capability"
    | "Mode" -> "Use mode"
    | "Interface" -> "Interaction point"
    | "State" -> "Condition"
    | "Host" -> "System element"
    | "Constraint" -> "Rule / limit"
    | atomKind -> atomKind

let formatModelFittingAtomKindPluralLabel = function
    | "Function" -> "Capabilities"
    | "Mode" -> "Use modes"
    | "Interface" -> "Interaction points"
    | "State" -> "Conditions"
    | "Host" -> "System elements"
    | "Constraint" -> "Rules / limits"
    | atomKind -> atomKind

let t6RealizationInquirySource = "T6 Realization Inquiry"
let derivedInquiryTag = "derived-inquiry"
let derivedInquiryContextKind = "DerivedInquiry"
let t6InquiryTargetContextKind = "T6InquiryTarget"
let t6InquiryGapKeyContextKind = "T6InquiryGapKey"
let t6InquiryQuestionContextKind = "T6InquiryQuestion"
let sigmaBasisItemDecisionResetLedgerKind = "SigmaBasisItemDecisionReset"
let t6RealizationReviewNeededLedgerKind = "T6RealizationReviewNeeded"
let parsedAtomRetiredLedgerKind = "ParsedAtomRetired"
let parsedAtomRetirementUndoneLedgerKind = "ParsedAtomRetirementUndone"
let phiDraftCreatedLedgerKind = "PhiDraftCreated"
let workbenchUndoLedgerKind = "WorkbenchUndoApplied"

type LedgerEvent =
    {
        EventId: string
        SequenceNumber: int
        TimestampUtc: string
        Actor: string
        EventKind: string
        TargetId: string
        Summary: string
        Detail: string
    }

type FactsReconstructionResult =
    {
        Question: string
        TargetKind: string
        TargetId: string
        AnswerSummary: string
        ReasonLines: string list
        RecommendedNextActions: string list
        FactLines: string list
        SourcePhiIds: string list
        SourcePhiTexts: (string * string) list
        ContextEntriesUsed: PhiContextEntry list
        CandidateFacts: CandidateDelta list
        GovernanceDecisions: CandidateDecision list
        RelatedLedgerEvents: LedgerEvent list
        ProvenanceLabels: string list
        MissingOrUnresolvedItems: string list
    }

type ReplayPreviewState =
    {
        ParsedPhiEvents: int
        IncludedPhiCount: int
        ExcludedPhiCount: int
        GovernanceAccepted: int
        GovernanceRejected: int
        GovernanceHeld: int
        TotalLedgerEvents: int
    }

type EvidenceRecord =
    {
        EvidenceId: string
        TimestampUtc: string
        Actor: string
        CaptureKind: string
        TargetKind: string
        TargetId: string
        TargetLabel: string
        Title: string
        Notes: string
        ContentRef: string
    }

type PhiDraftFormSnapshot =
    {
        ActiveTopNavigationTab: TopNavigationTab
        PhiDraftStatus: string option
        PhiDraftInputClass: string
        PhiDraftActor: string
        PhiDraftMission: string
        PhiDraftOperationalContext: string
        PhiDraftRawStatement: string
        PhiDraftTriggerContext: string
        PhiDraftSource: string
        PhiDraftQuickTags: string
        PhiDraftConfidence: string
        PhiContextSnipDraft: string
    }

type BasisDecisionUndoSnapshot =
    {
        BasisItemKey: string
        PreviousDecision: CandidateDecisionValue option
    }

type EvidenceRecordUndoSnapshot =
    {
        EvidenceRecord: EvidenceRecord
        PreviousStaleParsedPhiIds: string list
        PreviousPhiBatchParseStatus: string option
    }

type PhiContextEntryUndoSnapshot =
    {
        ContextEntry: PhiContextEntry
        PreviousStaleParsedPhiIds: string list
        PreviousPhiBatchParseStatus: string option
    }

type WorkbenchUndoAction =
    | UndoParsedAtomRetirement of atomKey: string * sourcePhiId: string * atomKind: string * atomText: string * previousCandidateDecisions: CandidateDecision list * previousBasisDecisions: BasisDecisionUndoSnapshot list
    | UndoParseAmendment of sourcePhiId: string * previousParse: PhiParse * previousBasisDecisions: BasisDecisionUndoSnapshot list
    | UndoEvidenceRecordCreation of EvidenceRecordUndoSnapshot
    | UndoPhiContextEntryCreation of PhiContextEntryUndoSnapshot
    | UndoCandidateDecision of candidateId: string * previousDecision: CandidateDecision option
    | UndoSigmaBasisItemDecision of BasisDecisionUndoSnapshot
    | UndoSigmaBasisItemBulkDecision of BasisDecisionUndoSnapshot list
    | UndoModelFittingDraftCreation of previousDraft: PhiDraftFormSnapshot * targetId: string

type RealizationObjectNote =
    {
        ObjectKind: string
        ObjectId: string
        Description: string
        SourceNote: string
    }

type RealizationState =
    {
        Sigma: Sigma
        VVItems: VVItem list
        ObjectNotes: RealizationObjectNote list
        Host_to_Part: (string * string) list
        Function_to_FR: (string * string) list
        CTQ_to_VV: (string * string) list
    }

let emptyRealizationSigma : Sigma =
    {
        FRs = []
        DPs = []
        TFs = []
        CTQs = []
        Parts = []
        FR_to_DP = []
        DP_to_TF = []
        TF_to_CTQ = []
        DP_to_Part = []
        FR_to_CtQ = []
    }

let emptyRealizationState : RealizationState =
    {
        Sigma = emptyRealizationSigma
        VVItems = []
        ObjectNotes = []
        Host_to_Part = []
        Function_to_FR = []
        CTQ_to_VV = []
    }

let realizationObjectKindFR = "FR"
let realizationObjectKindDP = "DP"
let realizationObjectKindTF = "TF"
let realizationObjectKindCTQ = "CTQ"
let realizationObjectKindPart = "Part"
let realizationObjectKindVV = "VV item"

let realizationObjectKinds =
    [
        realizationObjectKindFR
        realizationObjectKindDP
        realizationObjectKindTF
        realizationObjectKindCTQ
        realizationObjectKindPart
        realizationObjectKindVV
    ]

let defaultRealizationObjectKind = realizationObjectKindFR

let realizationLinkKindHostToPart = "Host -> Part"
let realizationLinkKindPartToDP = "Part -> DP"
let realizationLinkKindFunctionToFR = "Function -> FR"
let realizationLinkKindFRToDP = "FR -> DP"
let realizationLinkKindDPToTF = "DP -> TF"
let realizationLinkKindTFToCTQ = "TF -> CTQ"
let realizationLinkKindCTQToVV = "CTQ -> VV"
let realizationLinkKindDPToPart = "DP -> Part"

let realizationLinkKinds =
    [
        realizationLinkKindHostToPart
        realizationLinkKindPartToDP
        realizationLinkKindDPToTF
        realizationLinkKindTFToCTQ
        realizationLinkKindCTQToVV
        realizationLinkKindFunctionToFR
        realizationLinkKindFRToDP
    ]

let defaultRealizationLinkKind = realizationLinkKindHostToPart

let realizationSourceKindHost = "Host"
let realizationSourceKindFunction = "Function"

let realizationNavigationOperatorUpstream = "Upstream"
let realizationNavigationOperatorDownstream = "Downstream"
let realizationNavigationOperatorTopology = "Topology"
let realizationNavigationOperatorCompleteness = "Completeness"

let realizationNavigationOperators =
    [
        realizationNavigationOperatorUpstream
        realizationNavigationOperatorDownstream
        realizationNavigationOperatorTopology
        realizationNavigationOperatorCompleteness
    ]

let defaultRealizationNavigationOperator = realizationNavigationOperatorUpstream

let private normalizeCognopySemanticKind (value: string) =
    if isNull value then
        ""
    else
        value.Trim().ToUpperInvariant()

let private tryGetCognopySemanticSuffix objectKind =
    match normalizeCognopySemanticKind objectKind with
    | "FR"
    | "FUNCTIONAL REQUIREMENT" -> Some "fr"
    | "DP"
    | "DESIGN PARAMETER" -> Some "dp"
    | "TF"
    | "TRANSFER FUNCTION" -> Some "tf"
    | "CTQ" -> Some "ctq"
    | "PART" -> Some "part"
    | "VV"
    | "VV ITEM"
    | "VERIFICATION"
    | "VERIFICATION VALIDATION" -> Some "vv"
    | "DECISION"
    | "OPEN DECISION"
    | "GOVERNANCE DECISION"
    | "CANDIDATE DECISION"
    | "CANDIDATEDECISION" -> Some "decision"
    | "EVIDENCE"
    | "EVIDENCE REF"
    | "EVIDENCEREF" -> Some "evidence"
    | _ -> None

let tryGetCognopyObjectClass objectKind =
    tryGetCognopySemanticSuffix objectKind
    |> Option.map (fun suffix -> "cognopy-object-kind cognopy-object-" + suffix)

let tryGetCognopyObjectRowClass objectKind =
    tryGetCognopySemanticSuffix objectKind
    |> Option.map (fun suffix -> "cognopy-object-row cognopy-object-row-" + suffix)

type ProjectSnapshot =
    {
        SnapshotVersion: string
        SavedAtUtc: string
        ProjectName: string
        PhiIntakes: PhiIntake list
        PhiContextEntries: PhiContextEntry list
        ParsedPhis: PhiParse list
        StaleParsedPhiIds: string list
        ExcludedPhiIds: string list
        CandidateDecisions: CandidateDecision list
        LedgerEvents: LedgerEvent list
        EvidenceRecords: EvidenceRecord list
        RealizationState: RealizationState
    }

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate
