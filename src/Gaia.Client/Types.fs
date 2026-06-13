module Gaia.Client.Types

open System
open Bolero
open Gaia.Core

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Probe

type TopNavigationTab =
    | GaiaProbeTab
    | DetailsTab
    | DemoToolsTab
    | FactsReconstructionTab
    | EvidenceTab
    | PersistenceTab
    | LedgerTab

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

type ProjectSnapshot =
    {
        SnapshotVersion: string
        SavedAtUtc: string
        ProjectName: string
        PhiIntakes: PhiIntake list
        PhiContextEntries: PhiContextEntry list
        ParsedPhis: PhiParse list
        ExcludedPhiIds: string list
        CandidateDecisions: CandidateDecision list
        LedgerEvents: LedgerEvent list
        EvidenceRecords: EvidenceRecord list
    }

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate
