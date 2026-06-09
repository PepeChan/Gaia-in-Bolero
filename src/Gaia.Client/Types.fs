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

type ProjectSnapshot =
    {
        SnapshotVersion: string
        SavedAtUtc: string
        ProjectName: string
        PhiIntakes: PhiIntake list
        ParsedPhis: PhiParse list
        ExcludedPhiIds: string list
        CandidateDecisions: CandidateDecision list
        LedgerEvents: LedgerEvent list
    }

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate
