namespace Gaia.Core

open System

type FR = { Id: string; Name: string }
type DP = { Id: string; Name: string }
type TF = { Id: string; Name: string }
type CTQ = { Id: string; Name: string }
type Part = { Id: string; Name: string }

type Sigma =
    {
        FRs: FR list
        DPs: DP list
        TFs: TF list
        CTQs: CTQ list
        Parts: Part list
        FR_to_DP: (string * string) list
        DP_to_TF: (string * string) list
        TF_to_CTQ: (string * string) list
        DP_to_Part: (string * string) list
        FR_to_CtQ: (string * string) list
    }

type CommitDecision =
    | Accept
    | Reject
    | Hold

type SystemSnapshot =
    {
        SnapshotId: string
        ParentSnapshotId: string option
        Sigma: Sigma
        Summary: string
        CreatedAtUtc: DateTime
    }

type GammaAtom =
    {
        GammaId: string
        PhiId: string
        SnapshotId: string
        Kind: string
        Summary: string
        EvidenceRefs: string list
        TimestampUtc: DateTime
    }

type DerivationEntry =
    | FromFR
    | FromMode
    | FromInterface
    | FromState
    | FromParametric
    | GammaOnly

type Exposure =
    {
        Function: string
        Mode: string
        Interface: string
        State: string
        HostCandidate: string
    }

type PhiIntake =
    {
        PhiId: string
        Date: string
        Source: string
        Context: string
        Confidence: string
        Status: string
        RawStatement: string
        Trigger: string
        Claim: string
        About: string
        Condition: string
        Assumption: string
        TypeText: string
        Impact: string
        UnresolvedSignal: string
    }

type PhiParse =
    {
        PhiId: string
        Date: string
        Statement: string
        InScope: string
        OutOfScope: string
        Exposure: Exposure
        ExposureNotes: string
        DeltaAdd: bool
        DeltaRemove: bool
        DeltaConstrain: bool
        DeltaSplit: bool
        DeltaRevealMissing: bool
        DeltaNotes: string
        GammaInconsistencyFlagged: bool
        GammaEvidenceNeeded: bool
        GammaHypothesisLogged: bool
        GammaDetails: string
        Falsifiable: bool
        Traceable: bool
        PhaseCorrect: bool
        ContextBounded: bool
        ResultValid: bool
        ResultIndeterminate: bool
        ResultRejected: bool
        FormalNoFormalization: bool
        OutcomeUpdateSigma: bool
        OutcomeRecordGamma: bool
        OutcomeEscalate: bool
        OutcomeHold: bool
        DerivationEntry: DerivationEntry option
    }

type ResolutionView =
    {
        SelectedEntry: DerivationEntry option
        ExecutionPath: string list
        DeltaSigmaSummary: string
        GammaSummary: string
        MatchedFRs: string list
        MatchedDPs: string list
        MatchedTFs: string list
        MatchedCTQs: string list
    }