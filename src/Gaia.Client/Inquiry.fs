module Gaia.Client.Inquiry

open System
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.AppState

let private clean (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private isBlank value =
    String.IsNullOrWhiteSpace(value)

let private equalsText left right =
    String.Equals(clean left, clean right, StringComparison.OrdinalIgnoreCase)

let private containsText needle haystack =
    let needleValue = clean needle
    let haystackValue = if isNull haystack then "" else haystack

    needleValue <> ""
    && haystackValue.IndexOf(needleValue, StringComparison.OrdinalIgnoreCase) >= 0

let private containsAny needles haystack =
    needles
    |> List.exists (fun needle -> containsText needle haystack)

let private stableIdText (value: string) =
    let source =
        if isBlank value then
            "UNSPECIFIED"
        else
            value.Trim().ToUpperInvariant()

    let chars =
        source
        |> Seq.map (fun ch ->
            if Char.IsLetterOrDigit(ch) then
                ch
            else
                '-')
        |> Seq.toArray

    String(chars)

let formatInquiryMode = function
    | ForwardInquiry -> "Forward inquiry"
    | ReverseInquiry -> "Reverse inquiry"

let formatInquiryKind = function
    | Requirement -> "Requirement"
    | Observation -> "Observation"
    | Concern -> "Concern"
    | Proposal -> "Proposal"
    | Constraint -> "Constraint"
    | Question -> "Question"
    | StatusRequest -> "Status request"
    | ExplanationRequest -> "Explanation request"
    | ReviewFinding -> "Review finding"
    | Decision -> "Decision"
    | Evidence -> "Evidence"

let private forwardInquiryMechanism =
    [
        "T1 Phi intake"
        "T2 parse"
        "T3 Sigma context"
        "T4 candidate generation"
        "T5 governance"
        "Ledger history"
    ]

let private reverseInquiryMechanism =
    [
        "Facts Reconstruction"
        "Parsed Phi facts"
        "T4 candidates"
        "T5 governance"
        "Ledger history"
    ]

let private textFromPhiIntake (phi: PhiIntake) =
    [
        phi.TypeText
        phi.Source
        phi.Context
        phi.Trigger
        phi.RawStatement
        phi.Claim
        phi.About
        phi.Condition
        phi.Assumption
        phi.Impact
        phi.UnresolvedSignal
    ]
    |> List.filter (fun value -> not (isBlank value))
    |> String.concat " "

let inferForwardInquiryKind (phi: PhiIntake) =
    let text = textFromPhiIntake phi

    if containsAny [ "evidence"; "test report"; "measurement"; "proof" ] text then
        Evidence
    elif containsAny [ "decision"; "accepted"; "rejected"; "approved"; "declined" ] text then
        Decision
    elif containsAny [ "review"; "finding"; "inspection" ] text then
        ReviewFinding
    elif containsAny [ "status"; "progress"; "update" ] text then
        StatusRequest
    elif containsAny [ "why"; "explain"; "because" ] text then
        ExplanationRequest
    elif containsAny [ "?"; "question"; "can "; "could "; "should " ] text then
        Question
    elif containsAny [ "constraint"; "limit"; "maximum"; "minimum"; "max "; "min "; "must not" ] text then
        Constraint
    elif containsAny [ "requirement"; "shall"; "must"; "needs to"; "need to" ] text then
        Requirement
    elif containsAny [ "concern"; "risk"; "issue"; "problem"; "failure" ] text then
        Concern
    elif containsAny [ "proposal"; "suggest"; "recommend"; "option" ] text then
        Proposal
    else
        Observation

let inquiryFromPhiIntake (phi: PhiIntake) =
    {
        InquiryId = "INQ-FWD-" + stableIdText phi.PhiId
        Mode = ForwardInquiry
        Kind = inferForwardInquiryKind phi
        SourceLabel = "PhiIntake"
        SourceId = phi.PhiId
        Text = phi.RawStatement
        TargetKind = None
        TargetId = None
        UnderlyingMechanism = forwardInquiryMechanism
    }

let inquiryKindForFactsQuestion question =
    if equalsText question factsQuestionWhatStillUnresolved then
        StatusRequest
    elif equalsText question factsQuestionWhatFactsSupportedCandidate
         || equalsText question factsQuestionWhatContextAttachedToPhi then
        Evidence
    elif equalsText question factsQuestionWhatDecisionsFromPhi then
        Decision
    elif equalsText question factsQuestionWhyCandidateAccepted
         || equalsText question factsQuestionWhyCandidateRejected
         || equalsText question factsQuestionWhyHostKnown
         || equalsText question factsQuestionWhatChangedAfterPhiParsed then
        ExplanationRequest
    else
        Question

let inquiryFromFactsReconstructionQuestion question targetKind targetId =
    {
        InquiryId =
            "INQ-REV-"
            + stableIdText question
            + "-"
            + stableIdText targetKind
            + "-"
            + stableIdText targetId
        Mode = ReverseInquiry
        Kind = inquiryKindForFactsQuestion question
        SourceLabel = "FactsReconstruction"
        SourceId =
            if isBlank targetId then
                "Auto-selected target"
            else
                targetId
        Text = question
        TargetKind =
            if isBlank targetKind then
                None
            else
                Some targetKind
        TargetId =
            if isBlank targetId then
                None
            else
                Some targetId
        UnderlyingMechanism = reverseInquiryMechanism
    }

let formatInquiryMechanism inquiry =
    inquiry.UnderlyingMechanism
    |> String.concat " -> "
