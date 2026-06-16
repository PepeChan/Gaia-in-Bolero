module Gaia.Client.InquiryAnswer

open System
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.FactsReconstruction
open Gaia.Client.Inquiry

type InquiryAnswerFactKind =
    | DirectAnswer
    | Reason
    | Impact
    | OpenDecision
    | Evidence
    | LedgerReference
    | SuggestedAction
    | FollowUpQuestion
    | Status
    | Warning
    | MissingLink
    | RelatedInquiry
    | RelatedTarget

type InquiryAnswerFact =
    {
        FactId: string
        Kind: InquiryAnswerFactKind
        Label: string
        Value: string
        SourceKind: string
        SourceId: string option
        TargetKind: string option
        TargetId: string option
        Confidence: string option
        Rank: int
    }

type InquiryAnswer =
    {
        AnswerId: string
        Inquiry: Inquiry
        Summary: string
        Facts: InquiryAnswerFact list
    }

type InquiryIntentProfile =
    | EvidenceProfile
    | ExplanationProfile
    | StatusProfile
    | DeltaProfile
    | DecisionProfile
    | ContextProfile
    | UnresolvedProfile

type private InquiryAnswerFactDraft =
    {
        Kind: InquiryAnswerFactKind
        Label: string
        Value: string
        SourceKind: string
        SourceId: string option
        TargetKind: string option
        TargetId: string option
        Confidence: string option
    }

let private clean (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private isBlank value =
    String.IsNullOrWhiteSpace(value)

let private asOption value =
    let cleaned = clean value

    if cleaned = "" then
        None
    else
        Some cleaned

let private equalsText left right =
    String.Equals(clean left, clean right, StringComparison.OrdinalIgnoreCase)

let private containsText needle haystack =
    let needleValue = clean needle
    let haystackValue = if isNull haystack then "" else haystack

    needleValue <> ""
    && haystackValue.IndexOf(needleValue, StringComparison.OrdinalIgnoreCase) >= 0

let private startsWithText prefix value =
    (clean value).StartsWith(clean prefix, StringComparison.OrdinalIgnoreCase)

let private stripPrefix prefix value =
    let cleaned = clean value
    let prefixValue = clean prefix

    if cleaned.StartsWith(prefixValue, StringComparison.OrdinalIgnoreCase) then
        cleaned.Substring(prefixValue.Length).Trim()
    else
        cleaned

let private distinctText values =
    values
    |> List.map clean
    |> List.filter (fun value -> value <> "")
    |> List.fold
        (fun collected value ->
            if collected |> List.exists (equalsText value) then
                collected
            else
                collected @ [ value ])
        []

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

let formatInquiryAnswerFactKind (kind: InquiryAnswerFactKind) =
    match kind with
    | DirectAnswer -> "Direct answer"
    | Reason -> "Reason"
    | Impact -> "Impact"
    | OpenDecision -> "Open decision"
    | Evidence -> "Evidence"
    | LedgerReference -> "Ledger reference"
    | SuggestedAction -> "Suggested action"
    | FollowUpQuestion -> "Follow-up question"
    | Status -> "Status"
    | Warning -> "Warning"
    | MissingLink -> "Missing link"
    | RelatedInquiry -> "Related inquiry"
    | RelatedTarget -> "Related target"

let formatInquiryIntentProfile (profile: InquiryIntentProfile) =
    match profile with
    | EvidenceProfile -> "Evidence profile"
    | ExplanationProfile -> "Explanation profile"
    | StatusProfile -> "Status profile"
    | DeltaProfile -> "Delta profile"
    | DecisionProfile -> "Decision profile"
    | ContextProfile -> "Context profile"
    | UnresolvedProfile -> "Unresolved profile"

let private formatDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let private resultTargetKind (result: FactsReconstructionResult) =
    asOption result.TargetKind

let private resultTargetId (result: FactsReconstructionResult) =
    asOption result.TargetId

let private fact
    (kind: InquiryAnswerFactKind)
    (label: string)
    (value: string)
    (sourceKind: string)
    (sourceId: string option)
    (targetKind: string option)
    (targetId: string option)
    (confidence: string option)
    : InquiryAnswerFactDraft =
    {
        Kind = kind
        Label = clean label
        Value = clean value
        SourceKind = clean sourceKind
        SourceId = sourceId |> Option.bind asOption
        TargetKind = targetKind |> Option.bind asOption
        TargetId = targetId |> Option.bind asOption
        Confidence = confidence |> Option.bind asOption
    }

let private resultFact
    (kind: InquiryAnswerFactKind)
    (label: string)
    (value: string)
    (sourceKind: string)
    (sourceId: string option)
    (result: FactsReconstructionResult)
    : InquiryAnswerFactDraft =
    fact kind label value sourceKind sourceId (resultTargetKind result) (resultTargetId result) None

let private sameOption (left: string option) (right: string option) =
    match left, right with
    | None, None -> true
    | Some leftValue, Some rightValue -> equalsText leftValue rightValue
    | _ -> false

let private sameFactDraft (left: InquiryAnswerFactDraft) (right: InquiryAnswerFactDraft) =
    left.Kind = right.Kind
    && equalsText left.Label right.Label
    && equalsText left.Value right.Value
    && equalsText left.SourceKind right.SourceKind
    && sameOption left.SourceId right.SourceId
    && sameOption left.TargetKind right.TargetKind
    && sameOption left.TargetId right.TargetId

let private distinctFactDrafts (drafts: InquiryAnswerFactDraft list) =
    drafts
    |> List.filter (fun draft -> not (isBlank draft.Label) && not (isBlank draft.Value))
    |> List.fold
        (fun collected draft ->
            if collected |> List.exists (sameFactDraft draft) then
                collected
            else
                collected @ [ draft ])
        []

let private rankFacts (answerId: string) (drafts: InquiryAnswerFactDraft list) : InquiryAnswerFact list =
    drafts
    |> distinctFactDrafts
    |> List.mapi (fun index draft ->
        let rank = index + 1

        {
            FactId = answerId + "-FACT-" + rank.ToString("0000")
            Kind = draft.Kind
            Label = draft.Label
            Value = draft.Value
            SourceKind = draft.SourceKind
            SourceId = draft.SourceId
            TargetKind = draft.TargetKind
            TargetId = draft.TargetId
            Confidence = draft.Confidence
            Rank = rank
        })

let private splitPipe value =
    (clean value).Split([| '|' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map clean

let private tryHostName (result: FactsReconstructionResult) =
    if not (isBlank result.TargetId) && not (equalsText result.TargetId "Current project") then
        result.TargetId
    else
        result.FactLines
        |> List.tryPick (fun line ->
            if startsWithText "Host:" line then
                Some (stripPrefix "Host:" line)
            else
                None)
        |> Option.defaultValue result.TargetId

let private hasHostHintContext (result: FactsReconstructionResult) =
    result.ContextEntriesUsed
    |> List.exists (fun entry -> equalsText entry.Kind "HostHint")

let private hasAddHostCandidate (result: FactsReconstructionResult) =
    result.CandidateFacts
    |> List.exists (fun candidate -> candidate.Kind = AddHost)

let private summaryForHostKnown (result: FactsReconstructionResult) =
    let hostName = tryHostName result

    if isBlank hostName then
        result.AnswerSummary
    elif hasHostHintContext result && hasAddHostCandidate result then
        hostName
        + " is known because it was introduced as a HostHint context entry and surfaced as an ADD HOST candidate."
    elif hasHostHintContext result then
        hostName + " is known because it was introduced as a HostHint context entry."
    elif hasAddHostCandidate result then
        hostName + " is known because it surfaced as an ADD HOST candidate."
    else
        result.AnswerSummary

let private summaryForResult (result: FactsReconstructionResult) =
    if equalsText result.Question factsQuestionWhyHostKnown then
        summaryForHostKnown result
    else
        result.AnswerSummary

let formatInquiryAnswerSummary (answer: InquiryAnswer) =
    if not (isBlank answer.Summary) then
        answer.Summary
    else
        answer.Facts
        |> List.tryFind (fun fact -> fact.Kind = DirectAnswer)
        |> Option.map (fun fact -> fact.Value)
        |> Option.defaultValue "No answer summary is available."

let private classifyMissingLine line =
    if containsText "Class-level decision" line
       || containsText "Basis-derived status is" line
       || containsText "conflict" line
       || containsText "not fully" line then
        Warning
    else
        MissingLink

let private genericFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    [
        yield resultFact DirectAnswer "Answer" result.AnswerSummary "FactsReconstructionResult" None result

        yield!
            result.ReasonLines
            |> List.mapi (fun index line ->
                resultFact Reason ("Reason " + string (index + 1)) line "FactsReconstructionReasonLine" None result)

        yield!
            result.RecommendedNextActions
            |> List.mapi (fun index action ->
                resultFact
                    SuggestedAction
                    ("Suggested action " + string (index + 1))
                    action
                    "FactsReconstructionRecommendedAction"
                    None
                    result)

        yield!
            result.FactLines
            |> List.mapi (fun index line ->
                resultFact Evidence ("Supporting fact " + string (index + 1)) line "FactsReconstructionFactLine" None result)

        yield!
            result.SourcePhiIds
            |> distinctText
            |> List.map (fun phiId ->
                fact Evidence "Source Phi" ("Phi " + phiId) "Phi" (Some phiId) (resultTargetKind result) (resultTargetId result) None)

        yield!
            result.SourcePhiTexts
            |> List.map (fun (phiId, phiText) ->
                fact
                    Evidence
                    "Source Phi text"
                    (phiId + ": " + phiText)
                    "Phi"
                    (Some phiId)
                    (resultTargetKind result)
                    (resultTargetId result)
                    None)

        yield!
            result.ContextEntriesUsed
            |> List.map (fun entry ->
                fact
                    Evidence
                    ("Context entry " + entry.Kind)
                    (entry.Value + " [" + entry.Provenance + "]")
                    "PhiContextEntry"
                    (Some entry.ContextId)
                    (Some "Phi")
                    (Some entry.PhiId)
                    None)

        yield!
            result.CandidateFacts
            |> List.map (fun candidate ->
                fact
                    RelatedTarget
                    (formatCandidateDeltaKind candidate.Kind)
                    candidate.Target
                    "CandidateDelta"
                    (Some candidate.CandidateId)
                    (Some candidate.Target)
                    (Some candidate.Target)
                    (Some candidate.Confidence))

        yield!
            result.GovernanceDecisions
            |> List.map (fun decision ->
                fact
                    Status
                    "Governance decision"
                    (decision.CandidateType + " is " + formatDecisionValue decision.Decision)
                    "CandidateDecision"
                    (Some decision.CandidateId)
                    (Some "Candidate")
                    (Some decision.CandidateId)
                    None)

        yield!
            result.RelatedLedgerEvents
            |> List.map (fun ledgerEvent ->
                fact
                    LedgerReference
                    ledgerEvent.EventKind
                    ledgerEvent.Summary
                    "LedgerEvent"
                    (Some ledgerEvent.EventId)
                    (Some "Ledger target")
                    (Some ledgerEvent.TargetId)
                    None)

        yield!
            result.ProvenanceLabels
            |> distinctText
            |> List.map (fun provenance ->
                resultFact Evidence "Provenance" provenance "FactsReconstructionProvenance" None result)

        yield!
            result.MissingOrUnresolvedItems
            |> List.mapi (fun index missing ->
                resultFact
                    (classifyMissingLine missing)
                    ("Missing or unresolved " + string (index + 1))
                    missing
                    "FactsReconstructionMissingItem"
                    None
                    result)
    ]

let private hostKnownFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    if not (equalsText result.Question factsQuestionWhyHostKnown) then
        []
    else
        let hostName = tryHostName result

        [
            if not (isBlank hostName) then
                yield resultFact DirectAnswer "Known host" (hostName + " is currently a known host.") "FactsReconstructionResult" None result

            yield!
                result.ContextEntriesUsed
                |> List.filter (fun entry -> equalsText entry.Kind "HostHint")
                |> List.map (fun entry ->
                    fact
                        Reason
                        "Host context"
                        (entry.Kind + " context entry introduced " + entry.Value + ".")
                        "PhiContextEntry"
                        (Some entry.ContextId)
                        (Some "Host")
                        (Some entry.Value)
                        None)

            yield!
                result.CandidateFacts
                |> List.filter (fun candidate -> candidate.Kind = AddHost)
                |> List.map (fun candidate ->
                    fact
                        Evidence
                        "ADD HOST candidate"
                        (candidate.CandidateId + " surfaced " + candidate.Target + ".")
                        "CandidateDelta"
                        (Some candidate.CandidateId)
                        (Some "Host")
                        (Some candidate.Target)
                        (Some candidate.Confidence))

            yield!
                result.SourcePhiIds
                |> distinctText
                |> List.map (fun phiId ->
                    fact Evidence "Source Phi" ("Phi " + phiId) "Phi" (Some phiId) (Some "Host") (asOption hostName) None)

            if not (isBlank hostName) then
                yield
                    resultFact
                        FollowUpQuestion
                        "Dependent decisions"
                        ("What decisions depend on " + hostName + "?")
                        "InquiryAnswerProjection"
                        None
                        result

                yield
                    resultFact
                        FollowUpQuestion
                        "Supporting evidence"
                        ("What evidence supports " + hostName + "?")
                        "InquiryAnswerProjection"
                        None
                        result
        ]

let private tryParseCandidateGroupStatus line =
    if startsWithText "Candidate group:" line then
        match splitPipe line with
        | candidateLabel :: targetKind :: candidateId :: basisStatus :: classDecision :: _ ->
            let candidateKind = stripPrefix "Candidate group:" candidateLabel
            let status = stripPrefix "basis-derived" basisStatus
            Some (candidateKind, targetKind, candidateId, status, classDecision)
        | candidateLabel :: targetKind :: candidateId :: basisStatus :: [] ->
            let candidateKind = stripPrefix "Candidate group:" candidateLabel
            let status = stripPrefix "basis-derived" basisStatus
            Some (candidateKind, targetKind, candidateId, status, "")
        | _ -> None
    else
        None

let private unresolvedFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    if not (equalsText result.Question factsQuestionWhatStillUnresolved) then
        []
    else
        [
            yield resultFact DirectAnswer "Unresolved status" result.AnswerSummary "FactsReconstructionResult" None result

            yield!
                result.FactLines
                |> List.choose tryParseCandidateGroupStatus
                |> List.collect (fun (candidateKind, candidateTargetKind, candidateId, status, classDecision) ->
                    [
                        yield
                            fact
                                Status
                                candidateKind
                                (candidateKind + " is " + status)
                                "CandidateDelta"
                                (Some candidateId)
                                (Some candidateTargetKind)
                                (Some candidateId)
                                None

                        if not (isBlank classDecision) then
                            yield
                                fact
                                    Reason
                                    (candidateKind + " governance basis")
                                    classDecision
                                    "CandidateDelta"
                                    (Some candidateId)
                                    (Some candidateTargetKind)
                                    (Some candidateId)
                                    None
                    ])

            yield!
                result.FactLines
                |> List.filter (fun line -> containsText "basis item:" line)
                |> List.map (fun line ->
                    resultFact OpenDecision "Open basis item" line "SigmaBasisItemReview" None result)
        ]

let private candidateDecisionFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    if not (equalsText result.Question factsQuestionWhyCandidateAccepted)
       && not (equalsText result.Question factsQuestionWhyCandidateRejected) then
        []
    else
        [
            yield resultFact DirectAnswer "Candidate decision answer" result.AnswerSummary "FactsReconstructionResult" None result

            yield!
                result.FactLines
                |> List.choose (fun line ->
                    if startsWithText "Basis-derived status:" line then
                        Some (resultFact Status "Basis-derived status" (stripPrefix "Basis-derived status:" line) "FactsReconstructionFactLine" None result)
                    elif startsWithText "Class-level decision:" line
                         || startsWithText "No class-level decision recorded" line then
                        Some (resultFact Status "Class-level decision" line "FactsReconstructionFactLine" None result)
                    elif startsWithText "Basis item counts:" line then
                        Some (resultFact Reason "Basis item counts" (stripPrefix "Basis item counts:" line) "FactsReconstructionFactLine" None result)
                    else
                        None)

            yield!
                result.MissingOrUnresolvedItems
                |> List.filter (fun line -> classifyMissingLine line = Warning)
                |> List.map (fun line ->
                    resultFact Warning "Governance consistency" line "FactsReconstructionMissingItem" None result)
        ]

let private phiContextFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    if not (equalsText result.Question factsQuestionWhatContextAttachedToPhi) then
        []
    else
        [
            yield resultFact DirectAnswer "Attached context" result.AnswerSummary "FactsReconstructionResult" None result

            yield!
                result.ContextEntriesUsed
                |> List.map (fun entry ->
                    fact
                        Evidence
                        entry.Kind
                        entry.Value
                        "PhiContextEntry"
                        (Some entry.ContextId)
                        (Some "Phi")
                        (Some entry.PhiId)
                        None)
        ]

let private specializedFacts (result: FactsReconstructionResult) : InquiryAnswerFactDraft list =
    [
        yield! hostKnownFacts result
        yield! unresolvedFacts result
        yield! candidateDecisionFacts result
        yield! phiContextFacts result
    ]

let private answerIdForResult (result: FactsReconstructionResult) =
    "ANS-"
    + stableIdText result.Question
    + "-"
    + stableIdText result.TargetKind
    + "-"
    + stableIdText result.TargetId

let inquiryAnswerFromFactsReconstructionResult (result: FactsReconstructionResult) : InquiryAnswer =
    let inquiry = inquiryFromFactsReconstructionQuestion result.Question result.TargetKind result.TargetId
    let answerId = answerIdForResult result
    let summary = summaryForResult result
    let facts = rankFacts answerId (specializedFacts result @ genericFacts result)

    {
        AnswerId = answerId
        Inquiry = inquiry
        Summary = summary
        Facts = facts
    }

let private factKindOrder : InquiryAnswerFactKind list =
    [
        DirectAnswer
        Status
        Reason
        Impact
        OpenDecision
        SuggestedAction
        Warning
        MissingLink
        Evidence
        LedgerReference
        FollowUpQuestion
        RelatedInquiry
        RelatedTarget
    ]

let groupAnswerFactsByKind (facts: InquiryAnswerFact list) : (InquiryAnswerFactKind * InquiryAnswerFact list) list =
    factKindOrder
    |> List.choose (fun kind ->
        let matching =
            facts
            |> List.filter (fun fact -> fact.Kind = kind)
            |> List.sortBy (fun fact -> fact.Rank)

        if List.isEmpty matching then
            None
        else
            Some (kind, matching))

let inquiryIntentProfileForQuestion (question: string) : InquiryIntentProfile =
    if equalsText question factsQuestionWhatFactsSupportedCandidate then
        EvidenceProfile
    elif equalsText question factsQuestionWhyCandidateAccepted
         || equalsText question factsQuestionWhyCandidateRejected
         || equalsText question factsQuestionWhatDecisionsFromPhi then
        DecisionProfile
    elif equalsText question factsQuestionWhyHostKnown then
        ExplanationProfile
    elif equalsText question factsQuestionWhatChangedAfterPhiParsed then
        DeltaProfile
    elif equalsText question factsQuestionWhatContextAttachedToPhi then
        ContextProfile
    elif equalsText question factsQuestionWhatStillUnresolved then
        UnresolvedProfile
    else
        ExplanationProfile

let inquiryIntentProfileForInquiry (inquiry: Inquiry) : InquiryIntentProfile =
    inquiryIntentProfileForQuestion inquiry.Text

let inquiryIntentProfileForAnswer (answer: InquiryAnswer) : InquiryIntentProfile =
    inquiryIntentProfileForInquiry answer.Inquiry

let private kindPriority (profile: InquiryIntentProfile) (fact: InquiryAnswerFact) =
    match profile, fact.Kind with
    | EvidenceProfile, Evidence -> 0
    | EvidenceProfile, RelatedTarget -> 1
    | EvidenceProfile, LedgerReference -> 2
    | EvidenceProfile, Reason -> 3
    | EvidenceProfile, DirectAnswer -> 4
    | EvidenceProfile, Status -> 12
    | ExplanationProfile, DirectAnswer -> 0
    | ExplanationProfile, Reason -> 1
    | ExplanationProfile, RelatedTarget -> 2
    | ExplanationProfile, Evidence -> 3
    | ExplanationProfile, LedgerReference -> 4
    | StatusProfile, Status -> 0
    | StatusProfile, OpenDecision -> 1
    | StatusProfile, Warning -> 2
    | StatusProfile, MissingLink -> 3
    | StatusProfile, SuggestedAction -> 4
    | DeltaProfile, DirectAnswer -> 0
    | DeltaProfile, RelatedTarget -> 1
    | DeltaProfile, Reason -> 2
    | DeltaProfile, Evidence -> 3
    | DeltaProfile, LedgerReference -> 4
    | DecisionProfile, DirectAnswer -> 0
    | DecisionProfile, Status -> 1
    | DecisionProfile, Reason -> 2
    | DecisionProfile, Warning -> 3
    | DecisionProfile, LedgerReference -> 4
    | DecisionProfile, Evidence -> 5
    | ContextProfile, Evidence when equalsText fact.SourceKind "PhiContextEntry" -> 0
    | ContextProfile, Evidence when containsText "Source Phi" fact.Label -> 1
    | ContextProfile, Evidence when containsText "Provenance" fact.Label -> 2
    | ContextProfile, RelatedTarget -> 3
    | ContextProfile, LedgerReference -> 4
    | UnresolvedProfile, Status -> 0
    | UnresolvedProfile, OpenDecision -> 1
    | UnresolvedProfile, Warning -> 2
    | UnresolvedProfile, MissingLink -> 3
    | UnresolvedProfile, SuggestedAction -> 4
    | _, DirectAnswer -> 20
    | _, Reason -> 21
    | _, Evidence -> 22
    | _, RelatedTarget -> 23
    | _, LedgerReference -> 24
    | _, Status -> 25
    | _, SuggestedAction -> 26
    | _, Warning -> 27
    | _, MissingLink -> 28
    | _, OpenDecision -> 29
    | _, Impact -> 30
    | _, FollowUpQuestion -> 31
    | _, RelatedInquiry -> 32

let private profileTermPriority (profile: InquiryIntentProfile) (fact: InquiryAnswerFact) =
    match profile with
    | DeltaProfile ->
        if containsText "Added" fact.Label || containsText "Added" fact.Value || containsText "changed" fact.Value then
            0
        else
            10
    | ContextProfile ->
        if equalsText fact.SourceKind "PhiContextEntry" then
            0
        elif containsText "Source Phi" fact.Label then
            1
        elif containsText "Provenance" fact.Label then
            2
        else
            10
    | _ -> 10

let private hostOriginPriority (answer: InquiryAnswer) (fact: InquiryAnswerFact) =
    if equalsText answer.Inquiry.Text factsQuestionWhyHostKnown then
        if fact.Kind = DirectAnswer then
            0
        elif equalsText fact.SourceKind "PhiContextEntry"
             || containsText "HostHint" fact.Label
             || containsText "HostHint" fact.Value
             || containsText "context" fact.Label then
            1
        elif equalsText fact.SourceKind "Phi"
             || containsText "Source Phi" fact.Label then
            2
        elif equalsText fact.SourceKind "CandidateDelta"
             || containsText "ADD HOST" fact.Label then
            3
        elif equalsText fact.SourceKind "CandidateDecision"
             || fact.Kind = Status then
            4
        elif fact.Kind = LedgerReference then
            5
        else
            20
    else
        0

let private rerankFacts (facts: InquiryAnswerFact list) : InquiryAnswerFact list =
    facts
    |> List.mapi (fun index fact -> { fact with Rank = index + 1 })

let selectFactsForProfile (profile: InquiryIntentProfile) (facts: InquiryAnswerFact list) : InquiryAnswerFact list =
    facts
    |> List.sortBy (fun fact -> kindPriority profile fact, profileTermPriority profile fact, fact.Rank, fact.FactId)
    |> rerankFacts

let profileInquiryAnswer (answer: InquiryAnswer) : InquiryAnswer =
    let profile = inquiryIntentProfileForAnswer answer

    let facts =
        answer.Facts
        |> List.sortBy (fun fact ->
            hostOriginPriority answer fact,
            kindPriority profile fact,
            profileTermPriority profile fact,
            fact.Rank,
            fact.FactId)
        |> rerankFacts

    { answer with Facts = facts }

let isPrimaryAnswerFactForProfile (profile: InquiryIntentProfile) (fact: InquiryAnswerFact) =
    match profile, fact.Kind with
    | EvidenceProfile, Evidence
    | EvidenceProfile, RelatedTarget
    | EvidenceProfile, LedgerReference
    | EvidenceProfile, Reason -> true
    | ExplanationProfile, DirectAnswer
    | ExplanationProfile, Reason
    | ExplanationProfile, RelatedTarget
    | ExplanationProfile, Evidence
    | ExplanationProfile, LedgerReference -> true
    | StatusProfile, Status
    | StatusProfile, OpenDecision
    | StatusProfile, Warning
    | StatusProfile, MissingLink
    | StatusProfile, SuggestedAction -> true
    | DeltaProfile, DirectAnswer
    | DeltaProfile, RelatedTarget
    | DeltaProfile, Reason
    | DeltaProfile, Evidence -> true
    | DecisionProfile, DirectAnswer
    | DecisionProfile, Status
    | DecisionProfile, Reason
    | DecisionProfile, Warning
    | DecisionProfile, LedgerReference -> true
    | ContextProfile, Evidence when equalsText fact.SourceKind "PhiContextEntry" -> true
    | ContextProfile, Evidence when containsText "Source Phi" fact.Label -> true
    | ContextProfile, Evidence when containsText "Provenance" fact.Label -> true
    | ContextProfile, RelatedTarget -> true
    | UnresolvedProfile, Status
    | UnresolvedProfile, OpenDecision
    | UnresolvedProfile, Warning
    | UnresolvedProfile, MissingLink
    | UnresolvedProfile, SuggestedAction -> true
    | _ -> false

let splitAnswerFactsByProfile (answer: InquiryAnswer) : InquiryAnswerFact list * InquiryAnswerFact list =
    let profile = inquiryIntentProfileForAnswer answer

    answer.Facts
    |> List.partition (isPrimaryAnswerFactForProfile profile)

let buildInquiryAnswer (model: Model) : InquiryAnswer =
    reconstructFacts model
    |> inquiryAnswerFromFactsReconstructionResult
    |> profileInquiryAnswer
