module Gaia.Client.InquiryAnswerProjection

open System
open Gaia.Client.Types
open Gaia.Client.InquiryAnswer

type InquiryAnswerCardProjection =
    {
        Question: string
        TargetLabel: string
        Summary: string
        MaturityLabel: string
        GovernanceState: string
        PrimaryReason: string
        RecommendedNextStep: string option
        EvidenceStatus: string
        LedgerStatus: string
        PrimaryFactCount: int
        AdditionalFactCount: int
        SourcePhiCount: int
        LedgerEventCount: int
    }

let private formatInquiryTarget targetKind targetId =
    if String.IsNullOrWhiteSpace targetId then
        targetKind + ": Auto-selected target"
    else
        targetKind + ": " + targetId

let private isPrimaryReasonFact (fact: InquiryAnswerFact) =
    match fact.Kind with
    | Reason
    | Warning
    | Status ->
        fact.Label <> "Maturity stage"
        && fact.Label <> "Governance state"
    | _ -> false

let private tryPrimaryReason (primaryFacts: InquiryAnswerFact list) =
    primaryFacts
    |> List.tryFind isPrimaryReasonFact
    |> Option.map (fun fact -> fact.Value)

let private tryRecommendedNextStep (maturity: InquiryMaturityContext) (primaryFacts: InquiryAnswerFact list) =
    match maturity.RecommendedNextStep with
    | Some step -> Some step
    | None ->
        primaryFacts
        |> List.tryPick (fun fact ->
            if fact.Kind = SuggestedAction then
                Some fact.Value
            else
                None)

let projectInquiryAnswerCard (result: FactsReconstructionResult) (answer: InquiryAnswer) =
    let maturity = answer.MaturityContext
    let primaryFacts, additionalFacts = splitAnswerFactsByProfile answer

    let primaryReason =
        tryPrimaryReason primaryFacts
        |> Option.defaultValue maturity.PrimaryMessage

    let evidenceStatus =
        if maturity.HasEvidence then
            "Evidence available"
        else
            "No evidence attached"

    let ledgerStatus =
        if maturity.HasLedgerHistory then
            "Ledger history available"
        else
            "No related ledger events"

    {
        Question = result.Question
        TargetLabel = formatInquiryTarget result.TargetKind result.TargetId
        Summary = formatInquiryAnswerSummary answer
        MaturityLabel = formatInquiryMaturityStage maturity.MaturityStage
        GovernanceState = maturity.GovernanceState
        PrimaryReason = primaryReason
        RecommendedNextStep = tryRecommendedNextStep maturity primaryFacts
        EvidenceStatus = evidenceStatus
        LedgerStatus = ledgerStatus
        PrimaryFactCount = primaryFacts.Length
        AdditionalFactCount = additionalFacts.Length
        SourcePhiCount = result.SourcePhiIds.Length
        LedgerEventCount = result.RelatedLedgerEvents.Length
    }
