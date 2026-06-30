module Gaia.Client.Ledger

open System
open Gaia.Client.Types

let getUtcTimestampString () =
    DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fff'Z'")

let createLedgerEvent eventKind targetId summary detail (ledgerEvents: LedgerEvent list) =
    let sequenceNumber = List.length ledgerEvents + 1

    {
        EventId = sprintf "LEDGER-%06d" sequenceNumber
        SequenceNumber = sequenceNumber
        TimestampUtc = getUtcTimestampString ()
        Actor = "Demo user"
        EventKind = eventKind
        TargetId = targetId
        Summary = summary
        Detail = detail
    }

let appendLedgerEventToList eventKind targetId summary detail ledgerEvents =
    ledgerEvents @ [ createLedgerEvent eventKind targetId summary detail ledgerEvents ]

let inquiryResolvedLedgerEventKind = "InquiryResolved"

let isPhiLedgerEvent eventKind =
    eventKind = "PhiIngested"
    || eventKind = "PhiParsed"
    || eventKind = "PhiParseIgnoredAlreadyParsed"
    || eventKind = "PhiContextEntryCreated"

let isReplayLedgerEvent eventKind =
    eventKind = "PhiExcludedFromReplay"
    || eventKind = "PhiIncludedInReplay"

let isGovernanceLedgerEvent eventKind =
    eventKind = "CandidateAccepted"
    || eventKind = "CandidateRejected"
    || eventKind = "CandidateHeld"
    || eventKind = "SigmaBasisItemAccepted"
    || eventKind = "SigmaBasisItemRejected"
    || eventKind = "SigmaBasisItemHeld"
    || eventKind = sigmaBasisItemDecisionResetLedgerKind

let isInquiryLedgerEvent eventKind =
    eventKind = inquiryResolvedLedgerEventKind

let isAuditOnlyLedgerEvent eventKind =
    isInquiryLedgerEvent eventKind

let countLedgerEvents predicate (ledgerEvents: LedgerEvent list) =
    ledgerEvents
    |> List.filter (fun ledgerEvent -> predicate ledgerEvent.EventKind)
    |> List.length

let addToSet value values =
    values
    |> Set.add value

let removeFromSet value values =
    values
    |> Set.remove value

let applyReplayGovernanceEvent eventKind targetId governanceDecisions =
    match eventKind with
    | "CandidateAccepted" ->
        governanceDecisions
        |> Map.add targetId Accepted
    | "CandidateRejected" ->
        governanceDecisions
        |> Map.add targetId Rejected
    | "CandidateHeld" ->
        governanceDecisions
        |> Map.add targetId Held
    | _ ->
        governanceDecisions

let buildReplayPreviewState (ledgerEvents: LedgerEvent list) =
    let parsedPhiEvents, includedPhiIds, excludedPhiIds, governanceDecisions =
        ledgerEvents
        |> List.fold
            (fun (parsedCount, includedIds, excludedIds, decisions) ledgerEvent ->
                match ledgerEvent.EventKind with
                | "PhiParsed" ->
                    parsedCount + 1,
                    includedIds |> addToSet ledgerEvent.TargetId,
                    excludedIds |> removeFromSet ledgerEvent.TargetId,
                    decisions
                | "PhiExcludedFromReplay" ->
                    parsedCount,
                    includedIds |> removeFromSet ledgerEvent.TargetId,
                    excludedIds |> addToSet ledgerEvent.TargetId,
                    decisions
                | "PhiIncludedInReplay" ->
                    parsedCount,
                    includedIds |> addToSet ledgerEvent.TargetId,
                    excludedIds |> removeFromSet ledgerEvent.TargetId,
                    decisions
                | "PhiParseIgnoredAlreadyParsed" ->
                    parsedCount, includedIds, excludedIds, decisions
                | eventKind ->
                    parsedCount,
                    includedIds,
                    excludedIds,
                    applyReplayGovernanceEvent eventKind ledgerEvent.TargetId decisions)
            (0, Set.empty<string>, Set.empty<string>, Map.empty<string, CandidateDecisionValue>)

    let countGovernanceDecision decisionValue =
        governanceDecisions
        |> Map.toList
        |> List.filter (fun (_, decision) -> decision = decisionValue)
        |> List.length

    {
        ParsedPhiEvents = parsedPhiEvents
        IncludedPhiCount = Set.count includedPhiIds
        ExcludedPhiCount = Set.count excludedPhiIds
        GovernanceAccepted = countGovernanceDecision Accepted
        GovernanceRejected = countGovernanceDecision Rejected
        GovernanceHeld = countGovernanceDecision Held
        TotalLedgerEvents = List.length ledgerEvents
    }

let getReplayPreviewEvents selectedSequence ledgerEvents =
    ledgerEvents
    |> List.filter (fun ledgerEvent -> ledgerEvent.SequenceNumber <= selectedSequence)
