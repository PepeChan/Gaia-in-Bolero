module Gaia.Client.T4CandidateView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let tryFindCandidateDecision candidateId (candidateDecisions: CandidateDecision list) =
    candidateDecisions
    |> List.tryFind (fun decision -> decision.CandidateId = candidateId)

let getCandidateDecisionValue candidateId candidateDecisions =
    tryFindCandidateDecision candidateId candidateDecisions
    |> Option.map (fun decision -> decision.Decision)
    |> Option.defaultValue Pending

let formatCandidateDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let candidateDecisionTagClass = function
    | Pending -> "tag is-light"
    | Accepted -> "tag is-success is-light"
    | Rejected -> "tag is-danger is-light"
    | Held -> "tag is-warning is-light"

let candidateGroupStatusTagClass = function
    | GroupPending -> "tag is-light"
    | GroupAccepted -> "tag is-success"
    | GroupRejected -> "tag is-danger"
    | GroupHeld -> "tag is-warning"
    | GroupMixed -> "tag is-danger"
    | GroupPartiallyAccepted -> "tag is-info"
    | GroupPartiallyGoverned -> "tag is-info"

let renderCandidateGroupStatusTag status =
    span {
        attr.``class`` (candidateGroupStatusTagClass status)
        text (formatCandidateGroupStatus status)
    }

let candidateDecisionButtonClass decisionValue activeDecision buttonStyle =
    if decisionValue = activeDecision then
        "button is-small " + buttonStyle
    else
        "button is-small " + buttonStyle + " is-light"

let renderCandidateGovernanceActions (candidate: CandidateDelta) decisionValue dispatch =
    div {
        attr.``class`` "buttons are-small mb-0"
        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Accepted "is-success")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (AcceptCandidate candidate.CandidateId))
            text "Accept"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Rejected "is-danger")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (RejectCandidate candidate.CandidateId))
            text "Reject"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Held "is-warning")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (HoldCandidate candidate.CandidateId))
            text "Hold"
        }
    }

let formatCandidateDecisionTimestamp (timestamp: DateTime) =
    timestamp.ToString("yyyy-MM-dd HH:mm:ss") + " UTC"

let renderCandidateAmendmentResetNotice (candidate: CandidateDelta) ledgerEvents =
    let resetEvents =
        ledgerEvents
        |> getParseAmendmentResetEventsForCandidate candidate.CandidateId
        |> List.rev
        |> List.truncate 3

    match resetEvents with
    | [] -> empty()
    | events ->
        div {
            attr.``class`` "notification is-warning is-light is-size-7 mb-2"
            p {
                attr.``class`` "has-text-weight-semibold mb-1"
                text "Basis decision reset by parse amendment"
            }
            forEach events <| fun ledgerEvent ->
                div {
                    p {
                        attr.``class`` "mb-1"
                        code { text ledgerEvent.TargetId }
                    }
                    p {
                        attr.``class`` "mb-2"
                        text ledgerEvent.Detail
                    }
                }
        }

let renderCandidateClassDecisionTag candidateDecision =
    match candidateDecision with
    | None ->
        span {
            attr.``class`` "tag is-light"
            text "No class decision"
        }
    | Some decision ->
        span {
            attr.``class`` (candidateDecisionTagClass decision.Decision)
            text (formatCandidateDecisionValue decision.Decision)
        }

