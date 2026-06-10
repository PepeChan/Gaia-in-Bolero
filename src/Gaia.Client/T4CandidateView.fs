module Gaia.Client.T4CandidateView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let renderCandidateDeltaBasis basis =
    match basis with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text "No relevant Sigma basis."
        }
    | values ->
        ul {
            forEach values <| fun value ->
                li { text value }
        }

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

let renderCandidateDecisionMetadata (candidateDecision: CandidateDecision option) =
    match candidateDecision with
    | None ->
        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "No T5 decision recorded yet."
        }
    | Some decision ->
        div {
            p {
                attr.``class`` "is-size-7 has-text-grey mb-1"
                strong { text "Decision timestamp: " }
                text (formatCandidateDecisionTimestamp decision.Timestamp)
            }

            p {
                attr.``class`` "is-size-7 has-text-grey"
                strong { text "Rationale: " }
                text decision.Rationale
            }
        }

let renderCandidateDeltaCard (candidate: CandidateDelta) (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDecision = tryFindCandidateDecision candidate.CandidateId candidateDecisions
    let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions

    div {
        attr.``class`` "card mb-4"

        div {
            attr.``class`` "card-content"

            p {
                attr.``class`` "heading"
                text "Candidate type"
            }

            h3 {
                attr.``class`` "title is-6"
                text (formatCandidateDeltaKind candidate.Kind)
            }

            p {
                attr.``class`` "is-size-7 has-text-grey mb-3"
                strong { text "Candidate ID: " }
                code { text candidate.CandidateId }
            }

            div {
                attr.``class`` "columns is-multiline"

                div {
                    attr.``class`` "column is-6"
                    p {
                        strong { text "Target of change: " }
                        text candidate.Target
                    }
                }

                div {
                    attr.``class`` "column is-6"
                    p {
                        strong { text "Confidence: " }
                        text candidate.Confidence
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Proposed transition: " }
                        text candidate.ProposedTransition
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Why this candidate exists: " }
                        text candidate.Reason
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "Relevant Sigma basis" }
                    }

                    renderCandidateDeltaBasis candidate.RelevantSigmaBasis
                }

                div {
                    attr.``class`` "column is-12"
                    span {
                        attr.``class`` "tag is-warning is-light"
                        text candidate.Status
                    }
                }

                div {
                    attr.``class`` "column is-12"
                    p {
                        strong { text "T5 governance decision: " }
                        span {
                            attr.``class`` (candidateDecisionTagClass decisionValue)
                            text (formatCandidateDecisionValue decisionValue)
                        }
                    }

                    div {
                        attr.``class`` "mt-2 mb-2"
                        renderCandidateGovernanceActions candidate decisionValue dispatch
                    }

                    renderCandidateDecisionMetadata candidateDecision
                }
            }
        }
    }

let renderCandidateDeltaSigmaPanel sigmaContext (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "T4 — Candidate ΔΣ Formulation"
        }

        p {
            attr.``class`` "notification is-info is-light"
            text "T4 formulates candidate changes only. T5 records governance decisions here without Σ promotion."
        }

        forEach candidateDeltas <| fun candidateDelta ->
            renderCandidateDeltaCard candidateDelta candidateDecisions dispatch
    }

