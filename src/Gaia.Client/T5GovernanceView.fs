module Gaia.Client.T5GovernanceView

open System
open Bolero
open Bolero.Html
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.T2ParsingView
open Gaia.Client.T3SummaryView
open Gaia.Client.T4CandidateView

let countCandidateDecisions decisionValue (candidateDeltas: CandidateDelta list) (candidateDecisions: CandidateDecision list) =
    candidateDeltas
    |> List.sumBy (fun candidate ->
        if getCandidateDecisionValue candidate.CandidateId candidateDecisions = decisionValue then
            1
        else
            0)

let renderCandidateDecisionCount label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderCandidateDecisionTag decisionValue =
    span {
        attr.``class`` (candidateDecisionTagClass decisionValue)
        text (formatCandidateDecisionValue decisionValue)
    }

let renderT5GovernanceSummaryTable sigmaContext (candidateDecisions: CandidateDecision list) dispatch =
    let candidateDeltas = formulateCandidateDeltas sigmaContext
    let pendingCount = countCandidateDecisions Pending candidateDeltas candidateDecisions
    let acceptedCount = countCandidateDecisions Accepted candidateDeltas candidateDecisions
    let rejectedCount = countCandidateDecisions Rejected candidateDeltas candidateDecisions
    let heldCount = countCandidateDecisions Held candidateDeltas candidateDecisions

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "T5 Governance Summary"
        }

        div {
            attr.``class`` "tags mb-3"
            renderCandidateDecisionCount "Pending" pendingCount
            renderCandidateDecisionCount "Accepted" acceptedCount
            renderCandidateDecisionCount "Rejected" rejectedCount
            renderCandidateDecisionCount "Held" heldCount
        }

        div {
            attr.``class`` "table-container mb-2"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate type" }
                        th { text "Target" }
                        th { text "Basis count" }
                        th { text "Provenance" }
                        th { text "Decision" }
                        th { text "Action" }
                    }
                }

                tbody {
                    forEach candidateDeltas <| fun candidate ->
                        let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions

                        tr {
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { text (string (List.length candidate.RelevantSigmaBasis)) }
                            td { text candidate.Provenance }
                            td { renderCandidateDecisionTag decisionValue }
                            td { renderCandidateGovernanceActions candidate decisionValue dispatch }
                        }
                }
            }
        }

        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "T5 records governance decisions only. Candidate promotion to Sigma is intentionally not performed here."
        }
    }

let cognitionReviewTargetFilters = [ "All"; "Host"; "Interface"; "State"; "Mode"; "Constraint"; "Reinforced Atom" ]
let cognitionReviewDecisionFilters = [ "All"; "Pending"; "Accepted"; "Rejected"; "Held" ]

let getTopMissingContextRows sequencedParsedPhis =
    getMissingContextSummaryRows sequencedParsedPhis
    |> List.sortByDescending snd
    |> List.truncate 5

let getTopArchitecturalPressureRows sigmaContext =
    getArchitecturalPressureRows sigmaContext
    |> List.sortByDescending (fun (_, basisCount, _) -> basisCount)
    |> List.truncate 5

let getTopReinforcedAtomRows sigmaContext =
    getReinforcedAtoms sigmaContext
    |> List.sortByDescending (fun (_, entry) -> entry.SupportCount)
    |> List.truncate 10

let normalizeReviewText (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        ""
    else
        value.Trim().ToLowerInvariant()

let candidateMatchesTargetFilter filterValue (candidate: CandidateDelta) =
    match filterValue with
    | "All" -> true
    | "Reinforced Atom" -> candidate.Kind = ReinforcedSigmaAtom
    | "Host"
    | "Interface"
    | "State"
    | "Mode"
    | "Constraint" -> candidate.Kind <> ReinforcedSigmaAtom && candidate.Target = filterValue
    | _ -> true

let candidateMatchesDecisionFilter filterValue decisionValue =
    match filterValue with
    | "All" -> true
    | "Pending" -> decisionValue = Pending
    | "Accepted" -> decisionValue = Accepted
    | "Rejected" -> decisionValue = Rejected
    | "Held" -> decisionValue = Held
    | _ -> true

let candidateMatchesTextFilter searchText (candidate: CandidateDelta) =
    if searchText = "" then
        true
    else
        [
            formatCandidateDeltaKind candidate.Kind
            candidate.Target
            candidate.ProposedTransition
            candidate.Reason
            candidate.Provenance
            yield! candidate.RelevantSigmaBasis
            yield! getCandidateSupportingPhiIds candidate
            yield! getCandidateAtomValues candidate
        ]
        |> String.concat " "
        |> normalizeReviewText
        |> fun haystack -> haystack.Contains(searchText)

let getFilteredReviewCandidates (model: Model) sigmaContext =
    let searchText = normalizeReviewText model.cognitionReviewTextFilter

    formulateCandidateDeltas sigmaContext
    |> List.filter (fun candidate ->
        let decisionValue = getCandidateDecisionValue candidate.CandidateId model.candidateDecisions

        candidateMatchesTargetFilter model.cognitionReviewTargetFilter candidate
        && candidateMatchesDecisionFilter model.cognitionReviewDecisionFilter decisionValue
        && candidateMatchesTextFilter searchText candidate)

let interpretReviewCandidate (candidate: CandidateDelta) =
    match candidate.Kind with
    | AddUnknownRevealMissingHost -> "Functions and states exist, but no host has been identified."
    | AddInterface -> "This interface appears available for boundary reasoning."
    | AddState -> "This state appears available for condition and behavior reasoning."
    | AddMode -> "This mode appears available for operational-context reasoning."
    | AddHost -> "This host appears available for allocation and system-boundary reasoning."
    | AddConstraint -> "This constraint appears available for design-limit reasoning."
    | ReinforcedSigmaAtom -> "This atom appears in multiple Phi and may be an architectural theme."
    | NoStructuralChange -> "No actionable candidate transition is currently visible."

let renderLimitedPhiChips phiIds =
    let visiblePhiIds = phiIds |> List.truncate 5
    let remainingCount = List.length phiIds - List.length visiblePhiIds

    match phiIds with
    | [] ->
        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "No supporting Phi IDs available."
        }
    | _ ->
        div {
            attr.``class`` "tags mb-0"
            forEach visiblePhiIds <| fun phiId ->
                span {
                    attr.``class`` "tag is-link is-light"
                    text phiId
                }

            if remainingCount > 0 then
                span {
                    attr.``class`` "tag is-light"
                    text ("+" + string remainingCount + " more")
                }
        }

let renderPhiChips phiIds =
    match phiIds with
    | [] ->
        p {
            attr.``class`` "has-text-grey mb-0"
            text "No supporting Phi IDs available."
        }
    | values ->
        div {
            attr.``class`` "tags mb-0 sigma-basis-phi-tags"
            forEach values <| fun phiId ->
                span {
                    attr.``class`` "tag is-link is-light sigma-basis-phi-tag"
                    text phiId
                }
        }

let renderLimitedBasis basis =
    match basis with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text "No relevant Sigma basis."
        }
    | values ->
        div {
            forEach values <| fun basisValue ->
                p {
                    attr.``class`` "mb-2"
                    text basisValue
                }
        }

let renderSigmaBasisItemActions basisItemKey decisionValue dispatch =
    div {
        attr.``class`` "buttons are-small mb-0"
        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Accepted "is-success")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecision (basisItemKey, Accepted)))
            text "Accept"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Rejected "is-danger")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecision (basisItemKey, Rejected)))
            text "Reject"
        }

        button {
            attr.``class`` (candidateDecisionButtonClass decisionValue Held "is-warning")
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecision (basisItemKey, Held)))
            text "Hold"
        }
    }

let renderSigmaBasisItemBulkActions basisItemKeys dispatch =
    div {
        attr.``class`` "buttons are-small mb-0"
        button {
            attr.``class`` "button is-small is-success is-light"
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecisions (basisItemKeys, Accepted)))
            text "Accept all basis items"
        }

        button {
            attr.``class`` "button is-small is-danger is-light"
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecisions (basisItemKeys, Rejected)))
            text "Reject all basis items"
        }

        button {
            attr.``class`` "button is-small is-warning is-light"
            attr.``type`` "button"
            on.click (fun _ -> dispatch (SetSigmaBasisItemDecisions (basisItemKeys, Held)))
            text "Hold all basis items"
        }
    }

let renderSigmaBasisItemReview (basisItem: SigmaBasisItemReview) (sigmaBasisItemDecisions: Map<string, CandidateDecisionValue>) dispatch =
    let decisionValue = getSigmaBasisItemDecisionValue basisItem.Key sigmaBasisItemDecisions

    div {
        attr.``class`` "sigma-basis-item"

        div {
            attr.``class`` "columns is-variable is-3 mb-2"

            div {
                attr.``class`` "column is-8"
                div {
                    attr.``class`` "tags mb-2"
                    span {
                        attr.``class`` "tag is-light"
                        text ("Kind: " + basisItem.Kind)
                    }
                    span {
                        attr.``class`` "tag is-info is-light"
                        text ("Support count: " + string basisItem.SupportCount)
                    }
                }

                p {
                    attr.``class`` "sigma-basis-atom mb-0"
                    strong { text "Atom value: " }
                    text basisItem.AtomValue
                }
            }

            div {
                attr.``class`` "column is-4"
                p {
                    attr.``class`` "mb-2"
                    strong { text "Item review state: " }
                    renderCandidateDecisionTag decisionValue
                }
                renderSigmaBasisItemActions basisItem.Key decisionValue dispatch
            }
        }

        div {
            attr.``class`` "sigma-basis-detail-block"
            p {
                attr.``class`` "has-text-weight-semibold mb-2"
                text "Supporting Phi IDs"
            }
            renderPhiChips basisItem.SupportingPhiIds
        }

        match basisItem.RawPhiPreview with
        | None -> empty()
        | Some preview ->
            div {
                attr.``class`` "sigma-basis-detail-block sigma-basis-preview"
                p {
                    attr.``class`` "has-text-weight-semibold mb-1"
                    text "Raw Phi preview"
                }
                p {
                    attr.``class`` "mb-0"
                    text preview
                }
            }
    }

let renderTopMissingContextReview sequencedParsedPhis =
    div {
        attr.``class`` "column is-4"

        h3 {
            attr.``class`` "title is-6"
            text "Top Missing Context"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Area" }
                        th { text "Count" }
                        th { text "Interpretation" }
                    }
                }

                tbody {
                    forEach (getTopMissingContextRows sequencedParsedPhis) <| fun (missingArea, count) ->
                        tr {
                            td { text missingArea }
                            td { text (string count) }
                            td { text (interpretMissingContextCount count) }
                        }
                }
            }
        }
    }

let renderTopArchitecturalPressuresReview sigmaContext =
    div {
        attr.``class`` "column is-4"

        h3 {
            attr.``class`` "title is-6"
            text "Top Architectural Pressures"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Target" }
                        th { text "Basis" }
                        th { text "Pressure" }
                        th { text "Meaning" }
                    }
                }

                tbody {
                    forEach (getTopArchitecturalPressureRows sigmaContext) <| fun (target, basisCount, meaning) ->
                        tr {
                            td { text target }
                            td { text (string basisCount) }
                            td { text (interpretPressure basisCount) }
                            td { text meaning }
                        }
                }
            }
        }
    }

let renderTopReinforcedAtomsReview sigmaContext =
    div {
        attr.``class`` "column is-4"

        h3 {
            attr.``class`` "title is-6"
            text "Top Reinforced Atoms"
        }

        match getTopReinforcedAtomRows sigmaContext with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No reinforced atoms yet."
            }
        | atoms ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Kind" }
                            th { text "Atom" }
                            th { text "Support" }
                            th { text "Supporting Phi" }
                        }
                    }

                    tbody {
                        forEach atoms <| fun (kind, entry) ->
                            tr {
                                td { text kind }
                                td { text entry.Value }
                                td { text (string entry.SupportCount) }
                                td { renderLimitedPhiChips entry.SupportingPhiIds }
                            }
                    }
                }
            }
    }

let renderCognitionReviewFilters (model: Model) dispatch =
    div {
        attr.``class`` "columns is-variable is-3 mb-2"

        div {
            attr.``class`` "column is-3"
            label {
                attr.``class`` "label is-size-7"
                text "Candidate target"
            }
            div {
                attr.``class`` "select is-fullwidth is-small"
                select {
                    bind.input.string model.cognitionReviewTargetFilter (fun value -> dispatch (SetCognitionReviewTargetFilter value))
                    forEach cognitionReviewTargetFilters <| fun filterValue ->
                        option { text filterValue }
                }
            }
        }

        div {
            attr.``class`` "column is-3"
            label {
                attr.``class`` "label is-size-7"
                text "Decision"
            }
            div {
                attr.``class`` "select is-fullwidth is-small"
                select {
                    bind.input.string model.cognitionReviewDecisionFilter (fun value -> dispatch (SetCognitionReviewDecisionFilter value))
                    forEach cognitionReviewDecisionFilters <| fun filterValue ->
                        option { text filterValue }
                }
            }
        }

        div {
            attr.``class`` "column is-6"
            label {
                attr.``class`` "label is-size-7"
                text "Search"
            }
            input {
                attr.``class`` "input is-small"
                attr.placeholder "Candidate type, atom, target, or supporting Phi ID"
                bind.input.string model.cognitionReviewTextFilter (fun value -> dispatch (SetCognitionReviewTextFilter value))
            }
        }
    }

let renderReviewCandidateCard
    (candidate: CandidateDelta)
    (candidateDecisions: CandidateDecision list)
    (sigmaBasisItemDecisions: Map<string, CandidateDecisionValue>)
    sequencedParsedPhis
    dispatch =
    let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions
    let basisItems = buildSigmaBasisItemReviews candidate sequencedParsedPhis
    let basisItemKeys = basisItems |> List.map (fun basisItem -> basisItem.Key)

    div {
        attr.``class`` "card mb-4 cognition-review-card"

        div {
            attr.``class`` "card-content"

            div {
                attr.``class`` "columns is-variable is-3 is-vcentered mb-2"

                div {
                    attr.``class`` "column is-4"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Candidate"
                    }
                    h4 {
                        attr.``class`` "title is-6 mb-2"
                        text (formatCandidateDeltaKind candidate.Kind)
                    }
                    p {
                        attr.``class`` "is-size-7 has-text-grey"
                        text (interpretReviewCandidate candidate)
                    }
                }

                div {
                    attr.``class`` "column is-2"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Target"
                    }
                    p { text candidate.Target }
                }

                div {
                    attr.``class`` "column is-2"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Basis"
                    }
                    p { text (string (List.length candidate.RelevantSigmaBasis)) }
                }

                div {
                    attr.``class`` "column is-2"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Decision"
                    }
                    renderCandidateDecisionTag decisionValue
                }

                div {
                    attr.``class`` "column is-2"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Provenance"
                    }
                    span {
                        attr.``class`` "tag is-info is-light"
                        text candidate.Provenance
                    }
                }
            }

            div {
                attr.``class`` "cognition-review-section"

                div {
                    attr.``class`` "columns is-variable is-3"

                    div {
                        attr.``class`` "column is-8"
                        h5 {
                            attr.``class`` "title is-6 mb-2"
                            text "Section 1: Candidate class decision"
                        }

                        p {
                            attr.``class`` "mb-2"
                            strong { text "Proposed transition: " }
                            text candidate.ProposedTransition
                        }

                        p {
                            attr.``class`` "mb-0"
                            strong { text "Why this candidate exists: " }
                            text candidate.Reason
                        }
                    }

                    div {
                        attr.``class`` "column is-4"
                        p {
                            attr.``class`` "mb-2"
                            strong { text "Candidate class decision: " }
                            renderCandidateDecisionTag decisionValue
                        }

                        renderCandidateGovernanceActions candidate decisionValue dispatch
                    }
                }
            }

            div {
                attr.``class`` "cognition-review-section cognition-review-basis-section"

                div {
                    attr.``class`` "level is-mobile mb-2"
                    div {
                        attr.``class`` "level-left"
                        div {
                            h5 {
                                attr.``class`` "title is-6 mb-1"
                                text "Section 2: Basis item review"
                            }
                            p {
                                attr.``class`` "has-text-grey mb-0"
                                text "Local/session review state"
                            }
                        }
                    }
                    div {
                        attr.``class`` "level-right"
                        span {
                            attr.``class`` "tag is-light"
                            text ("Basis items: " + string (List.length basisItems))
                        }
                    }
                }

                p {
                    attr.``class`` "notification is-info is-light cognition-review-helper"
                    text "Class-level governance affects the candidate type. Basis item review records human review intent for individual Sigma atoms. Persistent atom-level promotion will be added later."
                }

                div {
                    attr.``class`` "mb-3"
                    renderSigmaBasisItemBulkActions basisItemKeys dispatch
                }

                match basisItems with
                | [] ->
                    p {
                        attr.``class`` "has-text-grey"
                        text "No relevant Sigma basis."
                    }
                | items ->
                    div {
                        attr.``class`` "sigma-basis-list"
                        forEach items <| fun basisItem ->
                            renderSigmaBasisItemReview basisItem sigmaBasisItemDecisions dispatch
                    }
            }
        }
    }

let renderCognitionReviewPanel (model: Model) sequencedParsedPhis sigmaContext dispatch =
    let reviewCandidates = getFilteredReviewCandidates model sigmaContext

    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "Cognition Review"
        }

        p {
            attr.``class`` "is-size-7 has-text-grey mb-4"
            text "Candidate-class decisions persist. Basis item decisions are local/session review state in this v0."
        }

        div {
            attr.``class`` "columns is-variable is-4"
            renderTopMissingContextReview sequencedParsedPhis
            renderTopArchitecturalPressuresReview sigmaContext
            renderTopReinforcedAtomsReview sigmaContext
        }

        hr {}

        h3 {
            attr.``class`` "title is-6"
            text "Review Queue"
        }

        renderCognitionReviewFilters model dispatch

        div {
            attr.``class`` "tags mb-3"
            span {
                attr.``class`` "tag is-light"
                text ("Matching candidates: " + string (List.length reviewCandidates))
            }
        }

        match reviewCandidates with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No candidates match the current review filters."
            }
        | candidates ->
            forEach candidates <| fun candidate ->
                renderReviewCandidateCard
                    candidate
                    model.candidateDecisions
                    model.sigmaBasisItemDecisions
                    sequencedParsedPhis
                    dispatch
    }

let renderLatestDeltaSummaryTable lastReplayAction =
    div {
        h3 {
            attr.``class`` "title is-6"
            text "Latest Delta Summary"
        }

        match lastReplayAction with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "No Sigma-changing action yet."
            }
        | Some analysis ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Last action" }
                            th { text "Added atom count" }
                            th { text "Removed atom count" }
                            th { text "Reinforced atom count" }
                        }
                    }

                    tbody {
                        tr {
                            td { text analysis.Action }
                            td { text (string (countDeltaSigmaAtoms analysis.AddedAtoms)) }
                            td { text (string (countDeltaSigmaAtoms analysis.RemovedAtoms)) }
                            td { text (string (countDeltaSigmaAtoms analysis.AlreadyKnownAtoms)) }
                        }
                    }
                }
            }
    }

let renderT5DecisionHistoryPanel (candidateDecisions: CandidateDecision list) =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "T5 — Decision History"
        }

        match candidateDecisions with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No T5 decisions recorded yet."
            }
        | decisions ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "CandidateId" }
                            th { text "Candidate type" }
                            th { text "Target" }
                            th { text "Decision" }
                            th { text "Timestamp" }
                            th { text "Rationale" }
                        }
                    }

                    tbody {
                        forEach decisions <| fun decision ->
                            tr {
                                td { code { text decision.CandidateId } }
                                td { text decision.CandidateType }
                                td { text decision.Target }
                                td { renderCandidateDecisionTag decision.Decision }
                                td { text (formatCandidateDecisionTimestamp decision.Timestamp) }
                                td { text decision.Rationale }
                            }
                    }
                }
            }
    }

let renderOperationalSummaryTablesPanel sequencedParsedPhis sigmaContext lastReplayAction (candidateDecisions: CandidateDecision list) dispatch =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "Summary Tables"
        }

        renderCurrentSigmaSummaryTable sigmaContext

        renderMissingContextSummaryTable sequencedParsedPhis

        renderArchitecturalPressureSummaryTable sigmaContext

        renderTopReinforcedAtomsTable sigmaContext

        renderT4CandidateSummaryTable sigmaContext

        renderT5GovernanceSummaryTable sigmaContext candidateDecisions dispatch

        renderLatestDeltaSummaryTable lastReplayAction
    }
