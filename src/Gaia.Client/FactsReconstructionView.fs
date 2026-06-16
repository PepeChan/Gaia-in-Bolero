module Gaia.Client.FactsReconstructionView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.FactsReconstruction
open Gaia.Client.Inquiry
open Gaia.Client.InquiryAnswer

let private renderMuted textValue =
    p {
        attr.``class`` "has-text-grey"
        text textValue
    }

let private renderStringTags values =
    match values with
    | [] ->
        renderMuted "None"
    | values ->
        div {
            attr.``class`` "tags mb-0"
            forEach values <| fun value ->
                span {
                    attr.``class`` "tag is-link is-light facts-reconstruction-tag"
                    text value
                }
        }

let private renderStringList emptyText values =
    match values with
    | [] ->
        renderMuted emptyText
    | values ->
        ul {
            forEach values <| fun value ->
                li { text value }
        }

let private renderSourcePhiTexts sourcePhiTexts =
    div {
        attr.``class`` "table-container"
        table {
            attr.``class`` "table is-fullwidth is-striped is-narrow"

            thead {
                tr {
                    th { text "Phi ID" }
                    th { text "Source Phi text" }
                }
            }

            tbody {
                forEach sourcePhiTexts <| fun (phiId, phiText) ->
                    tr {
                        td { code { text phiId } }
                        td { text phiText }
                    }
            }
        }
    }

let private renderContextEntries entries =
    match entries with
    | [] ->
        renderMuted "No context entries used."
    | entries ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Context ID" }
                        th { text "Phi ID" }
                        th { text "Kind" }
                        th { text "Value" }
                        th { text "Provenance" }
                    }
                }

                tbody {
                    forEach entries <| fun entry ->
                        tr {
                            td { code { text entry.ContextId } }
                            td { code { text entry.PhiId } }
                            td { text entry.Kind }
                            td { text entry.Value }
                            td { text entry.Provenance }
                        }
                }
            }
        }

let private renderCandidateBasis basis =
    match basis with
    | [] ->
        span {
            attr.``class`` "has-text-grey"
            text "No relevant Sigma basis."
        }
    | basisItems ->
        ul {
            attr.``class`` "facts-reconstruction-basis"
            forEach basisItems <| fun basisItem ->
                li { text basisItem }
        }

let private renderCandidateFacts (candidates: CandidateDelta list) =
    match candidates with
    | [] ->
        renderMuted "No candidate facts reconstructed."
    | candidates ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate ID" }
                        th { text "Type" }
                        th { text "Target" }
                        th { text "Basis" }
                        th { text "Provenance" }
                    }
                }

                tbody {
                    forEach candidates <| fun candidate ->
                        tr {
                            td { code { text candidate.CandidateId } }
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { renderCandidateBasis candidate.RelevantSigmaBasis }
                            td { text candidate.Provenance }
                        }
                }
            }
        }

let private formatDecisionValue = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let private formatDecisionTimestamp (timestamp: DateTime) =
    timestamp.ToString("yyyy-MM-dd HH:mm:ss") + " UTC"

let private renderGovernanceDecisions (decisions: CandidateDecision list) =
    match decisions with
    | [] ->
        renderMuted "No governance decision is present for this reconstruction."
    | decisions ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate ID" }
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
                            td { text (formatDecisionValue decision.Decision) }
                            td { text (formatDecisionTimestamp decision.Timestamp) }
                            td { text decision.Rationale }
                        }
                }
            }
        }

let private renderLedgerEvents (events: LedgerEvent list) =
    match events with
    | [] ->
        renderMuted "No related ledger events found."
    | events ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "#" }
                        th { text "Event kind" }
                        th { text "Target" }
                        th { text "Summary" }
                        th { text "Detail" }
                    }
                }

                tbody {
                    forEach events <| fun ledgerEvent ->
                        tr {
                            td { text (string ledgerEvent.SequenceNumber) }
                            td { text ledgerEvent.EventKind }
                            td { code { text ledgerEvent.TargetId } }
                            td { text ledgerEvent.Summary }
                            td { text ledgerEvent.Detail }
                        }
                }
            }
        }

let private renderResultSection title (renderContent: unit -> Node) : Node =
    div {
        attr.``class`` "facts-reconstruction-section"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        renderContent ()
    }

let private renderEvidenceSection title (renderContent: unit -> Node) : Node =
    div {
        attr.``class`` "facts-reconstruction-evidence-section"
        h4 {
            attr.``class`` "subtitle is-6 mb-2"
            text title
        }
        renderContent ()
    }

let private renderSupportingEvidence (result: FactsReconstructionResult) =
    div {
        renderEvidenceSection "Fact lines" (fun () -> renderStringList "No supporting facts reconstructed." result.FactLines)
        renderEvidenceSection "Source Phi IDs" (fun () -> renderStringTags result.SourcePhiIds)
        renderEvidenceSection "Source Phi text" (fun () -> renderSourcePhiTexts result.SourcePhiTexts)
        renderEvidenceSection "Context entries used" (fun () -> renderContextEntries result.ContextEntriesUsed)
        renderEvidenceSection "Candidate type / target / basis" (fun () -> renderCandidateFacts result.CandidateFacts)
        renderEvidenceSection "Governance decision" (fun () -> renderGovernanceDecisions result.GovernanceDecisions)
        renderEvidenceSection "Provenance labels" (fun () -> renderStringTags result.ProvenanceLabels)
        renderEvidenceSection "Missing or unresolved items" (fun () -> renderStringList "No missing or unresolved items found." result.MissingOrUnresolvedItems)
    }

let private renderAnswerFactsTable emptyText (facts: InquiryAnswerFact list) =
    match facts with
    | [] ->
        renderMuted emptyText
    | facts ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Kind" }
                        th { text "Label" }
                        th { text "Value" }
                    }
                }

                tbody {
                    forEach facts <| fun fact ->
                        tr {
                            td { text (formatInquiryAnswerFactKind fact.Kind) }
                            td { text fact.Label }
                            td { text fact.Value }
                        }
                }
            }
        }

let private renderAnswerFactsPreview (result: FactsReconstructionResult) =
    let answer =
        inquiryAnswerFromFactsReconstructionResult result
        |> profileInquiryAnswer

    let profile = inquiryIntentProfileForAnswer answer
    let primaryFacts, additionalFacts = splitAnswerFactsByProfile answer

    match answer.Facts with
    | [] ->
        renderMuted "No answer facts projected."
    | _ ->
        div {
            div {
                attr.``class`` "notification is-light facts-reconstruction-summary"
                text (formatInquiryAnswerSummary answer)
            }

            div {
                attr.``class`` "tags mb-3"
                span {
                    attr.``class`` "tag is-info is-light"
                    text "Profiled answer facts"
                }
                span {
                    attr.``class`` "tag is-light"
                    text (formatInquiryIntentProfile profile)
                }
            }

            renderEvidenceSection
                "Primary answer facts"
                (fun () -> renderAnswerFactsTable "No primary facts selected for this profile." primaryFacts)

            renderEvidenceSection
                "Additional supporting facts"
                (fun () -> renderAnswerFactsTable "No additional supporting facts." additionalFacts)
        }

let private renderResultPanel (result: FactsReconstructionResult) =
    let inquiry = inquiryFromFactsReconstructionQuestion result.Question result.TargetKind result.TargetId

    div {
        attr.``class`` "box facts-reconstruction-result"

        div {
            attr.``class`` "level mb-3"

            div {
                attr.``class`` "level-left"
                div {
                    p {
                        attr.``class`` "heading mb-1"
                        text "Reverse inquiry"
                    }
                    h2 {
                        attr.``class`` "title is-5 mb-0"
                        text result.Question
                    }
                }
            }

            div {
                attr.``class`` "level-right"
                div {
                    attr.``class`` "tags mb-0"
                    span {
                        attr.``class`` "tag is-link is-light"
                        text (formatInquiryMode inquiry.Mode)
                    }
                    span {
                        attr.``class`` "tag is-info is-light"
                        text (formatInquiryKind inquiry.Kind)
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text ("Target kind: " + result.TargetKind)
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text ("Target: " + result.TargetId)
                    }
                }
            }
        }

        renderResultSection "Question" (fun () ->
            div {
                p {
                    attr.``class`` "mb-2"
                    text result.Question
                }
                div {
                    attr.``class`` "tags mb-0"
                    span {
                        attr.``class`` "tag is-light"
                        text ("Target kind: " + result.TargetKind)
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text ("Target: " + result.TargetId)
                    }
                }
            })

        renderResultSection "Answer" (fun () ->
            div {
                attr.``class`` "notification is-info is-light facts-reconstruction-summary"
                text result.AnswerSummary
            })

        renderResultSection "Answer Facts" (fun () -> renderAnswerFactsPreview result)
        renderResultSection "Supporting facts" (fun () -> renderSupportingEvidence result)
        renderResultSection "Reasons" (fun () -> renderStringList "No deterministic reason lines reconstructed." result.ReasonLines)
        renderResultSection "Recommended next actions" (fun () -> renderStringList "No next action suggested by this deterministic reconstruction." result.RecommendedNextActions)
        renderResultSection "Ledger / history" (fun () -> renderLedgerEvents result.RelatedLedgerEvents)
    }

let renderFactsReconstructionTab model dispatch =
    let targetOptions = getFactsReconstructionTargetOptions model
    let inquiry =
        inquiryFromFactsReconstructionQuestion
            model.factsReconstructionQuestion
            model.factsReconstructionTargetKind
            model.factsReconstructionTargetId

    div {
        attr.``class`` "mb-6 pb-5"

        h2 {
            attr.``class`` "title is-4"
            text "Inquiry Resolution / Reverse Inquiry"
        }

        p {
            attr.``class`` "has-text-grey mb-4"
            text "Reverse inquiries resolve stakeholder questions into answers from stored facts. T1-T5 remain the reasoning pipeline behind the reconstruction."
        }

        div {
            attr.``class`` "box"

            div {
                attr.``class`` "tags mb-4"
                span {
                    attr.``class`` "tag is-link is-light"
                    text (formatInquiryMode inquiry.Mode)
                }
                span {
                    attr.``class`` "tag is-info is-light"
                    text (formatInquiryKind inquiry.Kind)
                }
                span {
                    attr.``class`` "tag is-light"
                    text "Inquiry Resolution / Facts Reconstruction"
                }
            }

            div {
                attr.``class`` "columns is-variable is-4"

                div {
                    attr.``class`` "column is-5"
                    label {
                        attr.``class`` "label"
                        text "Inquiry question"
                    }
                    div {
                        attr.``class`` "select is-fullwidth"
                        select {
                            bind.input.string model.factsReconstructionQuestion (fun value -> dispatch (SetFactsReconstructionQuestion value))
                            forEach factsReconstructionQuestions <| fun question ->
                                option { text question }
                        }
                    }
                }

                div {
                    attr.``class`` "column is-3"
                    label {
                        attr.``class`` "label"
                        text "Target kind"
                    }
                    div {
                        attr.``class`` "select is-fullwidth"
                        select {
                            bind.input.string model.factsReconstructionTargetKind (fun value -> dispatch (SetFactsReconstructionTargetKind value))
                            forEach factsTargetKinds <| fun targetKind ->
                                option { text targetKind }
                        }
                    }
                }

                div {
                    attr.``class`` "column is-4"
                    label {
                        attr.``class`` "label"
                        text "Target"
                    }
                    div {
                        attr.``class`` "select is-fullwidth"
                        select {
                            bind.input.string model.factsReconstructionTargetId (fun value -> dispatch (SetFactsReconstructionTargetId value))
                            option {
                                attr.value ""
                                text "Auto-select target"
                            }
                            forEach targetOptions <| fun (targetId, label) ->
                                option {
                                    attr.value targetId
                                    text label
                                }
                        }
                    }
                }
            }

            div {
                attr.``class`` "level mb-0"

                div {
                    attr.``class`` "level-left"
                    p {
                        attr.``class`` "has-text-grey is-size-7 mb-0"
                        text "Read-only deterministic inquiry resolution from stored facts, candidates, decisions, provenance, and ledger history."
                    }
                }

                div {
                    attr.``class`` "level-right"
                    button {
                        attr.``class`` "button is-link"
                        attr.``type`` "button"
                        on.click (fun _ -> dispatch RunFactsReconstruction)
                        text "Resolve Inquiry"
                    }
                }
            }
        }

        match model.factsReconstructionResult with
        | None ->
            div {
                attr.``class`` "box"
                renderMuted "Choose a reverse inquiry and resolve it to inspect stored project facts."
            }
        | Some result ->
            renderResultPanel result
    }
