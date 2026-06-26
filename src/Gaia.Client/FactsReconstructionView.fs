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
open Gaia.Client.InquiryAnswerProjection

let private renderMuted textValue =
    p {
        attr.``class`` "has-text-grey"
        text textValue
    }

let private containsText needle (value: string) =
    not (String.IsNullOrWhiteSpace needle)
    && not (isNull value)
    && value.IndexOf(needle, StringComparison.OrdinalIgnoreCase) >= 0

let private firstRecognizedSemanticKind values =
    values
    |> List.choose id
    |> List.tryFind (fun value -> tryGetCognopyObjectClass value |> Option.isSome)

let private renderSemanticText semanticKind textValue =
    match semanticKind |> Option.bind tryGetCognopyObjectClass with
    | None -> text textValue
    | Some className ->
        span {
            attr.``class`` className
            text textValue
        }

let private tryFactSemanticKind (fact: InquiryAnswerFact) =
    match fact.Kind with
    | Evidence -> Some "Evidence"
    | OpenDecision -> Some "Decision"
    | Status when fact.SourceKind = "CandidateDecision" || containsText "decision" fact.Label -> Some "Decision"
    | _ ->
        firstRecognizedSemanticKind [
            fact.TargetKind
            Some fact.SourceKind
            Some fact.Label
        ]

let private semanticFactRowClass fact =
    fact
    |> tryFactSemanticKind
    |> Option.bind tryGetCognopyObjectRowClass
    |> Option.defaultValue ""

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
                            td { renderSemanticText (Some entry.Kind) entry.Kind }
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
                            td { renderSemanticText (Some "Decision") (formatDecisionValue decision.Decision) }
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
                            attr.``class`` (semanticFactRowClass fact)
                            td { renderSemanticText (tryFactSemanticKind fact) (formatInquiryAnswerFactKind fact.Kind) }
                            td { text fact.Label }
                            td { text fact.Value }
                        }
                }
            }
        }

let private profiledInquiryAnswerFromResult (result: FactsReconstructionResult) =
    inquiryAnswerFromFactsReconstructionResult result
    |> profileInquiryAnswer

let private renderAnswerFactsPreview (answer: InquiryAnswer) =
    let profile = inquiryIntentProfileForAnswer answer
    let maturity = answer.MaturityContext
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
                span {
                    attr.``class`` "tag is-warning is-light"
                    text ("Maturity: " + formatInquiryMaturityStage maturity.MaturityStage)
                }
            }

            div {
                attr.``class`` "notification is-light"
                p {
                    strong { text "Maturity stage: " }
                    text (formatInquiryMaturityStage maturity.MaturityStage)
                }
                p {
                    strong { text "Primary message: " }
                    text maturity.PrimaryMessage
                }
                match maturity.RecommendedNextStep with
                | None -> empty()
                | Some step ->
                    p {
                        strong { text "Recommended next step: " }
                        text step
                    }
            }

            renderEvidenceSection
                "Primary answer facts"
                (fun () -> renderAnswerFactsTable "No primary facts selected for this profile." primaryFacts)

            renderEvidenceSection
                "Additional supporting facts"
                (fun () -> renderAnswerFactsTable "No additional supporting facts." additionalFacts)
        }

let private renderInquiryCardLine label value =
    p {
        attr.``class`` "mb-2"
        strong { text (label + ": ") }
        text value
    }

let private renderInquiryCardSemanticLine label semanticKind value =
    p {
        attr.``class`` "mb-2"
        strong { text (label + ": ") }
        renderSemanticText (Some semanticKind) value
    }

let private renderInquiryCountTag label count =
    span {
        attr.``class`` "tag is-light"
        text (label + " " + string count)
    }

let private renderInquiryCard (result: FactsReconstructionResult) (answer: InquiryAnswer) dispatch =
    let projection = projectInquiryAnswerCard result answer

    div {
        attr.``class`` "box facts-reconstruction-result"

        p {
            attr.``class`` "heading mb-1"
            text "Inquiry card"
        }

        h2 {
            attr.``class`` "title is-5 mb-3"
            text projection.Question
        }

        div {
            attr.``class`` "tags mb-3"
            span {
                attr.``class`` "tag is-info is-light"
                text "Compact projection"
            }
            span {
                attr.``class`` "tag is-light"
                text projection.TargetLabel
            }
            span {
                attr.``class`` "tag is-warning is-light"
                text projection.MaturityLabel
            }
        }

        div {
            attr.``class`` "notification is-info is-light facts-reconstruction-summary"
            text projection.Summary
        }

        div {
            attr.``class`` "content mb-3"
            renderInquiryCardLine "Maturity stage" projection.MaturityLabel
            renderInquiryCardSemanticLine "Governance state" "Decision" projection.GovernanceState
            renderInquiryCardLine "Primary reason" projection.PrimaryReason

            match projection.RecommendedNextStep with
            | None -> empty()
            | Some step -> renderInquiryCardLine "Recommended next step" step

            renderInquiryCardSemanticLine "Evidence status" "Evidence" projection.EvidenceStatus
            renderInquiryCardLine "Ledger status" projection.LedgerStatus
        }

        div {
            attr.``class`` "tags mb-4"
            renderInquiryCountTag "Primary facts" projection.PrimaryFactCount
            renderInquiryCountTag "Additional facts" projection.AdditionalFactCount
            renderInquiryCountTag "Source Phi" projection.SourcePhiCount
            renderInquiryCountTag "Ledger events" projection.LedgerEventCount
        }

        div {
            attr.``class`` "field is-grouped"
            div {
                attr.``class`` "control"
                button {
                    attr.``class`` "button is-small is-light"
                    attr.``type`` "button"
                    on.click (fun _ -> dispatch (SetFactsReconstructionDisplayMode factsReconstructionDisplayModeFullReport))
                    text "Open full report"
                }
            }
            div {
                attr.``class`` "control"
                p {
                    attr.``class`` "help"
                    text "Full report keeps the reconstruction details and answer fact tables available."
                }
            }
        }
    }

let private renderResultPanel (result: FactsReconstructionResult) (answer: InquiryAnswer) =
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

        renderResultSection "Answer Facts" (fun () -> renderAnswerFactsPreview answer)
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
                    div {
                        attr.``class`` "field mb-0 mr-3"
                        label {
                            attr.``class`` "label is-small mb-1"
                            text "Display"
                        }
                        div {
                            attr.``class`` "select is-small"
                            select {
                                bind.input.string model.factsReconstructionDisplayMode (fun value -> dispatch (SetFactsReconstructionDisplayMode value))
                                forEach factsReconstructionDisplayModes <| fun displayMode ->
                                    option { text displayMode }
                            }
                        }
                    }
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
            let answer = profiledInquiryAnswerFromResult result

            if model.factsReconstructionDisplayMode = factsReconstructionDisplayModeFullReport then
                renderResultPanel result answer
            else
                renderInquiryCard result answer dispatch
    }
