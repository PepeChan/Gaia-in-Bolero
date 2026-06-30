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

let countCandidateGroupStatuses status candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis =
    candidateDeltas
    |> List.sumBy (fun candidate ->
        let governance = buildCandidateGroupGovernance candidate candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

        if governance.Status = status then
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

let renderT5GovernanceSummaryTable sigmaContext (candidateDecisions: CandidateDecision list) sigmaBasisItemDecisions sequencedParsedPhis ledgerEvents dispatch =
    let candidateDeltas = formulateCandidateDeltas sigmaContext
    let pendingCount = countCandidateGroupStatuses GroupPending candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let acceptedCount = countCandidateGroupStatuses GroupAccepted candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let rejectedCount = countCandidateGroupStatuses GroupRejected candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let heldCount = countCandidateGroupStatuses GroupHeld candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let mixedCount = countCandidateGroupStatuses GroupMixed candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let partiallyGovernedCount =
        countCandidateGroupStatuses GroupPartiallyGoverned candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "T5 Governance Summary"
        }

        div {
            attr.``class`` "tags mb-3"
            renderCandidateDecisionCount "Group pending" pendingCount
            renderCandidateDecisionCount "Group accepted" acceptedCount
            renderCandidateDecisionCount "Group rejected" rejectedCount
            renderCandidateDecisionCount "Group held" heldCount
            renderCandidateDecisionCount "Mixed" mixedCount
            renderCandidateDecisionCount "Partially governed" partiallyGovernedCount
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
                        th { text "Basis-derived status" }
                        th { text "Class decision" }
                        th { text "Meaning" }
                        th { text "Action" }
                    }
                }

                tbody {
                    forEach candidateDeltas <| fun candidate ->
                        let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions
                        let candidateDecision = tryFindCandidateDecision candidate.CandidateId candidateDecisions
                        let groupGovernance =
                            buildCandidateGroupGovernance candidate candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

                        tr {
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { text (string (List.length candidate.RelevantSigmaBasis)) }
                            td { text candidate.Provenance }
                            td { renderCandidateGroupStatusTag groupGovernance.Status }
                            td { renderCandidateClassDecisionTag candidateDecision }
                            td {
                                p {
                                    attr.``class`` "is-size-7 mb-1"
                                    text groupGovernance.Explanation
                                }
                                match groupGovernance.ConflictExplanation with
                                | None -> empty()
                                | Some conflict ->
                                    p {
                                        attr.``class`` "is-size-7 has-text-warning-dark mb-0"
                                        text conflict
                                    }
                                let resetCount =
                                    ledgerEvents
                                    |> getParseAmendmentResetEventsForCandidate candidate.CandidateId
                                    |> List.length

                                if resetCount > 0 then
                                    p {
                                        attr.``class`` "is-size-7 has-text-warning-dark mb-0"
                                        text ("Parse amendment reset basis decisions for this group: " + string resetCount)
                                    }
                            }
                            td { renderCandidateGovernanceActions candidate decisionValue dispatch }
                        }
                }
            }
        }

        p {
            attr.``class`` "is-size-7 has-text-grey"
            text "T5 records class and basis-item governance decisions only. Human-facing group status is derived from basis-item decisions. Candidate promotion to Sigma is intentionally not performed here."
        }
    }

type ParsedAtomCandidateLink =
    {
        Candidate: CandidateDelta
        BasisItem: SigmaBasisItemReview
        GroupGovernance: CandidateGroupGovernance
        ClassDecision: CandidateDecisionValue
        BasisDecision: CandidateDecisionValue
    }

type ParsedAtomReviewRow =
    {
        AtomKind: string
        AtomText: string
        SourcePhiId: string
        SourcePhiStatement: string
        Provenance: string
        CandidateId: string option
        CandidateGovernanceStatus: string
        CandidateLinks: ParsedAtomCandidateLink list
    }

let formatAffectedParseAmendmentGroups oldKind newKind =
    [ oldKind; newKind ]
    |> List.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
    |> List.distinct
    |> String.concat ", "

let tryFindParsedAtomCandidateStatus
    candidateDeltas
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions
    sequencedParsedPhis
    atomKind
    atomText
    sourcePhiId =
    candidateDeltas
    |> List.tryPick (fun candidate ->
        let matchingBasisItem =
            buildSigmaBasisItemReviews candidate sequencedParsedPhis
            |> List.tryFind (fun basisItem ->
                basisItem.Kind = atomKind
                && basisItem.AtomValue = atomText
                && (basisItem.SupportingPhiIds |> List.contains sourcePhiId))

        matchingBasisItem
        |> Option.map (fun _ ->
            let groupGovernance =
                buildCandidateGroupGovernance candidate candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

            let classDecision = getCandidateDecisionValue candidate.CandidateId candidateDecisions

            candidate.CandidateId,
            ("Group "
             + formatCandidateGroupStatus groupGovernance.Status
             + "; class "
             + formatCandidateDecisionValue classDecision)))

let private parsedAtomTextEquals (left: string) (right: string) =
    let leftText = if isNull left then "" else left.Trim()
    let rightText = if isNull right then "" else right.Trim()

    String.Equals(leftText, rightText, StringComparison.OrdinalIgnoreCase)

let private parsedAtomSourceMatches (sourcePhiId: string) (supportingPhiIds: string list) =
    supportingPhiIds
    |> List.exists (fun supportingPhiId -> String.Equals(supportingPhiId, sourcePhiId, StringComparison.OrdinalIgnoreCase))

let buildParsedAtomCandidateLinks
    candidateDeltas
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions
    sequencedParsedPhis
    atomKind
    atomText
    sourcePhiId =
    candidateDeltas
    |> List.collect (fun candidate ->
        let groupGovernance =
            buildCandidateGroupGovernance candidate candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

        let classDecision = getCandidateDecisionValue candidate.CandidateId candidateDecisions

        buildSigmaBasisItemReviews candidate sequencedParsedPhis
        |> List.filter (fun basisItem ->
            basisItem.Kind = atomKind
            && parsedAtomTextEquals basisItem.AtomValue atomText
            && parsedAtomSourceMatches sourcePhiId basisItem.SupportingPhiIds)
        |> List.map (fun basisItem ->
            {
                Candidate = candidate
                BasisItem = basisItem
                GroupGovernance = groupGovernance
                ClassDecision = classDecision
                BasisDecision = getSigmaBasisItemDecisionValue basisItem.Key sigmaBasisItemDecisions
            }))

let formatParsedAtomCandidateGovernanceStatus candidateLinks =
    match candidateLinks with
    | [] -> "No current candidate group"
    | links ->
        links
        |> List.distinctBy (fun link -> link.Candidate.CandidateId)
        |> List.map (fun link ->
            formatCandidateDeltaKind link.Candidate.Kind
            + " / group "
            + formatCandidateGroupStatus link.GroupGovernance.Status
            + "; class "
            + formatCandidateDecisionValue link.ClassDecision)
        |> String.concat " | "

let buildParsedAtomReviewRows
    sequencedParsedPhis
    sigmaContext
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions =
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    sequencedParsedPhis
    |> List.collect (fun (_, parse: PhiParse) ->
        parsedExposureAtomKinds
        |> List.collect (fun atomKind ->
            getExposureAtomValue atomKind parse
            |> splitExposureAtomValues
            |> List.map (fun atomText ->
                let candidateLinks =
                    buildParsedAtomCandidateLinks
                        candidateDeltas
                        candidateDecisions
                        sigmaBasisItemDecisions
                        sequencedParsedPhis
                        atomKind
                        atomText
                        parse.PhiId

                {
                    AtomKind = atomKind
                    AtomText = atomText
                    SourcePhiId = parse.PhiId
                    SourcePhiStatement = parse.Statement
                    Provenance = getExposureProvenance atomKind parse
                    CandidateId =
                        candidateLinks
                        |> List.tryHead
                        |> Option.map (fun link -> link.Candidate.CandidateId)
                    CandidateGovernanceStatus = formatParsedAtomCandidateGovernanceStatus candidateLinks
                    CandidateLinks = candidateLinks
                })))

let renderOptionalCandidateGroupStatus = function
    | None -> text "Not present"
    | Some status -> renderCandidateGroupStatusTag status

let formatParsedAtomKindLabel = function
    | "Function" -> "Capability"
    | "Mode" -> "Use mode"
    | "Interface" -> "Interaction point"
    | "State" -> "Condition"
    | "Host" -> "System element"
    | "Constraint" -> "Rule / limit"
    | atomKind -> atomKind

let formatParsedAtomKindPluralLabel = function
    | "Function" -> "Capabilities"
    | "Mode" -> "Use modes"
    | "Interface" -> "Interaction points"
    | "State" -> "Conditions"
    | "Host" -> "System elements"
    | "Constraint" -> "Rules / limits"
    | atomKind -> atomKind

let renderParseAmendmentPreview (draft: ParseAmendmentDraft) (impact: ParseAmendmentImpactPreview option) =
    div {
        attr.``class`` "notification is-warning is-light py-2"
        p {
            attr.``class`` "mb-1"
            strong { text "Preview impact" }
        }
        p {
            attr.``class`` "mb-1"
            strong { text "Old atom: " }
            text (formatParsedAtomKindLabel draft.OriginalAtomKind + " = " + draft.OriginalAtomText)
        }
        p {
            attr.``class`` "mb-1"
            strong { text "New atom: " }
            text (formatParsedAtomKindLabel draft.ProposedAtomKind + " = " + draft.ProposedAtomText)
        }
        p {
            attr.``class`` "mb-2"
            strong { text "Source Phi: " }
            code { text draft.SourcePhiId }
        }

        match impact with
        | None ->
            p {
                attr.``class`` "mb-0"
                text "Candidate group before/after status is available from the inline selected-kind review panel. T6 stale propagation is not performed in this slice."
            }
        | Some preview ->
            match preview.CandidateGroupImpacts with
            | [] ->
                p {
                    attr.``class`` "mb-2"
                    text "No affected candidate group could be computed for this amendment."
                }
            | impacts ->
                div {
                    attr.``class`` "table-container mb-2"
                    table {
                        attr.``class`` "table is-fullwidth is-narrow"
                        thead {
                            tr {
                                th { text "Candidate group" }
                                th { text "Before" }
                                th { text "After" }
                                th { text "Basis before" }
                                th { text "Basis after" }
                            }
                        }
                        tbody {
                            forEach impacts <| fun candidateImpact ->
                                tr {
                                    td {
                                        p {
                                            attr.``class`` "mb-1"
                                            text (candidateImpact.CandidateType + " / " + candidateImpact.CandidateTarget)
                                        }
                                        code { text candidateImpact.CandidateId }
                                    }
                                    td { renderOptionalCandidateGroupStatus candidateImpact.BeforeStatus }
                                    td { renderOptionalCandidateGroupStatus candidateImpact.AfterStatus }
                                    td {
                                        match candidateImpact.BeforeBasisItemKey with
                                        | None -> text "None"
                                        | Some key -> code { text key }
                                    }
                                    td {
                                        match candidateImpact.AfterBasisItemKey with
                                        | None -> text "None"
                                        | Some key -> code { text key }
                                    }
                                }
                        }
                    }
                }

            match preview.ResetImpacts with
            | [] ->
                p {
                    attr.``class`` "mb-2"
                    text "No existing T4/T5 basis-item decisions are attached to the amended atom, so no basis decisions will be reset."
                }
            | resets ->
                div {
                    attr.``class`` "mb-2"
                    p {
                        attr.``class`` "has-text-weight-semibold mb-1"
                        text "Basis-item decisions to reopen"
                    }
                    forEach resets <| fun resetImpact ->
                        p {
                            attr.``class`` "mb-1"
                            renderCandidateDecisionTag resetImpact.PreviousDecision
                            text (" -> Pending for ")
                            code { text resetImpact.BasisItemKey }
                        }
                }

            p {
                attr.``class`` "mb-0"
                text "T6 stale propagation is intentionally deferred."
            }
    }

let renderParseAmendmentPanel (draft: ParseAmendmentDraft option) status dispatch =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Amend Parsing"
        }

        match status with
        | None -> empty()
        | Some message ->
            div {
                attr.``class`` "notification is-info is-light py-2"
                text message
            }

        match draft with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "No parsed atom selected."
            }
        | Some amendment ->
            div {
                attr.``class`` "box"

                div {
                    attr.``class`` "columns is-variable is-3"

                    div {
                        attr.``class`` "column is-4"
                        p {
                            attr.``class`` "heading mb-1"
                            text "Original kind"
                        }
                        p { text (formatParsedAtomKindLabel amendment.OriginalAtomKind) }
                    }

                    div {
                        attr.``class`` "column is-8"
                        p {
                            attr.``class`` "heading mb-1"
                            text "Original atom"
                        }
                        p { text amendment.OriginalAtomText }
                    }
                }

                p {
                    attr.``class`` "is-size-7 has-text-grey"
                    strong { text "Source Phi: " }
                    code { text amendment.SourcePhiId }
                }

                p {
                    attr.``class`` "mb-3"
                    text amendment.SourcePhiStatement
                }

                div {
                    attr.``class`` "columns is-variable is-3 mb-0"

                    div {
                        attr.``class`` "column is-4"
                        label {
                            attr.``class`` "label is-size-7"
                            text "Proposed kind"
                        }
                        div {
                            attr.``class`` "select is-fullwidth is-small"
                            select {
                                bind.input.string amendment.ProposedAtomKind (fun value -> dispatch (SetParseAmendmentProposedKind value))
                                forEach parsedExposureAtomKinds <| fun atomKind ->
                                    option {
                                        attr.value atomKind
                                        text (formatParsedAtomKindLabel atomKind)
                                    }
                            }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"
                        label {
                            attr.``class`` "label is-size-7"
                            text "Proposed atom text"
                        }
                        input {
                            attr.``class`` "input is-small"
                            bind.input.string amendment.ProposedAtomText (fun value -> dispatch (SetParseAmendmentProposedText value))
                        }
                    }
                }

                div {
                    attr.``class`` "field"
                    label {
                        attr.``class`` "label is-size-7"
                        text "Reason"
                    }
                    textarea {
                        attr.``class`` "textarea"
                        attr.style "min-height: 5rem;"
                        bind.input.string amendment.Reason (fun value -> dispatch (SetParseAmendmentReason value))
                    }
                }

                if amendment.PreviewRequested then
                    renderParseAmendmentPreview amendment None

                div {
                    attr.``class`` "buttons are-small mb-0"
                    button {
                        attr.``class`` "button is-info is-light"
                        attr.``type`` "button"
                        on.click (fun _ -> dispatch PreviewParseAmendment)
                        text "Preview impact"
                    }

                    if amendment.PreviewRequested then
                        button {
                            attr.``class`` "button is-warning"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch ConfirmParseAmendment)
                            text "Confirm amendment"
                        }

                    button {
                        attr.``class`` "button is-light"
                        attr.``type`` "button"
                        on.click (fun _ -> dispatch CancelParseAmendment)
                        text "Cancel"
                    }
                }
            }
    }

let parsedAtomReviewKindLabels =
    parsedExposureAtomKinds
    |> List.map (fun atomKind -> atomKind, formatParsedAtomKindPluralLabel atomKind)

let getParsedAtomReviewKindLabel atomKind =
    parsedAtomReviewKindLabels
    |> List.tryFind (fun (kind, _) -> kind = atomKind)
    |> Option.map snd
    |> Option.defaultValue atomKind

let getSigmaEntriesForParsedAtomKind atomKind sigmaContext =
    match atomKind with
    | "Function" -> sigmaContext.Functions
    | "Mode" -> sigmaContext.Modes
    | "Interface" -> sigmaContext.Interfaces
    | "State" -> sigmaContext.States
    | "Host" -> sigmaContext.Hosts
    | "Constraint" -> sigmaContext.Constraints
    | _ -> []

let parsedAtomReviewButtonClass selectedKind atomKind =
    if selectedKind = Some atomKind then
        "tag is-link is-medium"
    else
        "tag is-light is-medium"

let isParseAmendmentDraftForRow (row: ParsedAtomReviewRow) (draft: ParseAmendmentDraft) =
    draft.SourcePhiId = row.SourcePhiId
    && draft.OriginalAtomKind = row.AtomKind
    && draft.OriginalAtomText = row.AtomText

let renderParseAmendmentInlineEditor amendment candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis dispatch =
    let impactPreview =
        if amendment.PreviewRequested then
            Some (buildParseAmendmentImpactPreview amendment candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis)
        else
            None

    div {
        attr.``class`` "sigma-basis-detail-block"

        div {
            attr.``class`` "columns is-variable is-3 mb-2"

            div {
                attr.``class`` "column is-4"
                p {
                    attr.``class`` "heading mb-1"
                    text "Original kind"
                }
                p { text (formatParsedAtomKindLabel amendment.OriginalAtomKind) }
            }

            div {
                attr.``class`` "column is-8"
                p {
                    attr.``class`` "heading mb-1"
                    text "Original atom"
                }
                p {
                    attr.``class`` "sigma-basis-atom mb-0"
                    text amendment.OriginalAtomText
                }
            }
        }

        p {
            attr.``class`` "is-size-7 has-text-grey mb-1"
            strong { text "Source Phi: " }
            code { text amendment.SourcePhiId }
        }

        p {
            attr.``class`` "is-size-7 has-text-grey mb-3"
            text "The original Phi text remains unchanged; only the parsed exposure atom is amended."
        }

        div {
            attr.``class`` "columns is-variable is-3 mb-0"

            div {
                attr.``class`` "column is-4"
                label {
                    attr.``class`` "label is-size-7"
                    text "Proposed kind"
                }
                div {
                    attr.``class`` "select is-fullwidth is-small"
                    select {
                        bind.input.string amendment.ProposedAtomKind (fun value -> dispatch (SetParseAmendmentProposedKind value))
                        forEach parsedExposureAtomKinds <| fun atomKind ->
                            option {
                                attr.value atomKind
                                text (formatParsedAtomKindLabel atomKind)
                            }
                    }
                }
            }

            div {
                attr.``class`` "column is-8"
                label {
                    attr.``class`` "label is-size-7"
                    text "Proposed atom text"
                }
                input {
                    attr.``class`` "input is-small"
                    bind.input.string amendment.ProposedAtomText (fun value -> dispatch (SetParseAmendmentProposedText value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label is-size-7"
                text "Reason"
            }
            textarea {
                attr.``class`` "textarea is-small"
                attr.style "min-height: 4rem;"
                bind.input.string amendment.Reason (fun value -> dispatch (SetParseAmendmentReason value))
            }
        }

        if amendment.PreviewRequested then
            renderParseAmendmentPreview amendment impactPreview

        div {
            attr.``class`` "buttons are-small mb-0"
            button {
                attr.``class`` "button is-info is-light"
                attr.``type`` "button"
                on.click (fun _ -> dispatch PreviewParseAmendment)
                text "Preview impact"
            }

            if amendment.PreviewRequested then
                button {
                    attr.``class`` "button is-warning"
                    attr.``type`` "button"
                    on.click (fun _ -> dispatch ConfirmParseAmendment)
                    text "Confirm amendment"
                }

            button {
                attr.``class`` "button is-light"
                attr.``type`` "button"
                on.click (fun _ -> dispatch CancelParseAmendment)
                text "Cancel"
            }
        }
    }

let renderAtomCandidateStatusTags candidateLinks =
    match candidateLinks with
    | [] ->
        span {
            attr.``class`` "tag is-light"
            text "No candidate group"
        }
    | links ->
        div {
            attr.``class`` "tags mb-0"
            forEach (links |> List.distinctBy (fun link -> link.Candidate.CandidateId)) <| fun link ->
                concat {
                    span {
                        attr.``class`` "tag is-info is-light model-fitting-status-tag"
                        text (formatCandidateDeltaKind link.Candidate.Kind)
                    }
                    renderCandidateGroupStatusTag link.GroupGovernance.Status
                    renderCandidateDecisionTag link.ClassDecision
                }
        }

let countBasisDecisions decision candidateLinks =
    candidateLinks
    |> List.filter (fun link -> link.BasisDecision = decision)
    |> List.length

let renderAtomBasisStatusTags candidateLinks =
    match candidateLinks with
    | [] ->
        span {
            attr.``class`` "tag is-light"
            text "No basis item"
        }
    | links ->
        div {
            attr.``class`` "tags mb-0"
            span {
                attr.``class`` "tag is-light"
                text ("Basis items: " + string (List.length links))
            }
            span {
                attr.``class`` "tag is-light"
                text ("Pending: " + string (countBasisDecisions Pending links))
            }
            span {
                attr.``class`` "tag is-success is-light"
                text ("Accepted: " + string (countBasisDecisions Accepted links))
            }
            span {
                attr.``class`` "tag is-danger is-light"
                text ("Rejected: " + string (countBasisDecisions Rejected links))
            }
            span {
                attr.``class`` "tag is-warning is-light"
                text ("Held: " + string (countBasisDecisions Held links))
            }
        }

let renderParsedAtomAmendmentControl row amendmentDraft candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis dispatch =
    match amendmentDraft with
    | Some amendment when isParseAmendmentDraftForRow row amendment ->
        renderParseAmendmentInlineEditor amendment candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis dispatch
    | _ ->
        div {
            attr.``class`` "sigma-basis-detail-block"
            p {
                attr.``class`` "has-text-weight-semibold mb-2"
                text "Amend atom kind/value"
            }
            p {
                attr.``class`` "is-size-7 has-text-grey mb-3"
                text "Open an amendment draft for this parsed atom, then preview and confirm the impact from this panel."
            }
            button {
                attr.``class`` "button is-small is-warning is-light"
                attr.``type`` "button"
                on.click (fun _ ->
                    dispatch
                        (StartParseAmendment
                            (row.SourcePhiId, row.AtomKind, row.AtomText, row.Provenance)))
                text "Amend atom"
            }
        }

let renderParsedAtomPhiChips phiIds =
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

let renderParsedAtomBasisItemActions basisItemKey decisionValue dispatch =
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

let renderParsedAtomBasisGovernance row ledgerEvents dispatch =
    div {
        attr.``class`` "sigma-basis-detail-block"

        p {
            attr.``class`` "has-text-weight-semibold mb-2"
            text "Supporting basis governance"
        }

        match row.CandidateLinks with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No T4/T5 basis item currently references this parsed atom."
            }
        | links ->
            div {
                attr.``class`` "sigma-basis-list"
                forEach links <| fun link ->
                    let basisItem = link.BasisItem
                    let resetEvent = tryFindLatestParseAmendmentResetEventForBasisItem basisItem.Key ledgerEvents

                    div {
                        attr.``class`` "model-fitting-governance-row"

                        div {
                            attr.``class`` "columns is-variable is-3 is-vcentered mb-2"

                            div {
                                attr.``class`` "column is-7"
                                p {
                                    attr.``class`` "heading mb-1"
                                    text (formatCandidateDeltaKind link.Candidate.Kind)
                                }
                                p {
                                    attr.``class`` "sigma-basis-atom mb-1"
                                    text (formatParsedAtomKindLabel basisItem.Kind + " = " + basisItem.AtomValue)
                                }
                                renderParsedAtomPhiChips basisItem.SupportingPhiIds
                            }

                            div {
                                attr.``class`` "column is-2"
                                p {
                                    attr.``class`` "heading mb-1"
                                    text "Basis status"
                                }
                                renderCandidateDecisionTag link.BasisDecision
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderParsedAtomBasisItemActions basisItem.Key link.BasisDecision dispatch
                            }
                        }

                        match resetEvent with
                        | None -> empty()
                        | Some ledgerEvent ->
                            div {
                                attr.``class`` "notification is-warning is-light is-size-7 py-2 mb-0"
                                text ledgerEvent.Detail
                            }
                    }
            }
    }

let renderParsedAtomCandidateGovernance row dispatch =
    div {
        attr.``class`` "sigma-basis-detail-block"

        p {
            attr.``class`` "has-text-weight-semibold mb-2"
            text "Candidate group governance"
        }

        match row.CandidateLinks |> List.distinctBy (fun link -> link.Candidate.CandidateId) with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No candidate group currently applies to this atom."
            }
        | links ->
            div {
                attr.``class`` "sigma-basis-list"
                forEach links <| fun link ->
                    div {
                        attr.``class`` "model-fitting-governance-row"

                        div {
                            attr.``class`` "columns is-variable is-3 is-vcentered mb-2"

                            div {
                                attr.``class`` "column is-5"
                                p {
                                    attr.``class`` "heading mb-1"
                                    text "Candidate"
                                }
                                p {
                                    attr.``class`` "mb-1"
                                    text (formatCandidateDeltaKind link.Candidate.Kind)
                                }
                                code { text link.Candidate.CandidateId }
                            }

                            div {
                                attr.``class`` "column is-3"
                                p {
                                    attr.``class`` "heading mb-1"
                                    text "Group status"
                                }
                                renderCandidateGroupStatusTag link.GroupGovernance.Status
                            }

                            div {
                                attr.``class`` "column is-4"
                                renderCandidateGovernanceActions link.Candidate link.ClassDecision dispatch
                            }
                        }

                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-1"
                            text link.GroupGovernance.Explanation
                        }

                        match link.GroupGovernance.ConflictExplanation with
                        | None -> empty()
                        | Some conflict ->
                            p {
                                attr.``class`` "notification is-warning is-light cognition-review-helper is-size-7 mb-0"
                                text conflict
                            }
                    }
            }
    }

let renderParsedAtomReviewRow row amendmentDraft parseAmendmentStatus candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis ledgerEvents dispatch =
    let isActiveAmendment =
        match amendmentDraft with
        | Some amendment -> isParseAmendmentDraftForRow row amendment
        | None -> false

    div {
        attr.``class`` "card mb-3 model-fitting-atom-card"

        div {
            attr.``class`` "card-content"

            div {
                attr.``class`` "columns is-variable is-3 mb-2"

                div {
                    attr.``class`` "column is-3"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Source Phi"
                    }
                    code { text row.SourcePhiId }
                }

                div {
                    attr.``class`` "column is-5"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Current item"
                    }
                    p {
                        attr.``class`` "sigma-basis-atom mb-0"
                        strong { text (formatParsedAtomKindLabel row.AtomKind + ": ") }
                        text row.AtomText
                    }
                }

                div {
                    attr.``class`` "column is-4"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Provenance"
                    }
                    span {
                        attr.``class`` "tag is-info is-light"
                        text row.Provenance
                    }
                }
            }

            div {
                attr.``class`` "model-fitting-raw-preview mb-3"
                p {
                    attr.``class`` "heading mb-1"
                    text "Raw Phi preview"
                }
                p {
                    attr.``class`` "mb-0"
                    text row.SourcePhiStatement
                }
            }

            div {
                attr.``class`` "columns is-variable is-3 mb-2"

                div {
                    attr.``class`` "column is-6"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Candidate status"
                    }
                    renderAtomCandidateStatusTags row.CandidateLinks
                }

                div {
                    attr.``class`` "column is-6"
                    p {
                        attr.``class`` "heading mb-1"
                        text "Supporting basis status"
                    }
                    renderAtomBasisStatusTags row.CandidateLinks
                }
            }

            details {
                attr.``class`` "model-fitting-panel"
                attr.``open`` isActiveAmendment

                summary {
                    attr.``class`` "model-fitting-panel-summary"
                    text "Work on this item"
                }

                div {
                    attr.``class`` "model-fitting-panel-body"

                    match amendmentDraft with
                    | Some amendment when isParseAmendmentDraftForRow row amendment ->
                        match parseAmendmentStatus with
                        | None -> empty()
                        | Some message ->
                            div {
                                attr.``class`` "notification is-info is-light py-2"
                                text message
                            }
                    | _ -> empty()

                    renderParsedAtomAmendmentControl
                        row
                        amendmentDraft
                        candidateDecisions
                        sigmaBasisItemDecisions
                        sequencedParsedPhis
                        dispatch

                    renderParsedAtomBasisGovernance row ledgerEvents dispatch
                    renderParsedAtomCandidateGovernance row dispatch
                }
            }
        }
    }

let renderParsedAtomReviewTable
    sequencedParsedPhis
    sigmaContext
    candidateDecisions
    sigmaBasisItemDecisions
    selectedParsedAtomReviewKind
    parseAmendmentDraft
    parseAmendmentStatus
    ledgerEvents
    dispatch =
    let atomRows = buildParsedAtomReviewRows sequencedParsedPhis sigmaContext candidateDecisions sigmaBasisItemDecisions

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Review Parsed Atoms"
        }

        div {
            attr.``class`` "tags are-medium mb-3"
            forEach parsedAtomReviewKindLabels <| fun (atomKind, label) ->
                let count =
                    atomRows
                    |> List.filter (fun row -> row.AtomKind = atomKind)
                    |> List.length

                button {
                    attr.``class`` (parsedAtomReviewButtonClass selectedParsedAtomReviewKind atomKind)
                    attr.``type`` "button"
                    on.click (fun _ -> dispatch (SelectParsedAtomReviewKind atomKind))
                    text (label + ": " + string count)
                }
        }

        match selectedParsedAtomReviewKind with
        | None ->
            p {
                attr.``class`` "is-size-7 has-text-grey"
                text "Select a kind count above to review just those parsed atoms."
            }
        | Some atomKind ->
            let selectedRows =
                atomRows
                |> List.filter (fun row -> row.AtomKind = atomKind)

            div {
                attr.``class`` "cognition-review-section"

                div {
                    attr.``class`` "level is-mobile mb-3"

                    div {
                        attr.``class`` "level-left"
                        div {
                            h4 {
                                attr.``class`` "title is-6 mb-1"
                                text (getParsedAtomReviewKindLabel atomKind + " Review")
                            }
                            p {
                                attr.``class`` "has-text-grey mb-0"
                                text "Selected-kind parsed atom review"
                            }
                        }
                    }

                    div {
                        attr.``class`` "level-right"
                        div {
                            attr.``class`` "buttons are-small mb-0"
                            span {
                                attr.``class`` "tag is-light"
                                text ("Atom rows: " + string (List.length selectedRows))
                            }
                            button {
                                attr.``class`` "button is-small is-light"
                                attr.``type`` "button"
                                on.click (fun _ -> dispatch ClearParsedAtomReviewKind)
                                text "Close"
                            }
                        }
                    }
                }

                match parseAmendmentStatus with
                | None -> empty()
                | Some message ->
                    div {
                        attr.``class`` "notification is-info is-light py-2"
                        text message
                    }

                match selectedRows with
                | [] ->
                    p {
                        attr.``class`` "has-text-grey"
                        text ("No " + (getParsedAtomReviewKindLabel atomKind).ToLowerInvariant() + " parsed atoms are currently exposed.")
                    }
                | rows ->
                    div {
                        attr.``class`` "sigma-basis-list"
                        forEach rows <| fun row ->
                            renderParsedAtomReviewRow
                                row
                                parseAmendmentDraft
                                parseAmendmentStatus
                                candidateDecisions
                                sigmaBasisItemDecisions
                                sequencedParsedPhis
                                ledgerEvents
                                dispatch
                    }
            }
    }

let cognitionReviewTargetFilters = [ "All"; "Host"; "Interface"; "State"; "Mode"; "Constraint"; "Reinforced Atom" ]
let cognitionReviewDecisionFilters = [ "All"; "Pending"; "Accepted"; "Rejected"; "Held"; "Mixed"; "Partially governed" ]

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
    | "Pending" -> decisionValue = GroupPending
    | "Accepted" -> decisionValue = GroupAccepted
    | "Rejected" -> decisionValue = GroupRejected
    | "Held" -> decisionValue = GroupHeld
    | "Mixed" -> decisionValue = GroupMixed
    | "Partially governed" -> decisionValue = GroupPartiallyGoverned
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

let getFilteredReviewCandidates (model: Model) sigmaContext sequencedParsedPhis =
    let searchText = normalizeReviewText model.cognitionReviewTextFilter

    formulateCandidateDeltas sigmaContext
    |> List.filter (fun candidate ->
        let groupGovernance =
            buildCandidateGroupGovernance candidate model.candidateDecisions model.sigmaBasisItemDecisions sequencedParsedPhis

        candidateMatchesTargetFilter model.cognitionReviewTargetFilter candidate
        && candidateMatchesDecisionFilter model.cognitionReviewDecisionFilter groupGovernance.Status
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

let renderSigmaBasisItemReview (basisItem: SigmaBasisItemReview) (sigmaBasisItemDecisions: Map<string, CandidateDecisionValue>) ledgerEvents dispatch =
    let decisionValue = getSigmaBasisItemDecisionValue basisItem.Key sigmaBasisItemDecisions
    let resetEvent = tryFindLatestParseAmendmentResetEventForBasisItem basisItem.Key ledgerEvents

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

        match resetEvent with
        | None -> empty()
        | Some ledgerEvent ->
            div {
                attr.``class`` "notification is-warning is-light is-size-7 py-2 mb-2"
                p {
                    attr.``class`` "has-text-weight-semibold mb-1"
                    text "Decision reset by parsed atom amendment"
                }
                p {
                    attr.``class`` "mb-0"
                    text ledgerEvent.Detail
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
                text "Group status"
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
    ledgerEvents
    dispatch =
    let decisionValue = getCandidateDecisionValue candidate.CandidateId candidateDecisions
    let candidateDecision = tryFindCandidateDecision candidate.CandidateId candidateDecisions
    let basisItems = buildSigmaBasisItemReviews candidate sequencedParsedPhis
    let basisItemKeys = basisItems |> List.map (fun basisItem -> basisItem.Key)
    let groupGovernance = buildCandidateGroupGovernance candidate candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis

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
                        text "Group status"
                    }
                    renderCandidateGroupStatusTag groupGovernance.Status
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
                            text "Section 1: Candidate group governance"
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
                            strong { text "Basis-derived status: " }
                            renderCandidateGroupStatusTag groupGovernance.Status
                        }

                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-2"
                            text groupGovernance.Explanation
                        }

                        match groupGovernance.ConflictExplanation with
                        | None -> empty()
                        | Some conflict ->
                            p {
                                attr.``class`` "notification is-warning is-light cognition-review-helper is-size-7"
                                text conflict
                            }

                        renderCandidateAmendmentResetNotice candidate ledgerEvents

                        p {
                            attr.``class`` "mb-2"
                            strong { text "Candidate class decision: " }
                            renderCandidateClassDecisionTag candidateDecision
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
                    text "Basis item review is the finer-grained governance layer. The candidate group status above is derived from these item decisions."
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
                            renderSigmaBasisItemReview basisItem sigmaBasisItemDecisions ledgerEvents dispatch
                    }
            }
        }
    }

let private tryExtractLedgerDetailField fieldName (detail: string) =
    if String.IsNullOrWhiteSpace(detail) then
        None
    else
        let prefix = fieldName + ": "

        detail.Split([| " | " |], StringSplitOptions.None)
        |> Array.tryPick (fun part ->
            if part.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
                Some (part.Substring(prefix.Length).Trim())
            else
                None)

let private formatParseAmendmentResetSummary (ledgerEvent: LedgerEvent) =
    let atomKind =
        ledgerEvent.Detail
        |> tryExtractLedgerDetailField "Original kind"
        |> Option.defaultValue "Basis item"

    let atomValue =
        ledgerEvent.Detail
        |> tryExtractLedgerDetailField "Original text"
        |> Option.defaultValue ledgerEvent.TargetId

    atomKind + " decision reopened: \"" + atomValue + "\""

let renderRecentParseAmendmentResetEvents ledgerEvents =
    let resetEvents =
        ledgerEvents
        |> getParseAmendmentResetLedgerEvents
        |> List.rev
        |> List.truncate 5

    match resetEvents with
    | [] -> empty()
    | events ->
        div {
            attr.``class`` "notification is-warning is-light cognition-review-helper"
            p {
                attr.``class`` "has-text-weight-semibold mb-2"
                text "Basis decisions reopened by parse amendment"
            }
            forEach events <| fun ledgerEvent ->
                div {
                    p {
                        attr.``class`` "is-size-7 has-text-weight-semibold mb-1"
                        text (formatParseAmendmentResetSummary ledgerEvent)
                    }
                    p {
                        attr.``class`` "is-size-7 has-text-grey mb-2"
                        text "Basis key: "
                        code { text ledgerEvent.TargetId }
                    }
                }
        }

let renderCognitionReviewPanel (model: Model) sequencedParsedPhis sigmaContext dispatch =
    let reviewCandidates = getFilteredReviewCandidates model sigmaContext sequencedParsedPhis

    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "Cognition Review"
        }

        p {
            attr.``class`` "is-size-7 has-text-grey mb-4"
            text "Candidate-class decisions persist. Basis-derived group status is the human-facing governance state."
        }

        renderRecentParseAmendmentResetEvents model.LedgerEvents

        div {
            attr.``class`` "columns is-variable is-4"
            renderTopMissingContextReview sequencedParsedPhis
            renderTopArchitecturalPressuresReview sigmaContext
            renderTopReinforcedAtomsReview sigmaContext
        }

        hr { attr.``class`` "my-4" }

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
                    model.LedgerEvents
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

let getAllCandidateBasisItems candidateDeltas sequencedParsedPhis =
    candidateDeltas
    |> List.collect (fun candidate -> buildSigmaBasisItemReviews candidate sequencedParsedPhis)
    |> List.distinctBy (fun basisItem -> basisItem.Key)

let countBasisItemsWithDecision decision candidateDeltas sigmaBasisItemDecisions sequencedParsedPhis =
    getAllCandidateBasisItems candidateDeltas sequencedParsedPhis
    |> List.filter (fun basisItem -> getSigmaBasisItemDecisionValue basisItem.Key sigmaBasisItemDecisions = decision)
    |> List.length

let renderModelFittingSummaryCounts
    atomRows
    candidateDeltas
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions
    sequencedParsedPhis =
    let candidateCount = List.length candidateDeltas
    let pendingCount = countCandidateGroupStatuses GroupPending candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let acceptedCount = countCandidateGroupStatuses GroupAccepted candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let rejectedCount = countCandidateGroupStatuses GroupRejected candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let heldCount = countCandidateGroupStatuses GroupHeld candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let mixedCount = countCandidateGroupStatuses GroupMixed candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let partiallyGovernedCount =
        countCandidateGroupStatuses GroupPartiallyGoverned candidateDeltas candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis
    let basisItemCount = getAllCandidateBasisItems candidateDeltas sequencedParsedPhis |> List.length

    div {
        attr.``class`` "tags are-medium mb-4"
        renderCandidateDecisionCount "Parsed atoms" (List.length atomRows)
        renderCandidateDecisionCount "Candidate groups" candidateCount
        renderCandidateDecisionCount "Group pending" pendingCount
        renderCandidateDecisionCount "Group accepted" acceptedCount
        renderCandidateDecisionCount "Group rejected" rejectedCount
        renderCandidateDecisionCount "Group held" heldCount
        renderCandidateDecisionCount "Mixed" mixedCount
        renderCandidateDecisionCount "Partially governed" partiallyGovernedCount
        renderCandidateDecisionCount "Basis items" basisItemCount
        renderCandidateDecisionCount "Basis pending" (countBasisItemsWithDecision Pending candidateDeltas sigmaBasisItemDecisions sequencedParsedPhis)
        renderCandidateDecisionCount "Basis accepted" (countBasisItemsWithDecision Accepted candidateDeltas sigmaBasisItemDecisions sequencedParsedPhis)
        renderCandidateDecisionCount "Basis rejected" (countBasisItemsWithDecision Rejected candidateDeltas sigmaBasisItemDecisions sequencedParsedPhis)
        renderCandidateDecisionCount "Basis held" (countBasisItemsWithDecision Held candidateDeltas sigmaBasisItemDecisions sequencedParsedPhis)
    }

let renderCollapsedIntermediateSummaries
    sequencedParsedPhis
    sigmaContext
    lastReplayAction =
    details {
        attr.``class`` "model-fitting-summary-details"

        summary {
            attr.``class`` "model-fitting-panel-summary"
            text "Intermediate summaries"
        }

        div {
            attr.``class`` "model-fitting-panel-body"
            renderCurrentSigmaSummaryTable sigmaContext
            renderMissingContextSummaryTable sequencedParsedPhis
            renderArchitecturalPressureSummaryTable sigmaContext
            renderTopReinforcedAtomsTable sigmaContext
            renderT4CandidateSummaryTable sigmaContext
            renderLatestDeltaSummaryTable lastReplayAction
        }
    }

let renderModelFittingWorkspace
    sequencedParsedPhis
    sigmaContext
    lastReplayAction
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions
    ledgerEvents
    selectedParsedAtomReviewKind
    parseAmendmentDraft
    parseAmendmentStatus
    dispatch =
    let atomRows = buildParsedAtomReviewRows sequencedParsedPhis sigmaContext candidateDecisions sigmaBasisItemDecisions
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    div {
        attr.``class`` "box model-fitting-workspace"

        h2 {
            attr.``class`` "title is-5"
            text "Model Fitting"
        }

        p {
            attr.``class`` "is-size-7 has-text-grey mb-4"
            text "Parsed atom review, amendment, candidate governance, and basis-item governance are handled from the atom cards below."
        }

        renderModelFittingSummaryCounts
            atomRows
            candidateDeltas
            candidateDecisions
            sigmaBasisItemDecisions
            sequencedParsedPhis

        renderRecentParseAmendmentResetEvents ledgerEvents

        renderParsedAtomReviewTable
            sequencedParsedPhis
            sigmaContext
            candidateDecisions
            sigmaBasisItemDecisions
            selectedParsedAtomReviewKind
            parseAmendmentDraft
            parseAmendmentStatus
            ledgerEvents
            dispatch

        renderCollapsedIntermediateSummaries
            sequencedParsedPhis
            sigmaContext
            lastReplayAction
    }

let renderOperationalSummaryTablesPanel
    sequencedParsedPhis
    sigmaContext
    lastReplayAction
    (candidateDecisions: CandidateDecision list)
    sigmaBasisItemDecisions
    ledgerEvents
    selectedParsedAtomReviewKind
    parseAmendmentDraft
    parseAmendmentStatus
    dispatch =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "Summary Tables"
        }

        renderCurrentSigmaSummaryTable sigmaContext

        renderParsedAtomReviewTable
            sequencedParsedPhis
            sigmaContext
            candidateDecisions
            sigmaBasisItemDecisions
            selectedParsedAtomReviewKind
            parseAmendmentDraft
            parseAmendmentStatus
            ledgerEvents
            dispatch

        renderMissingContextSummaryTable sequencedParsedPhis

        renderArchitecturalPressureSummaryTable sigmaContext

        renderTopReinforcedAtomsTable sigmaContext

        renderT4CandidateSummaryTable sigmaContext

        renderT5GovernanceSummaryTable sigmaContext candidateDecisions sigmaBasisItemDecisions sequencedParsedPhis ledgerEvents dispatch

        renderLatestDeltaSummaryTable lastReplayAction
    }
