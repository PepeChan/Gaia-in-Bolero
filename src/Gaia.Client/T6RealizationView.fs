module Gaia.Client.T6RealizationView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.EvidenceView
open Gaia.Client.ImpactProjection
open Gaia.Client.Realization
open Gaia.Client.RealizationInquiryEngine
open Gaia.Client.Workflow

let private formatNone (values: string list) =
    match values with
    | [] -> "None"
    | _ -> String.concat ", " values

let private formatIdName (id: string) (name: string) =
    id + " - " + name

let private renderOption (value: string) (label: string) =
    option {
        attr.value value
        text label
    }

let private renderStatusNotification status =
    match status with
    | None -> empty()
    | Some message ->
        div {
            attr.``class`` "notification is-info is-light"
            text message
        }

let private realizationStatusTagClass status =
    match status with
    | "Not realized" -> "tag is-light"
    | "Part linked" -> "tag is-warning is-light"
    | "DP linked" -> "tag is-info is-light"
    | "TF linked" -> "tag is-link is-light"
    | "CTQ linked" -> "tag is-primary is-light"
    | "Continuity complete" -> "tag is-success is-light"
    | "Partially realized" -> "tag is-warning is-light"
    | "Realization started" -> "tag is-info is-light"
    | "Behavior linked" -> "tag is-link is-light"
    | "Verification path started" -> "tag is-success is-light"
    | _ -> "tag is-light"

let private renderRealizationStatusTag status =
    span {
        attr.``class`` (realizationStatusTagClass status)
        text status
    }

let private renderT6ReviewStatusTag reviewNeeded =
    span {
        attr.``class`` (if reviewNeeded then "tag is-warning is-light" else "tag is-success is-light")
        text (if reviewNeeded then "Review Needed" else "Confirmed")
    }

let private readinessBadgeClass readiness =
    match readiness with
    | Missing -> "readiness-badge readiness-missing"
    | Partial -> "readiness-badge readiness-partial"
    | Complete -> "readiness-badge readiness-complete"

let private semanticObjectClass objectKind =
    tryGetCognopyObjectClass objectKind
    |> Option.defaultValue "cognopy-object-kind"

let private semanticObjectRowClass objectKind =
    tryGetCognopyObjectRowClass objectKind
    |> Option.defaultValue ""

let private renderObjectKindTag objectKind =
    span {
        attr.``class`` (semanticObjectClass objectKind)
        text objectKind
    }

let private renderReadinessBadge prefix showLabel readiness =
    let label =
        if showLabel then
            getReadinessLabel readiness
        else
            ""

    let body =
        match prefix, label with
        | "", "" -> ""
        | "", value -> value
        | value, "" -> value
        | prefixValue, labelValue -> prefixValue + " " + labelValue

    span {
        attr.``class`` (readinessBadgeClass readiness)
        span {
            attr.``class`` "readiness-symbol"
            text (getReadinessSymbol readiness)
        }

        if body <> "" then
            text body
    }

let private renderReviewNeededBadge () =
    span {
        attr.``class`` "tag is-warning is-light review-needed-badge"
        text reviewNeededLabel
    }

let private renderReviewNeededBadgeIf needsReview =
    if needsReview then
        span {
            attr.``class`` "ml-1"
            renderReviewNeededBadge ()
        }
    else
        empty()

let private realizationObjectNeedsReview objectKind objectId reviewNeededMarks =
    reviewNeededMarks
    |> hasReviewNeededMark reviewTargetKindRealizationObject (realizationObjectReviewTargetId objectKind objectId)

let private realizationLinkNeedsReview linkKind sourceId targetId reviewNeededMarks =
    reviewNeededMarks
    |> hasReviewNeededMark reviewTargetKindRealizationLink (realizationLinkReviewTargetId linkKind sourceId targetId)

let private realizationPathNeedsReview sourceKind sourceValue reviewNeededMarks =
    reviewNeededMarks
    |> hasReviewNeededMark reviewTargetKindRealizationPath (realizationPathReviewTargetId sourceKind sourceValue)

let private renderObjectReadiness objectKind readiness =
    concat {
        renderObjectKindTag objectKind
        text " "
        renderReadinessBadge "" true readiness
    }

let private formatTraceObjectLabel (node: RealizationTraceNode) =
    if node.ObjectName = "" then
        node.ObjectId
    else
        node.ObjectId + " " + node.ObjectName

let private renderReadinessCell objectKind readiness values =
    td {
        renderObjectReadiness objectKind readiness
        div {
            attr.``class`` "readiness-cell-detail"
            text (formatNone values)
        }
    }

let private renderCreateObjectForm (model: Model) dispatch =
    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Create Realization Object"
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Object kind"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.realizationObjectKindDraft (fun value -> dispatch (SetRealizationObjectKindDraft value))
                        forEach realizationObjectKinds <| fun kind ->
                            renderOption kind kind
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Id"
            }
            div {
                attr.``class`` "control"
                input {
                    attr.``class`` "input"
                    attr.placeholder "FR-001, DP-001, PART-001..."
                    bind.input.string model.realizationObjectIdDraft (fun value -> dispatch (SetRealizationObjectIdDraft value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Name"
            }
            div {
                attr.``class`` "control"
                input {
                    attr.``class`` "input"
                    attr.placeholder "Short engineering object name"
                    bind.input.string model.realizationObjectNameDraft (fun value -> dispatch (SetRealizationObjectNameDraft value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Description"
            }
            div {
                attr.``class`` "control"
                textarea {
                    attr.``class`` "textarea"
                    attr.placeholder "Optional"
                    bind.input.string model.realizationObjectDescriptionDraft (fun value -> dispatch (SetRealizationObjectDescriptionDraft value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Source / note"
            }
            div {
                attr.``class`` "control"
                input {
                    attr.``class`` "input"
                    attr.placeholder "Optional manual source, note, or rationale"
                    bind.input.string model.realizationObjectSourceNoteDraft (fun value -> dispatch (SetRealizationObjectSourceNoteDraft value))
                }
            }
        }

        button {
            attr.``class`` "button is-link is-fullwidth"
            attr.``type`` "button"
            on.click (fun _ -> dispatch CreateRealizationObject)
            text "Create Object"
        }
    }

let private renderSelectOptions (placeholder: string) (options: (string * string) list) =
    concat {
        option {
            attr.value ""
            text placeholder
        }
        forEach options <| fun (value, label) ->
            renderOption value label
    }

let private renderCreateLinkForm (model: Model) dispatch =
    let sourceOptions = getRealizationLinkSourceOptions model.realizationLinkKindDraft model
    let targetOptions = getRealizationLinkTargetOptions model.realizationLinkKindDraft model

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Create Realization Link"
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Link kind"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.realizationLinkKindDraft (fun value -> dispatch (SetRealizationLinkKindDraft value))
                        forEach realizationLinkKinds <| fun kind ->
                            renderOption kind kind
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Source"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.realizationLinkSourceIdDraft (fun value -> dispatch (SetRealizationLinkSourceIdDraft value))
                        renderSelectOptions "Select source" sourceOptions
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Target"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.realizationLinkTargetIdDraft (fun value -> dispatch (SetRealizationLinkTargetIdDraft value))
                        renderSelectOptions "Select target" targetOptions
                    }
                }
            }
        }

        if List.isEmpty sourceOptions || List.isEmpty targetOptions then
            p {
                attr.``class`` "is-size-7 has-text-grey mb-3"
                text "Create the needed source and target objects, or parse/govern Hosts and Functions first."
            }

        button {
            attr.``class`` "button is-link is-light is-fullwidth"
            attr.``type`` "button"
            on.click (fun _ -> dispatch CreateRealizationLink)
            text "Create Link"
        }
    }

let private renderHostCompletenessTable (model: Model) =
    let hostEntries = getRealizationSourceHosts model
    let state = model.realizationState

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Host Realization Completeness"
        }

        match hostEntries with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No known Host atoms are available yet."
            }
        | entries ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Host" }
                            th { text "Support" }
                            th { text "Parts" }
                            th { text "DPs" }
                            th { text "TFs" }
                            th { text "CTQs" }
                            th { text "VVs" }
                            th { text "Overall" }
                            th { text "Status" }
                        }
                    }

                    tbody {
                        forEach entries <| fun entry ->
                            let partIds, dpIds, tfIds, ctqIds, vvIds = getHostContinuityIds entry.Value state
                            let status = getHostRealizationStatus entry.Value state
                            let readiness = getHostReadiness entry.Value state
                            let needsReview =
                                model.reviewNeededMarks
                                |> realizationPathNeedsReview realizationSourceKindHost entry.Value

                            tr {
                                td {
                                    text entry.Value
                                    renderReviewNeededBadgeIf needsReview
                                }
                                td { text (string entry.SupportCount) }
                                renderReadinessCell realizationObjectKindPart readiness.Part partIds
                                renderReadinessCell realizationObjectKindDP readiness.DP dpIds
                                renderReadinessCell realizationObjectKindTF readiness.TF tfIds
                                renderReadinessCell realizationObjectKindCTQ readiness.CTQ ctqIds
                                renderReadinessCell realizationObjectKindVV readiness.VV vvIds
                                td { renderReadinessBadge "" true readiness.Overall }
                                td { renderRealizationStatusTag status }
                            }
                    }
                }
            }
    }

let private renderGapRow (label: string) objectKind readiness (values: string list) =
    tr {
        td { text label }
        td { renderObjectReadiness objectKind readiness }
        td { text (string (List.length values)) }
        td { text (formatNone values) }
    }

let private renderT6Summary (model: Model) =
    let state = model.realizationState
    let hostEntries = getRealizationSourceHosts model
    let hostsWithoutParts = getHostsWithoutParts hostEntries state |> List.map (fun entry -> entry.Value)
    let partsWithoutDPs = getPartsWithoutDPs state |> List.map (fun item -> formatIdName item.Id item.Name)
    let dpsWithoutTFs = getDPsWithoutTFs state |> List.map (fun item -> formatIdName item.Id item.Name)
    let tfsWithoutCTQs = getTFsWithoutCTQs state |> List.map (fun item -> formatIdName item.Id item.Name)
    let ctqsWithoutVV = getCTQsWithoutVV state |> List.map (fun item -> formatIdName item.Id item.Name)
    let hostReadiness = getGapReadiness (List.length hostEntries) (List.length hostsWithoutParts)
    let partReadiness = getGapReadiness (List.length state.Sigma.Parts) (List.length partsWithoutDPs)
    let dpReadiness = getGapReadiness (List.length state.Sigma.DPs) (List.length dpsWithoutTFs)
    let tfReadiness = getGapReadiness (List.length state.Sigma.TFs) (List.length tfsWithoutCTQs)
    let ctqReadiness = getGapReadiness (List.length state.Sigma.CTQs) (List.length ctqsWithoutVV)
    let reviewNeededCount =
        model.reviewNeededMarks
        |> Gaia.Client.Workflow.getRealizationReviewNeededMarks
        |> List.length

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "T6 Summary"
        }

        if reviewNeededCount > 0 then
            div {
                attr.``class`` "tags mb-3"
                span {
                    attr.``class`` "tag is-warning is-light"
                    text (reviewNeededLabel + ": " + string reviewNeededCount)
                }
            }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Gap" }
                        th { text "Readiness" }
                        th { text "Count" }
                        th { text "Items" }
                    }
                }

                tbody {
                    renderGapRow "Hosts without Parts" realizationObjectKindPart hostReadiness hostsWithoutParts
                    renderGapRow "Parts without DPs" realizationObjectKindDP partReadiness partsWithoutDPs
                    renderGapRow "DPs without TFs" realizationObjectKindTF dpReadiness dpsWithoutTFs
                    renderGapRow "TFs without CTQs" realizationObjectKindCTQ tfReadiness tfsWithoutCTQs
                    renderGapRow "CTQs without VV" realizationObjectKindVV ctqReadiness ctqsWithoutVV
                }
            }
        }
    }

let private formatT6ImpactObjectLabel (state: RealizationState) (impact: T6RealizationReviewImpact) =
    let objectName = getRealizationObjectName impact.AffectedObjectKind impact.AffectedObjectId state

    if String.IsNullOrWhiteSpace(objectName) then
        impact.AffectedObjectKind + " " + impact.AffectedObjectId
    else
        impact.AffectedObjectKind + " " + impact.AffectedObjectId + " - " + objectName

let private formatT6ImpactLinkLabel (impact: T6RealizationReviewImpact) =
    impact.AffectedLinkKind + ": " + impact.AffectedLinkSourceId + " -> " + impact.AffectedLinkTargetId

let private renderT6ImpactSummary (model: Model) =
    let state = model.realizationState
    let impacts = getT6ReviewNeededImpactsFromLedger model.LedgerEvents

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "T6 Impact Summary"
        }

        match impacts with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No T6 realization paths currently need review."
            }
        | values ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Status" }
                            th { text "Affected object" }
                            th { text "Affected link" }
                            th { text "Upstream atom" }
                            th { text "Upstream candidate" }
                            th { text "Supporting Phi" }
                            th { text "Reason" }
                        }
                    }

                    tbody {
                        forEach values <| fun impact ->
                            tr {
                                td { renderT6ReviewStatusTag true }
                                td { text (formatT6ImpactObjectLabel state impact) }
                                td { text (formatT6ImpactLinkLabel impact) }
                                td { text (impact.UpstreamAtomKind + ": \"" + impact.UpstreamAtomValue + "\"") }
                                td { text impact.UpstreamCandidate }
                                td {
                                    code { text impact.SupportingPhiId }

                                    if not (String.IsNullOrWhiteSpace(impact.SupportingPhiStatement)) then
                                        div {
                                            attr.``class`` "is-size-7 has-text-grey mt-1"
                                            text impact.SupportingPhiStatement
                                        }
                                }
                                td { text impact.Reason }
                            }
                    }
                }
            }
    }

let private renderObjectRow
    (state: RealizationState)
    reviewNeededMarks
    (reviewImpacts: T6RealizationReviewImpact list)
    (objectKind: string, objectId: string, objectName: string)
    =
    let note = tryFindObjectNote objectKind objectId state
    let readiness = getRealizationObjectReadiness objectKind objectId state
    let needsReview =
        reviewNeededMarks
        |> realizationObjectNeedsReview objectKind objectId
    let reviewNeeded = needsReview || t6ReviewNeededImpactsAffectObject objectKind objectId reviewImpacts

    tr {
        attr.``class`` (semanticObjectRowClass objectKind)
        td {
            renderObjectReadiness objectKind readiness
            renderReviewNeededBadgeIf needsReview
        }
        td { renderT6ReviewStatusTag reviewNeeded }
        td { code { text objectId } }
        td { text objectName }
        td {
            match note with
            | None -> text ""
            | Some value -> text value.Description
        }
        td {
            match note with
            | None -> text ""
            | Some value -> text value.SourceNote
        }
    }

let private getObjectRows (state: RealizationState) : (string * string * string) list =
    [
        yield! state.Sigma.FRs |> List.map (fun item -> realizationObjectKindFR, item.Id, item.Name)
        yield! state.Sigma.DPs |> List.map (fun item -> realizationObjectKindDP, item.Id, item.Name)
        yield! state.Sigma.TFs |> List.map (fun item -> realizationObjectKindTF, item.Id, item.Name)
        yield! state.Sigma.CTQs |> List.map (fun item -> realizationObjectKindCTQ, item.Id, item.Name)
        yield! state.Sigma.Parts |> List.map (fun item -> realizationObjectKindPart, item.Id, item.Name)
        yield! state.VVItems |> List.map (fun item -> realizationObjectKindVV, item.Id, item.Name)
    ]

let private renderNavigationMissingNext missingKind =
    div {
        attr.``class`` "mt-1"
        renderReadinessBadge ("Missing " + missingKind) false Missing
    }

let private isProjectionTarget (target: RealizationNavigationNode) (node: RealizationNavigationNode) =
    String.Equals(target.ObjectKind, node.ObjectKind, StringComparison.OrdinalIgnoreCase)
    && String.Equals(target.ObjectId, node.ObjectId, StringComparison.OrdinalIgnoreCase)

let private navigationNodeNeedsReview reviewNeededMarks (node: RealizationNavigationNode) =
    if node.ObjectKind = realizationSourceKindHost || node.ObjectKind = realizationSourceKindFunction then
        reviewNeededMarks
        |> realizationPathNeedsReview node.ObjectKind node.ObjectId
    else
        reviewNeededMarks
        |> realizationObjectNeedsReview node.ObjectKind node.ObjectId

let private renderProjectionNodeSummary reviewNeededMarks emphasize (node: RealizationNavigationNode) =
    span {
        attr.``class`` (if emphasize then "has-text-weight-semibold" else "")
        renderObjectKindTag node.ObjectKind
        span {
            attr.``class`` "ml-1"
            text (formatRealizationNavigationNodeLabel node)
        }
        renderReviewNeededBadgeIf (navigationNodeNeedsReview reviewNeededMarks node)
    }

let rec private renderNavigationNode reviewNeededMarks (node: RealizationNavigationNode) =
    li {
        div {
            attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
            renderProjectionNodeSummary reviewNeededMarks false node
            span {
                attr.``class`` "ml-2"
                renderReadinessBadge "" true node.Readiness
            }
        }

        forEach node.DetailLines <| fun detailLine ->
            p {
                attr.``class`` "is-size-7 has-text-grey mt-1"
                text detailLine
            }

        match node.MissingNextKind with
        | None -> empty()
        | Some missingKind -> renderNavigationMissingNext missingKind

        match node.Children with
        | [] -> empty()
        | children ->
            ul {
                forEach children <| fun child ->
                    renderNavigationNode reviewNeededMarks child
            }
    }

let private renderProjectionTarget reviewNeededMarks (node: RealizationNavigationNode) =
    div {
        attr.``class`` "notification is-link is-light py-3 my-3"
        div {
            attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
            strong {
                attr.``class`` "mr-2"
                text "Target"
            }
            renderProjectionNodeSummary reviewNeededMarks true node
            span {
                attr.``class`` "ml-2"
                renderReadinessBadge "" true node.Readiness
            }
        }

        forEach node.DetailLines <| fun detailLine ->
            p {
                attr.``class`` "is-size-7 mt-2"
                text detailLine
            }
    }

let private renderUpstreamPath reviewNeededMarks (target: RealizationNavigationNode) (path: RealizationNavigationNode list) =
    li {
        forEach (path |> List.mapi (fun index node -> index, node)) <| fun (index, node) ->
            concat {
                if index > 0 then
                    span {
                        attr.``class`` "mx-1 has-text-grey"
                        text "->"
                    }

                renderProjectionNodeSummary reviewNeededMarks (isProjectionTarget target node) node
            }
    }

let private renderUpstreamProjection reviewNeededMarks (topology: RealizationTargetTopology) =
    let upstreamPaths =
        topology.UpstreamPaths
        |> List.filter (fun path -> List.length path > 1)

    if List.isEmpty upstreamPaths then
        p {
            attr.``class`` "has-text-grey"
            text "No upstream realization parent."
        }
    else
        ul {
            forEach upstreamPaths <| fun path ->
                renderUpstreamPath reviewNeededMarks topology.Target path
        }

let private renderDownstreamProjection reviewNeededMarks (topology: RealizationTargetTopology) =
    if topology.DownstreamTree.Children |> List.isEmpty then
        div {
            p {
                attr.``class`` "has-text-grey"
                text "No downstream realization nodes are linked yet."
            }

            match topology.DownstreamTree.MissingNextKind with
            | None -> empty()
            | Some missingKind -> renderNavigationMissingNext missingKind
        }
    else
        ul {
            renderNavigationNode reviewNeededMarks topology.DownstreamTree
        }

let private renderNavigationGapRow reviewNeededMarks (gap: RealizationNavigationGap) =
    let needsReview =
        reviewNeededMarks
        |> realizationObjectNeedsReview gap.OwnerKind gap.OwnerId

    tr {
        td {
            renderObjectKindTag gap.OwnerKind
            span {
                attr.``class`` "ml-1"
                text (formatRealizationNavigationObjectLabel gap.OwnerKind gap.OwnerId gap.OwnerName)
            }
            renderReviewNeededBadgeIf needsReview
        }
        td {
            renderReadinessBadge ("Missing " + gap.MissingKind) false Missing
        }
        td { text (String.concat " -> " gap.PathLabels) }
    }

let private renderCompletenessGaps reviewNeededMarks (gaps: RealizationNavigationGap list) =
    match gaps with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text "The selected target has no missing next realization link."
        }
    | missingGaps ->
        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"
                thead {
                    tr {
                        th { text "Object" }
                        th { text "Gap" }
                        th { text "Path" }
                    }
                }
                tbody {
                    forEach missingGaps <| fun gap ->
                        renderNavigationGapRow reviewNeededMarks gap
                }
            }
        }

let private renderNavigationResult (model: Model) (target: RealizationNavigationTarget) =
    let selectedOperator = getSelectedRealizationNavigationOperator model
    let topology = getTargetTopology target.ObjectKind target.ObjectId model
    let reviewNeededMarks = model.reviewNeededMarks

    div {
        attr.``class`` "mt-4"

        h4 {
            attr.``class`` "title is-6"
            text "Navigation Projection"
        }

        match selectedOperator with
        | value when value = realizationNavigationOperatorUpstream ->
            h5 {
                attr.``class`` "subtitle is-6 mb-2"
                text "Upstream"
            }
            renderUpstreamProjection reviewNeededMarks topology
            renderProjectionTarget reviewNeededMarks topology.Target
        | value when value = realizationNavigationOperatorDownstream ->
            renderProjectionTarget reviewNeededMarks topology.Target
            h5 {
                attr.``class`` "subtitle is-6 mb-2"
                text "Downstream"
            }
            renderDownstreamProjection reviewNeededMarks topology
        | value when value = realizationNavigationOperatorTopology ->
            div {
                attr.``class`` "content"
                h5 { text "Upstream" }
                renderUpstreamProjection reviewNeededMarks topology
                renderProjectionTarget reviewNeededMarks topology.Target
                h5 { text "Downstream" }
                renderDownstreamProjection reviewNeededMarks topology
            }
        | value when value = realizationNavigationOperatorCompleteness ->
            let targetWithCompleteness =
                { topology.Target with Readiness = topology.Completeness }

            div {
                attr.``class`` "content"
                renderProjectionTarget reviewNeededMarks targetWithCompleteness
                renderCompletenessGaps reviewNeededMarks topology.MissingGaps
            }
        | _ ->
            p {
                attr.``class`` "has-text-grey"
                text "Select a navigation operator."
            }
    }

let private renderRealizationInquiryNodeTag (node: RealizationInquiryNode) =
    span {
        attr.``class`` "tag is-light"
        text (node.ObjectKind + " " + node.Label)
    }

let private renderRealizationInquiryLines emptyText lines =
    match lines with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text emptyText
        }
    | values ->
        ul {
            forEach values <| fun line ->
                li { text line }
        }

let private renderRealizationInquiryResult (model: Model) (result: RealizationInquiryResult) dispatch =
    let canConvertToIntake = canConvertRealizationInquiryToIntake result
    let duplicateInvestigationExists =
        canConvertToIntake && hasDuplicateRealizationInquiryIntake result model

    div {
        attr.``class`` "mt-4"

        div {
            attr.``class`` "notification is-light py-3"
            p {
                attr.``class`` "heading mb-1"
                text (formatRealizationInquiryQuestion result.Question)
            }
            p {
                attr.``class`` "mb-2"
                strong { text result.Summary }
            }
            div {
                attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
                renderRealizationInquiryNodeTag result.Target
                span {
                    attr.``class`` "ml-2"
                    renderReadinessBadge "" true result.Target.Readiness
                }
            }
        }

        if canConvertToIntake then
            if duplicateInvestigationExists then
                div {
                    attr.``class`` "notification is-warning is-light py-2 mb-3"
                    text "An investigation already exists for this gap."
                }
            else
                div {
                    attr.``class`` "buttons mb-3"
                    button {
                        attr.``class`` "button is-link is-light"
                        attr.``type`` "button"
                        on.click (fun _ -> dispatch (PrefillPhiDraft (buildPhiDraftFromRealizationInquiry result)))
                        text "Send to Investigation Backlog"
                    }
                }

        div {
            attr.``class`` "columns is-variable is-4"

            div {
                attr.``class`` "column"
                h5 {
                    attr.``class`` "subtitle is-6 mb-2"
                    text "Answer"
                }
                renderRealizationInquiryLines "No answer lines available." result.AnswerLines
            }

            div {
                attr.``class`` "column"
                h5 {
                    attr.``class`` "subtitle is-6 mb-2"
                    text "Path"
                }
                renderRealizationInquiryLines "No realization path available." result.PathLines
            }
        }

        if not (List.isEmpty result.RelatedNodes) then
            div {
                attr.``class`` "mb-3"
                h5 {
                    attr.``class`` "subtitle is-6 mb-2"
                    text "Related nodes"
                }
                div {
                    attr.``class`` "tags mb-0"
                    forEach result.RelatedNodes <| fun node ->
                        renderRealizationInquiryNodeTag node
                }
            }

        if not (List.isEmpty result.RecommendedNextSteps) then
            div {
                attr.``class`` "mb-3"
                h5 {
                    attr.``class`` "subtitle is-6 mb-2"
                    text "Recommended next step"
                }
                renderRealizationInquiryLines "No recommended next step." result.RecommendedNextSteps
            }
    }

let private renderRealizationInquirySection (model: Model) (target: RealizationNavigationTarget) dispatch =
    let selectedQuestionKey = getRealizationInquiryQuestionKeyOrDefault model.realizationInquiryQuestion
    let selectedQuestion = getRealizationInquiryQuestionOrDefault model.realizationInquiryQuestion
    let inquiryResult = resolveRealizationInquiryForTarget selectedQuestion target model

    div {
        attr.``class`` "mt-5"

        h4 {
            attr.``class`` "title is-6"
            text "Ask Cognopy: Realization Inquiry"
        }

        div {
            attr.``class`` "columns is-variable is-3"

            div {
                attr.``class`` "column is-6"
                div {
                    attr.``class`` "field"
                    label {
                        attr.``class`` "label"
                        text "Inquiry"
                    }
                    div {
                        attr.``class`` "control"
                        div {
                            attr.``class`` "select is-fullwidth"
                            select {
                                bind.input.string selectedQuestionKey (fun value -> dispatch (SetRealizationInquiryQuestion value))
                                forEach realizationInquiryQuestionOptions <| fun (value, label) ->
                                    renderOption value label
                            }
                        }
                    }
                }
            }

            div {
                attr.``class`` "column is-6"
                div {
                    attr.``class`` "field"
                    label {
                        attr.``class`` "label"
                        text "Selected realization node"
                    }
                    p {
                        attr.``class`` "control"
                        code { text target.Label }
                    }
                }
            }
        }

        renderRealizationInquiryResult model inquiryResult dispatch
    }

let private renderNavigationOperatorsSection (model: Model) dispatch =
    let targetOptions = getRealizationNavigationTargetOptions model
    let selectedTarget = tryFindRealizationNavigationTarget model.realizationNavigationTarget model

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Navigation Operators"
        }

        div {
            attr.``class`` "columns is-variable is-3"

            div {
                attr.``class`` "column"
                div {
                    attr.``class`` "field"
                    label {
                        attr.``class`` "label"
                        text "Operator"
                    }
                    div {
                        attr.``class`` "control"
                        div {
                            attr.``class`` "select is-fullwidth"
                            select {
                                bind.input.string model.realizationNavigationOperator (fun value -> dispatch (SetRealizationNavigationOperator value))
                                forEach realizationNavigationOperators <| fun operatorName ->
                                    renderOption operatorName operatorName
                            }
                        }
                    }
                }
            }

            div {
                attr.``class`` "column"
                div {
                    attr.``class`` "field"
                    label {
                        attr.``class`` "label"
                        text "Target object"
                    }
                    div {
                        attr.``class`` "control"
                        div {
                            attr.``class`` "select is-fullwidth"
                            select {
                                bind.input.string model.realizationNavigationTarget (fun value -> dispatch (SetRealizationNavigationTarget value))
                                option {
                                    attr.value ""
                                    text "Select target object"
                                }
                                forEach targetOptions <| fun target ->
                                    renderOption target.Value target.Label
                            }
                        }
                    }
                }
            }
        }

        match selectedTarget, targetOptions with
        | Some target, _ ->
            renderNavigationResult model target
            renderRealizationInquirySection model target dispatch
        | None, [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No realization navigation targets are available yet."
            }
        | None, _ ->
            p {
                attr.``class`` "has-text-grey"
                text "Select a target object."
            }
    }

let private renderObjectsTable (model: Model) =
    let state = model.realizationState
    let reviewNeededMarks = model.reviewNeededMarks
    let objectRows = getObjectRows state
    let reviewImpacts = getT6ReviewNeededImpactsFromLedger model.LedgerEvents

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Realization Objects"
        }

        match objectRows with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No realization objects created yet."
            }
        | rows ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Kind" }
                            th { text "Status" }
                            th { text "Id" }
                            th { text "Name" }
                            th { text "Description" }
                            th { text "Source / note" }
                        }
                    }

                    tbody {
                        forEach rows <| fun row ->
                            renderObjectRow state reviewNeededMarks reviewImpacts row
                    }
                }
            }
    }

let private renderRealizationTraceSection (model: Model) =
    let state = model.realizationState
    let reviewNeededMarks = model.reviewNeededMarks
    let reviewImpacts = getT6ReviewNeededImpactsFromLedger model.LedgerEvents
    let traces =
        getRealizationSourceHosts model
        |> List.map (fun entry -> getHostRealizationTrace entry.Value state)

    let renderMissingNextLink missingKind =
        div {
            attr.``class`` "mt-1"
            renderReadinessBadge ("Missing " + missingKind) false Missing
        }

    let rec renderTraceNode (node: RealizationTraceNode) =
        let needsReview =
            reviewNeededMarks
            |> realizationObjectNeedsReview node.ObjectKind node.ObjectId

        li {
            div {
                attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
                renderObjectKindTag node.ObjectKind
                span {
                    attr.``class`` "ml-1"
                    text (formatTraceObjectLabel node)
                }
                renderReviewNeededBadgeIf needsReview
                span {
                    attr.``class`` "ml-2"
                    renderReadinessBadge "" true node.Readiness
                }

                if t6ReviewNeededImpactsAffectObject node.ObjectKind node.ObjectId reviewImpacts then
                    span {
                        attr.``class`` "ml-2"
                        renderT6ReviewStatusTag true
                    }
            }

            match node.MissingNextKind with
            | None -> empty()
            | Some missingKind -> renderMissingNextLink missingKind

            match node.Children with
            | [] -> empty()
            | children ->
                ul {
                    forEach children <| fun child ->
                        renderTraceNode child
                }
        }

    let renderHostTrace (trace: HostRealizationTrace) =
        let needsReview =
            reviewNeededMarks
            |> realizationPathNeedsReview realizationSourceKindHost trace.HostValue

        li {
            div {
                attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
                strong { text "Host: " }
                span {
                    attr.``class`` "ml-1"
                    text trace.HostValue
                }
                renderReviewNeededBadgeIf needsReview
                span {
                    attr.``class`` "ml-2"
                    renderReadinessBadge "" true trace.Readiness
                }

                if t6ReviewNeededImpactsAffectUpstreamAtom realizationSourceKindHost trace.HostValue reviewImpacts then
                    span {
                        attr.``class`` "ml-2"
                        renderT6ReviewStatusTag true
                    }
            }

            match trace.Parts with
            | [] ->
                p {
                    attr.``class`` "is-size-7 has-text-grey mt-1"
                    text "No Parts linked."
                }
            | parts ->
                ul {
                    forEach parts <| fun part ->
                        renderTraceNode part
                }
        }

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Realization Trace"
        }

        match traces with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No known Host atoms are available yet."
            }
        | hostTraces ->
            div {
                attr.``class`` "content"
                ul {
                    forEach hostTraces <| fun trace ->
                        renderHostTrace trace
                }
            }
    }

let private renderLinksTable (model: Model) =
    let state = model.realizationState
    let reviewNeededMarks = model.reviewNeededMarks
    let linkRows = getRealizationLinkRows state
    let reviewImpacts = getT6ReviewNeededImpactsFromLedger model.LedgerEvents

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "Realization Links"
        }

        match linkRows with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No realization links created yet."
            }
        | rows ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Link kind" }
                            th { text "Status" }
                            th { text "Source" }
                            th { text "Target" }
                        }
                    }

                    tbody {
                        forEach rows <| fun (linkKind, sourceId, targetId) ->
                            let needsReview =
                                reviewNeededMarks
                                |> realizationLinkNeedsReview linkKind sourceId targetId
                            let reviewNeeded =
                                needsReview || t6ReviewNeededImpactsAffectLink linkKind sourceId targetId reviewImpacts

                            tr {
                                td {
                                    text linkKind
                                    renderReviewNeededBadgeIf needsReview
                                }
                                td { renderT6ReviewStatusTag reviewNeeded }
                                td { text sourceId }
                                td { text targetId }
                            }
                    }
                }
            }
    }

let renderT6RealizationTab (model: Model) dispatch =
    let state = model.realizationState
    let impactProjection = buildImpactProjection model
    let hostCount = model |> getRealizationSourceHosts |> List.length
    let functionCount = model |> getRealizationSourceFunctions |> List.length
    let objectCount = getObjectRows state |> List.length
    let linkCount = getRealizationLinkRows state |> List.length
    let realizationReviewNeededCount =
        model.reviewNeededMarks
        |> Gaia.Client.Workflow.getRealizationReviewNeededMarks
        |> List.length

    div {
        attr.``class`` "mb-6 pb-5"

        h2 {
            attr.``class`` "title is-4"
            text "T6 Design Realization"
        }

        p {
            attr.``class`` "has-text-grey mb-4"
            text "Manual continuity bookkeeping from accepted or known Sigma context into Host -> Part -> DP -> TF -> CTQ -> VV realization chains."
        }

        div {
            attr.``class`` "tags mb-4"
            span {
                attr.``class`` "tag is-light"
                text ("Source Hosts: " + string hostCount)
            }
            span {
                attr.``class`` "tag is-light"
                text ("Source Functions: " + string functionCount)
            }
            span {
                attr.``class`` "tag is-light"
                text ("Objects: " + string objectCount)
            }
            span {
                attr.``class`` "tag is-light"
                text ("Links: " + string linkCount)
            }
            if realizationReviewNeededCount > 0 then
                span {
                    attr.``class`` "tag is-warning is-light"
                    text (reviewNeededLabel + ": " + string realizationReviewNeededCount)
                }
        }

        renderStatusNotification model.realizationStatus

        div {
            attr.``class`` "columns is-variable is-5"

            div {
                attr.``class`` "column is-4"
                renderCreateObjectForm model dispatch
                renderCreateLinkForm model dispatch
                renderEvidenceCapturePanel model dispatch
                renderSelectedEvidenceReferences model
            }

            div {
                attr.``class`` "column is-8"
                renderHostCompletenessTable model
                renderT6Summary model
                renderT6ImpactSummary model
                renderImpactProjectionSection
                    "No realization impact rows are currently projected."
                    impactProjection
                    impactProjection.RealizationRows
                renderNavigationOperatorsSection model dispatch
                renderRealizationTraceSection model
                renderObjectsTable model
                renderLinksTable model
            }
        }
    }
