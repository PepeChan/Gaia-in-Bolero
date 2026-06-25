module Gaia.Client.T6RealizationView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Realization

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

                            tr {
                                td { text entry.Value }
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

    div {
        attr.``class`` "box"

        h3 {
            attr.``class`` "title is-5"
            text "T6 Summary"
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

let private renderObjectRow (state: RealizationState) (objectKind: string, objectId: string, objectName: string) =
    let note = tryFindObjectNote objectKind objectId state
    let readiness = getRealizationObjectReadiness objectKind objectId state

    tr {
        attr.``class`` (semanticObjectRowClass objectKind)
        td {
            renderObjectReadiness objectKind readiness
        }
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

type private NavigationTarget =
    {
        Value: string
        ObjectKind: string
        ObjectId: string
        Label: string
    }

type private NavigationNode =
    {
        ObjectKind: string
        ObjectId: string
        ObjectName: string
        Readiness: ReadinessState
        MissingNextKind: string option
        DetailLines: string list
        Children: NavigationNode list
    }

type private NavigationGap =
    {
        OwnerKind: string
        OwnerId: string
        OwnerName: string
        MissingKind: string
        PathLabels: string list
    }

let private cleanNavigationText (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private equalsNavigationText left right =
    String.Equals(cleanNavigationText left, cleanNavigationText right, StringComparison.OrdinalIgnoreCase)

let private encodeNavigationTarget objectKind objectId =
    objectKind + "|" + objectId

let private makeNavigationTarget objectKind objectId label =
    {
        Value = encodeNavigationTarget objectKind objectId
        ObjectKind = objectKind
        ObjectId = objectId
        Label = label
    }

let private distinctNavigationTargets targets =
    targets
    |> List.fold
        (fun selected target ->
            if selected |> List.exists (fun existing -> equalsNavigationText existing.Value target.Value) then
                selected
            else
                selected @ [ target ])
        []

let private getNavigationTargetOptions (model: Model) =
    let state = model.realizationState

    [
        yield!
            getRealizationSourceHosts model
            |> List.map (fun entry ->
                makeNavigationTarget
                    realizationSourceKindHost
                    entry.Value
                    (entry.Value + " (Host; support " + string entry.SupportCount + ")"))
        yield!
            getRealizationSourceFunctions model
            |> List.map (fun entry ->
                makeNavigationTarget
                    realizationSourceKindFunction
                    entry.Value
                    (entry.Value + " (Function; support " + string entry.SupportCount + ")"))
        yield!
            getObjectRows state
            |> List.map (fun (objectKind, objectId, objectName) ->
                makeNavigationTarget objectKind objectId (objectKind + ": " + formatIdName objectId objectName))
    ]
    |> distinctNavigationTargets

let private tryFindSelectedNavigationTarget (model: Model) =
    getNavigationTargetOptions model
    |> List.tryFind (fun target -> equalsNavigationText target.Value model.realizationNavigationTarget)

let private getSelectedNavigationOperator (model: Model) =
    if realizationNavigationOperators |> List.contains model.realizationNavigationOperator then
        model.realizationNavigationOperator
    else
        defaultRealizationNavigationOperator

let private formatNavigationKindForMissing missingKind =
    if missingKind = realizationObjectKindVV then
        "VV"
    else
        missingKind

let private getNavigationObjectName objectKind objectId (state: RealizationState) =
    if objectKind = realizationSourceKindHost || objectKind = realizationSourceKindFunction then
        ""
    else
        getRealizationObjectName objectKind objectId state

let private formatNavigationObjectLabel objectKind objectId objectName =
    if objectKind = realizationSourceKindHost || objectKind = realizationSourceKindFunction then
        objectId
    elif objectName = "" then
        objectId
    else
        objectId + " - " + objectName

let private getFunctionDirectReadiness functionValue (state: RealizationState) =
    if getFrIdsForFunction functionValue state |> List.isEmpty then
        Missing
    else
        Complete

let private getNavigationDirectReadiness objectKind objectId (state: RealizationState) =
    match objectKind with
    | kind when kind = realizationSourceKindHost -> (getHostReadiness objectId state).Overall
    | kind when kind = realizationSourceKindFunction -> getFunctionDirectReadiness objectId state
    | _ -> getRealizationObjectReadiness objectKind objectId state

let private makeNavigationNode objectKind objectId missingNextKind detailLines children (state: RealizationState) =
    {
        ObjectKind = objectKind
        ObjectId = objectId
        ObjectName = getNavigationObjectName objectKind objectId state
        Readiness = getNavigationDirectReadiness objectKind objectId state
        MissingNextKind =
            if children |> List.isEmpty then
                missingNextKind |> Option.map formatNavigationKindForMissing
            else
                None
        DetailLines = detailLines
        Children = children
    }

let private getDownstreamChildTargets objectKind objectId (state: RealizationState) =
    match objectKind with
    | kind when kind = realizationSourceKindHost ->
        (
            getPartIdsForHost objectId state
            |> List.map (fun childId -> realizationObjectKindPart, childId),
            Some realizationObjectKindPart
        )
    | kind when kind = realizationSourceKindFunction ->
        (
            getFrIdsForFunction objectId state
            |> List.map (fun childId -> realizationObjectKindFR, childId),
            Some realizationObjectKindFR
        )
    | kind when kind = realizationObjectKindFR ->
        (
            getDpIdsForFR objectId state
            |> List.map (fun childId -> realizationObjectKindDP, childId),
            Some realizationObjectKindDP
        )
    | kind when kind = realizationObjectKindPart ->
        (
            getDpIdsForPart objectId state
            |> List.map (fun childId -> realizationObjectKindDP, childId),
            Some realizationObjectKindDP
        )
    | kind when kind = realizationObjectKindDP ->
        (
            getTfIdsForDp objectId state
            |> List.map (fun childId -> realizationObjectKindTF, childId),
            Some realizationObjectKindTF
        )
    | kind when kind = realizationObjectKindTF ->
        (
            getCtqIdsForTf objectId state
            |> List.map (fun childId -> realizationObjectKindCTQ, childId),
            Some realizationObjectKindCTQ
        )
    | kind when kind = realizationObjectKindCTQ ->
        (
            getVvIdsForCtq objectId state
            |> List.map (fun childId -> realizationObjectKindVV, childId),
            Some "VV"
        )
    | _ -> [], None

let private getUpstreamParentTargets objectKind objectId (state: RealizationState) =
    match objectKind with
    | kind when kind = realizationObjectKindFR ->
        getFunctionValuesForFR objectId state
        |> List.map (fun parentId -> realizationSourceKindFunction, parentId)
    | kind when kind = realizationObjectKindPart ->
        getHostValuesForPart objectId state
        |> List.map (fun parentId -> realizationSourceKindHost, parentId)
    | kind when kind = realizationObjectKindDP ->
        [
            yield!
                getPartIdsForDp objectId state
                |> List.map (fun parentId -> realizationObjectKindPart, parentId)
            yield!
                getFrIdsForDp objectId state
                |> List.map (fun parentId -> realizationObjectKindFR, parentId)
        ]
    | kind when kind = realizationObjectKindTF ->
        getDpIdsForTf objectId state
        |> List.map (fun parentId -> realizationObjectKindDP, parentId)
    | kind when kind = realizationObjectKindCTQ ->
        getTfIdsForCtq objectId state
        |> List.map (fun parentId -> realizationObjectKindTF, parentId)
    | kind when kind = realizationObjectKindVV ->
        getCtqIdsForVv objectId state
        |> List.map (fun parentId -> realizationObjectKindCTQ, parentId)
    | _ -> []

let private formatSourceSummaryLines sourceKind sourceValue (model: Model) =
    let entries =
        if sourceKind = realizationSourceKindHost then
            getRealizationSourceHosts model
        elif sourceKind = realizationSourceKindFunction then
            getRealizationSourceFunctions model
        else
            []

    match entries |> List.tryFind (fun entry -> equalsNavigationText entry.Value sourceValue) with
    | Some entry ->
        [
            sourceKind + " source is present in accepted or known Sigma context."
            "Support count: " + string entry.SupportCount + "."
            if not (String.IsNullOrWhiteSpace(entry.Provenance)) then
                "Provenance: " + entry.Provenance + "."
            if not (String.IsNullOrWhiteSpace(entry.SourcePhiId)) then
                "Source Phi: " + entry.SourcePhiId + "."
            if not (List.isEmpty entry.SupportingPhiIds) then
                "Supporting Phi IDs: " + String.concat ", " entry.SupportingPhiIds + "."
        ]
    | None -> [ "No upstream realization parent." ]

let rec private buildDownstreamNavigationNode (model: Model) objectKind objectId =
    let state = model.realizationState
    let childTargets, missingNextKind = getDownstreamChildTargets objectKind objectId state

    let children =
        childTargets
        |> List.map (fun (childKind, childId) -> buildDownstreamNavigationNode model childKind childId)

    makeNavigationNode objectKind objectId missingNextKind [] children state

let rec private buildUpstreamNavigationNode (model: Model) objectKind objectId =
    let state = model.realizationState
    let parentTargets = getUpstreamParentTargets objectKind objectId state

    let parents =
        parentTargets
        |> List.map (fun (parentKind, parentId) -> buildUpstreamNavigationNode model parentKind parentId)

    let detailLines =
        if objectKind = realizationSourceKindHost || objectKind = realizationSourceKindFunction then
            formatSourceSummaryLines objectKind objectId model
        else
            []

    makeNavigationNode objectKind objectId None detailLines parents state

let rec private getNavigationCompleteness objectKind objectId (state: RealizationState) =
    if objectKind = realizationObjectKindVV then
        getRealizationObjectReadiness objectKind objectId state
    else
        let childTargets, _ = getDownstreamChildTargets objectKind objectId state

        match childTargets with
        | [] -> Missing
        | children ->
            let childReadiness =
                children
                |> List.map (fun (childKind, childId) -> getNavigationCompleteness childKind childId state)

            if childReadiness |> List.forall (fun readiness -> readiness = Complete) then
                Complete
            else
                Partial

let rec private collectNavigationGaps objectKind objectId pathLabels (state: RealizationState) =
    let objectName = getNavigationObjectName objectKind objectId state
    let pathLabel = formatNavigationObjectLabel objectKind objectId objectName
    let pathLabels = pathLabels @ [ pathLabel ]
    let childTargets, missingNextKind = getDownstreamChildTargets objectKind objectId state

    match missingNextKind, childTargets with
    | Some missingKind, [] ->
        [
            {
                OwnerKind = objectKind
                OwnerId = objectId
                OwnerName = objectName
                MissingKind = formatNavigationKindForMissing missingKind
                PathLabels = pathLabels
            }
        ]
    | _ ->
        childTargets
        |> List.collect (fun (childKind, childId) -> collectNavigationGaps childKind childId pathLabels state)

let private renderNavigationMissingNext missingKind =
    div {
        attr.``class`` "mt-1"
        renderReadinessBadge ("Missing " + missingKind) false Missing
    }

let rec private renderNavigationNode (node: NavigationNode) =
    li {
        div {
            attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
            renderObjectKindTag node.ObjectKind
            span {
                attr.``class`` "ml-1"
                text (formatNavigationObjectLabel node.ObjectKind node.ObjectId node.ObjectName)
            }
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
                    renderNavigationNode child
            }
    }

let private renderNavigationTree node =
    div {
        attr.``class`` "content"
        ul {
            renderNavigationNode node
        }
    }

let private renderNoUpstreamParent node =
    if List.isEmpty node.Children && List.isEmpty node.DetailLines then
        p {
            attr.``class`` "has-text-grey"
            text "No upstream realization parent."
        }
    else
        empty()

let private renderNavigationGapRow gap =
    tr {
        td {
            renderObjectKindTag gap.OwnerKind
            span {
                attr.``class`` "ml-1"
                text (formatNavigationObjectLabel gap.OwnerKind gap.OwnerId gap.OwnerName)
            }
        }
        td {
            renderReadinessBadge ("Missing " + gap.MissingKind) false Missing
        }
        td { text (String.concat " -> " gap.PathLabels) }
    }

let private renderCompletenessGaps gaps =
    match gaps with
    | [] ->
        p {
            attr.``class`` "has-text-grey"
            text "No missing next links."
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
                        renderNavigationGapRow gap
                }
            }
        }

let private renderNavigationResult (model: Model) (target: NavigationTarget) =
    let selectedOperator = getSelectedNavigationOperator model
    let state = model.realizationState

    div {
        attr.``class`` "mt-4"

        match selectedOperator with
        | value when value = realizationNavigationOperatorUpstream ->
            let upstreamNode = buildUpstreamNavigationNode model target.ObjectKind target.ObjectId
            renderNavigationTree upstreamNode
            renderNoUpstreamParent upstreamNode
        | value when value = realizationNavigationOperatorDownstream ->
            buildDownstreamNavigationNode model target.ObjectKind target.ObjectId
            |> renderNavigationTree
        | value when value = realizationNavigationOperatorTopology ->
            let upstreamNode = buildUpstreamNavigationNode model target.ObjectKind target.ObjectId
            let downstreamNode = buildDownstreamNavigationNode model target.ObjectKind target.ObjectId

            div {
                attr.``class`` "content"
                h4 {
                    attr.``class`` "title is-6"
                    text "Upstream Context"
                }
                ul { renderNavigationNode upstreamNode }
                renderNoUpstreamParent upstreamNode
                h4 {
                    attr.``class`` "title is-6 mt-4"
                    text "Downstream Realization"
                }
                ul { renderNavigationNode downstreamNode }
            }
        | value when value = realizationNavigationOperatorCompleteness ->
            let readiness = getNavigationCompleteness target.ObjectKind target.ObjectId state
            let gaps = collectNavigationGaps target.ObjectKind target.ObjectId [] state
            let objectName = getNavigationObjectName target.ObjectKind target.ObjectId state

            div {
                attr.``class`` "content"
                div {
                    attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap mb-3"
                    renderObjectKindTag target.ObjectKind
                    span {
                        attr.``class`` "ml-1"
                        text (formatNavigationObjectLabel target.ObjectKind target.ObjectId objectName)
                    }
                    span {
                        attr.``class`` "ml-2"
                        renderReadinessBadge "" true readiness
                    }
                }
                renderCompletenessGaps gaps
            }
        | _ ->
            p {
                attr.``class`` "has-text-grey"
                text "Select a navigation operator."
            }
    }

let private renderNavigationOperatorsSection (model: Model) dispatch =
    let targetOptions = getNavigationTargetOptions model
    let selectedTarget = tryFindSelectedNavigationTarget model

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
        | Some target, _ -> renderNavigationResult model target
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

let private renderObjectsTable (state: RealizationState) =
    let objectRows = getObjectRows state

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
                            th { text "Id" }
                            th { text "Name" }
                            th { text "Description" }
                            th { text "Source / note" }
                        }
                    }

                    tbody {
                        forEach rows <| fun row ->
                            renderObjectRow state row
                    }
                }
            }
    }

let private renderRealizationTraceSection (model: Model) =
    let state = model.realizationState
    let traces =
        getRealizationSourceHosts model
        |> List.map (fun entry -> getHostRealizationTrace entry.Value state)

    let renderMissingNextLink missingKind =
        div {
            attr.``class`` "mt-1"
            renderReadinessBadge ("Missing " + missingKind) false Missing
        }

    let rec renderTraceNode (node: RealizationTraceNode) =
        li {
            div {
                attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
                renderObjectKindTag node.ObjectKind
                span {
                    attr.``class`` "ml-1"
                    text (formatTraceObjectLabel node)
                }
                span {
                    attr.``class`` "ml-2"
                    renderReadinessBadge "" true node.Readiness
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
        li {
            div {
                attr.``class`` "is-flex is-align-items-center is-flex-wrap-wrap"
                strong { text "Host: " }
                span {
                    attr.``class`` "ml-1"
                    text trace.HostValue
                }
                span {
                    attr.``class`` "ml-2"
                    renderReadinessBadge "" true trace.Readiness
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

let private renderLinksTable (state: RealizationState) =
    let linkRows = getRealizationLinkRows state

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
                            th { text "Source" }
                            th { text "Target" }
                        }
                    }

                    tbody {
                        forEach rows <| fun (linkKind, sourceId, targetId) ->
                            tr {
                                td { text linkKind }
                                td { text sourceId }
                                td { text targetId }
                            }
                    }
                }
            }
    }

let renderT6RealizationTab (model: Model) dispatch =
    let state = model.realizationState
    let hostCount = model |> getRealizationSourceHosts |> List.length
    let functionCount = model |> getRealizationSourceFunctions |> List.length
    let objectCount = getObjectRows state |> List.length
    let linkCount = getRealizationLinkRows state |> List.length

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
        }

        renderStatusNotification model.realizationStatus

        div {
            attr.``class`` "columns is-variable is-5"

            div {
                attr.``class`` "column is-4"
                renderCreateObjectForm model dispatch
                renderCreateLinkForm model dispatch
            }

            div {
                attr.``class`` "column is-8"
                renderHostCompletenessTable model
                renderT6Summary model
                renderNavigationOperatorsSection model dispatch
                renderRealizationTraceSection model
                renderObjectsTable state
                renderLinksTable state
            }
        }
    }
