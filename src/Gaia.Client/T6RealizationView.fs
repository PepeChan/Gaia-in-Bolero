module Gaia.Client.T6RealizationView

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
                            th { text "Linked Parts" }
                            th { text "Status" }
                        }
                    }

                    tbody {
                        forEach entries <| fun entry ->
                            let partIds = getPartIdsForHost entry.Value state
                            let status = getHostRealizationStatus entry.Value state

                            tr {
                                td { text entry.Value }
                                td { text (string entry.SupportCount) }
                                td { text (formatNone partIds) }
                                td { renderRealizationStatusTag status }
                            }
                    }
                }
            }
    }

let private renderGapRow (label: string) (values: string list) =
    tr {
        td { text label }
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
                        th { text "Count" }
                        th { text "Items" }
                    }
                }

                tbody {
                    renderGapRow "Hosts without Parts" hostsWithoutParts
                    renderGapRow "Parts without DPs" partsWithoutDPs
                    renderGapRow "DPs without TFs" dpsWithoutTFs
                    renderGapRow "TFs without CTQs" tfsWithoutCTQs
                    renderGapRow "CTQs without VV" ctqsWithoutVV
                }
            }
        }
    }

let private renderObjectRow (state: RealizationState) (objectKind: string, objectId: string, objectName: string) =
    let note = tryFindObjectNote objectKind objectId state

    tr {
        td { text objectKind }
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
            text "Manual continuity bookkeeping from accepted or known Sigma context into realization objects and links."
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
                renderObjectsTable state
                renderLinksTable state
            }
        }
    }
