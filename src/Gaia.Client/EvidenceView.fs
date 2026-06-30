module Gaia.Client.EvidenceView

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let private formatEvidenceTargetKindLabel targetKind =
    if parsedExposureAtomKinds |> List.contains targetKind then
        formatModelFittingAtomKindLabel targetKind
    else
        targetKind

let private renderEvidenceRecords title emptyText (evidenceRecords: EvidenceRecord list) =
    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text title
        }

        match evidenceRecords with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text emptyText
            }
        | records ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "EvidenceId" }
                            th { text "Time UTC" }
                            th { text "Kind" }
                            th { text "Target" }
                            th { text "Title" }
                            th { text "Notes" }
                            th { text "ContentRef" }
                        }
                    }

                    tbody {
                        forEach records <| fun evidenceRecord ->
                            tr {
                                td { text evidenceRecord.EvidenceId }
                                td { text evidenceRecord.TimestampUtc }
                                td { text evidenceRecord.CaptureKind }
                                td { text (formatEvidenceTargetKindLabel evidenceRecord.TargetKind + ": " + evidenceRecord.TargetLabel) }
                                td { text evidenceRecord.Title }
                                td { text evidenceRecord.Notes }
                                td { text evidenceRecord.ContentRef }
                            }
                    }
                }
            }
    }

let renderEvidenceLibrary (evidenceRecords: EvidenceRecord list) =
    renderEvidenceRecords "Evidence / Context References" "No evidence captured yet." evidenceRecords

let private getSelectedEvidenceTargetLabel (model: Model) =
    model
    |> getCurrentEvidenceTargetOptions
    |> List.tryFind (fun (targetId, _) -> targetId = model.evidenceTargetId)
    |> Option.map snd

let getSelectedEvidenceRecords (model: Model) =
    if String.IsNullOrWhiteSpace(model.evidenceTargetKind) || String.IsNullOrWhiteSpace(model.evidenceTargetId) then
        []
    else
        model.evidenceRecords
        |> List.filter (fun record ->
            record.TargetKind = model.evidenceTargetKind
            && record.TargetId = model.evidenceTargetId)

let renderSelectedEvidenceReferences (model: Model) =
    let title =
        match getSelectedEvidenceTargetLabel model with
        | Some label -> "Selected Target Evidence: " + label
        | None -> "Selected Target Evidence"

    let emptyText =
        if String.IsNullOrWhiteSpace(model.evidenceTargetId) then
            "Select a target to see attached evidence references."
        else
            "No evidence references for this target yet."

    renderEvidenceRecords title emptyText (getSelectedEvidenceRecords model)

let renderEvidenceCapturePanel (model: Model) dispatch =
    let targetOptions = getCurrentEvidenceTargetOptions model

    div {
        attr.``class`` "box"

        h2 {
            attr.``class`` "title is-5"
            text "1sec Snip"
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Capture kind"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.evidenceCaptureKind (fun value -> dispatch (SetEvidenceCaptureKind value))
                        forEach evidenceCaptureKinds <| fun captureKind ->
                            option { text captureKind }
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Target kind"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.evidenceTargetKind (fun value -> dispatch (SetEvidenceTargetKind value))
                        forEach evidenceTargetKinds <| fun targetKind ->
                            option {
                                attr.value targetKind
                                text (formatEvidenceTargetKindLabel targetKind)
                            }
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Target item"
            }
            div {
                attr.``class`` "control"
                div {
                    attr.``class`` "select is-fullwidth"
                    select {
                        bind.input.string model.evidenceTargetId (fun value -> dispatch (SetEvidenceTargetId value))

                        option {
                            attr.value ""
                            text (
                                if List.isEmpty targetOptions then
                                    "No targets available"
                                else
                                    "Select target item")
                        }

                        forEach targetOptions <| fun (targetId, targetLabel) ->
                            option {
                                attr.value targetId
                                text targetLabel
                            }
                    }
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Title"
            }
            div {
                attr.``class`` "control"
                input {
                    attr.``class`` "input"
                    bind.input.string model.evidenceTitle (fun value -> dispatch (SetEvidenceTitle value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Notes"
            }
            div {
                attr.``class`` "control"
                textarea {
                    attr.``class`` "textarea"
                    bind.input.string model.evidenceNotes (fun value -> dispatch (SetEvidenceNotes value))
                }
            }
        }

        div {
            attr.``class`` "field"
            label {
                attr.``class`` "label"
                text "Content reference"
            }
            div {
                attr.``class`` "control"
                input {
                    attr.``class`` "input"
                    bind.input.string model.evidenceContentRef (fun value -> dispatch (SetEvidenceContentRef value))
                }
            }
        }

        button {
            attr.``class`` "button is-link is-fullwidth"
            attr.``type`` "button"
            on.click (fun _ -> dispatch CreateEvidenceRecord)
            text "Create 1sec Snip"
        }

        match model.evidenceStatus with
        | None ->
            empty()
        | Some status ->
            div {
                attr.``class`` "notification is-info is-light mt-4"
                text status
            }
    }

let renderEvidenceTab (model: Model) dispatch =
    div {
        attr.``class`` "mb-6 pb-5"

        h2 {
            attr.``class`` "title is-4"
            text "Evidence"
        }

        div {
            attr.``class`` "columns is-variable is-5"

            div {
                attr.``class`` "column is-4"
                renderEvidenceCapturePanel model dispatch
            }

            div {
                attr.``class`` "column is-8"
                renderEvidenceLibrary model.evidenceRecords
            }
        }
    }
