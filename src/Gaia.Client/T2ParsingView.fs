module Gaia.Client.T2ParsingView

open System
open Bolero
open Bolero.Html
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let tryGetSelectedScenario model =
    model.selectedScenarioId
    |> Option.bind tryFindScenario
    |> Option.orElse initialScenario

let getAdmissibilityResult (parse: PhiParse) =
    if parse.OutcomeEscalate then
        Escalate
    elif parse.ResultRejected then
        Reject
    elif parse.OutcomeHold || parse.ResultIndeterminate then
        Hold
    elif parse.ResultValid then
        Admit
    else
        Hold

let formatAdmissibilityResult = function
    | Admit -> "ADMIT"
    | Hold -> "HOLD"
    | Reject -> "REJECT"
    | Escalate -> "ESCALATE"

let admissibilityBadgeClass = function
    | Admit -> "tag is-success is-medium"
    | Hold -> "tag is-warning is-medium"
    | Reject -> "tag is-danger is-medium"
    | Escalate -> "tag is-black is-medium"

let formatDerivationEntry = function
    | Some FromFR -> "From FR"
    | Some FromMode -> "From Mode"
    | Some FromInterface -> "From Interface"
    | Some FromState -> "From State"
    | Some FromParametric -> "From Parametric"
    | Some GammaOnly -> "Gamma Only"
    | None -> "Not resolved"

let mapIdsToNames getId getName items ids =
    ids
    |> List.map (fun id ->
        items
        |> List.tryFind (fun item -> getId item = id)
        |> Option.map getName
        |> Option.defaultValue id)

let renderSigmaSnapshotMetric label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderSigmaSnapshotCounts parsedPhiCount staleParsedPhiCount sigmaContext =
    div {
        attr.``class`` "tags are-medium mb-4"
        renderSigmaSnapshotMetric "Included parsed Φ" parsedPhiCount
        renderSigmaSnapshotMetric "Stale parsed Φ" staleParsedPhiCount
        renderSigmaSnapshotMetric "Functions" (List.length sigmaContext.Functions)
        renderSigmaSnapshotMetric "Modes" (List.length sigmaContext.Modes)
        renderSigmaSnapshotMetric "Interfaces" (List.length sigmaContext.Interfaces)
        renderSigmaSnapshotMetric "States" (List.length sigmaContext.States)
        renderSigmaSnapshotMetric "Hosts" (List.length sigmaContext.Hosts)
    }

let renderParsedPhiLedgerPanel parsedPhis staleParsedPhiIds excludedPhiIds dispatch =
    let sequencedParsedPhis = getSequencedParsedPhis parsedPhis

    let excludedPhiCount =
        sequencedParsedPhis
        |> List.filter (fun (_, parse) -> isPhiExcluded excludedPhiIds parse.PhiId)
        |> List.length

    let totalParsedPhiCount = List.length sequencedParsedPhis
    let includedPhiCount = totalParsedPhiCount - excludedPhiCount
    let staleParsedPhiCount = getStaleParsedPhiCount staleParsedPhiIds parsedPhis

    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Replay Engine Lite"
        }

        h2 {
            attr.``class`` "title is-5"
            text "Parsed Φ Ledger / Replay Control"
        }

        div {
            attr.``class`` "tags are-medium mb-4"
            renderSigmaSnapshotMetric "Total parsed Φ" totalParsedPhiCount
            renderSigmaSnapshotMetric "Included Φ" includedPhiCount
            renderSigmaSnapshotMetric "Excluded Φ" excludedPhiCount
            renderSigmaSnapshotMetric "Stale Φ" staleParsedPhiCount
        }

        match sequencedParsedPhis with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "Parse a Φ to make it available for replay control."
            }

        | phis ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-hoverable"

                    thead {
                        tr {
                            th { text "#" }
                            th { text "PhiId" }
                            th { text "Statement" }
                            th { text "Status" }
                            th { text "Replay" }
                        }
                    }

                    tbody {
                        forEach phis <| fun (parseSequenceNumber, parse) ->
                            let isExcluded = isPhiExcluded excludedPhiIds parse.PhiId
                            let isStale = isPhiParseStale staleParsedPhiIds parse.PhiId

                            tr {
                                td { text (string parseSequenceNumber) }
                                td {
                                    code { text parse.PhiId }
                                }
                                td { text parse.Statement }
                                td {
                                    div {
                                        attr.``class`` "tags mb-0"
                                        span {
                                            attr.``class`` (
                                                if isExcluded then
                                                    "tag is-warning"
                                                else
                                                    "tag is-success is-light")
                                            text (
                                                if isExcluded then
                                                    "Excluded"
                                                else
                                                    "Included")
                                        }
                                        if isStale then
                                            span {
                                                attr.``class`` "tag is-warning is-light"
                                                text "Stale parse"
                                            }
                                    }
                                }
                                td {
                                    button {
                                        attr.``class`` (
                                            if isExcluded then
                                                "button is-small is-success is-light"
                                            else
                                                "button is-small is-warning is-light")
                                        attr.``type`` "button"
                                        on.click (fun _ -> dispatch (ToggleExcludeParsedPhi parse.PhiId))
                                        text (
                                            if isExcluded then
                                                "Include"
                                            else
                                                "Exclude")
                                    }
                                }
                            }
                    }
                }
            }
    }

