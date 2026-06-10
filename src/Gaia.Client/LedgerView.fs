module Gaia.Client.LedgerView

open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Ledger

let renderLedgerCounter label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderReplayComparisonRow measure selectedValue currentValue =
    tr {
        td { text measure }
        td { text selectedValue }
        td { text currentValue }
    }

let formatSignedDelta value =
    if value > 0 then
        "+" + string value
    else
        string value

let renderReplayDeltaRow measure selectedValue currentValue =
    tr {
        td { text measure }
        td { text (formatSignedDelta (selectedValue - currentValue)) }
    }

let renderReplayPreviewTables selectedState currentState =
    div {
        div {
            attr.``class`` "table-container mb-4"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Measure" }
                        th { text "At selected event" }
                        th { text "Current" }
                    }
                }

                tbody {
                    renderReplayComparisonRow "Parsed Phi events" (string selectedState.ParsedPhiEvents) (string currentState.ParsedPhiEvents)
                    renderReplayComparisonRow "Included Phi count" (string selectedState.IncludedPhiCount) (string currentState.IncludedPhiCount)
                    renderReplayComparisonRow "Excluded Phi count" (string selectedState.ExcludedPhiCount) (string currentState.ExcludedPhiCount)
                    renderReplayComparisonRow "Governance accepted" (string selectedState.GovernanceAccepted) (string currentState.GovernanceAccepted)
                    renderReplayComparisonRow "Governance rejected" (string selectedState.GovernanceRejected) (string currentState.GovernanceRejected)
                    renderReplayComparisonRow "Governance held" (string selectedState.GovernanceHeld) (string currentState.GovernanceHeld)
                    renderReplayComparisonRow "Governance pending" "-" "-"
                    renderReplayComparisonRow "Total ledger events" (string selectedState.TotalLedgerEvents) (string currentState.TotalLedgerEvents)
                }
            }
        }

        h3 {
            attr.``class`` "title is-6"
            text "Replay Delta vs Current"
        }

        div {
            attr.``class`` "table-container mb-3"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Measure" }
                        th { text "Selected - current" }
                    }
                }

                tbody {
                    renderReplayDeltaRow "Parsed Phi delta" selectedState.ParsedPhiEvents currentState.ParsedPhiEvents
                    renderReplayDeltaRow "Included Phi delta" selectedState.IncludedPhiCount currentState.IncludedPhiCount
                    renderReplayDeltaRow "Excluded Phi delta" selectedState.ExcludedPhiCount currentState.ExcludedPhiCount
                    renderReplayDeltaRow "Accepted decision delta" selectedState.GovernanceAccepted currentState.GovernanceAccepted
                    renderReplayDeltaRow "Rejected decision delta" selectedState.GovernanceRejected currentState.GovernanceRejected
                    renderReplayDeltaRow "Held decision delta" selectedState.GovernanceHeld currentState.GovernanceHeld
                }
            }
        }
    }

let renderReplayPreviewPanel (replayPreviewSequence: int option) (ledgerEvents: LedgerEvent list) dispatch =
    div {
        attr.``class`` "box"

        div {
            attr.``class`` "level mb-3"

            div {
                attr.``class`` "level-left"
                h2 {
                    attr.``class`` "title is-5 mb-0"
                    text "Replay Preview Lite"
                }
            }

            match replayPreviewSequence with
            | None ->
                empty()
            | Some _ ->
                div {
                    attr.``class`` "level-right"
                    button {
                        attr.``class`` "button is-small is-light"
                        attr.``type`` "button"
                        on.click (fun _ -> dispatch ClearReplayPreview)
                        text "Clear preview"
                    }
                }
        }

        match replayPreviewSequence with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "Select a ledger event to preview the project state at that point."
            }
        | Some selectedSequence ->
            let selectedEvents = getReplayPreviewEvents selectedSequence ledgerEvents
            let selectedState = buildReplayPreviewState selectedEvents
            let currentState = buildReplayPreviewState ledgerEvents
            let selectedLedgerEvent = ledgerEvents |> List.tryFind (fun ledgerEvent -> ledgerEvent.SequenceNumber = selectedSequence)

            div {
                attr.``class`` "tags mb-3"
                span {
                    attr.``class`` "tag is-link"
                    text ("Selected #" + string selectedSequence)
                }

                match selectedLedgerEvent with
                | None ->
                    span {
                        attr.``class`` "tag is-warning is-light"
                        text "Selected ledger event not found"
                    }
                | Some ledgerEvent ->
                    span {
                        attr.``class`` "tag is-light"
                        text ledgerEvent.EventId
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text ledgerEvent.EventKind
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text ledgerEvent.TargetId
                    }
            }

            renderReplayPreviewTables selectedState currentState

        p {
            attr.``class`` "notification is-warning is-light is-size-7"
            text "Replay Preview Lite reconstructs compact state from ledger events only. Full Sigma/Gamma reconstruction will require richer event payloads or checkpoints."
        }
    }

let renderLedgerTab (ledgerEvents: LedgerEvent list) (replayPreviewSequence: int option) dispatch =
    let totalEvents = List.length ledgerEvents
    let phiEvents = countLedgerEvents isPhiLedgerEvent ledgerEvents
    let replayEvents = countLedgerEvents isReplayLedgerEvent ledgerEvents
    let governanceEvents = countLedgerEvents isGovernanceLedgerEvent ledgerEvents

    div {
        attr.``class`` "mb-6 pb-5"

        h2 {
            attr.``class`` "title is-4"
            text "Clio Ledger Lite"
        }

        div {
            attr.``class`` "box"

            div {
                attr.``class`` "tags mb-3"
                renderLedgerCounter "Total events" totalEvents
                renderLedgerCounter "Phi events" phiEvents
                renderLedgerCounter "Replay events" replayEvents
                renderLedgerCounter "Governance events" governanceEvents
            }

            match ledgerEvents with
            | [] ->
                p {
                    attr.``class`` "has-text-grey"
                    text "No ledger events recorded yet."
                }
            | events ->
                div {
                    attr.``class`` "table-container"
                    table {
                        attr.``class`` "table is-fullwidth is-striped is-narrow"

                        thead {
                            tr {
                                th { text "#" }
                                th { text "Time UTC" }
                                th { text "Event kind" }
                                th { text "Target" }
                                th { text "Summary" }
                                th { text "Detail" }
                                th { text "Action" }
                            }
                        }

                        tbody {
                            forEach events <| fun ledgerEvent ->
                                let isSelectedForPreview = replayPreviewSequence = Some ledgerEvent.SequenceNumber

                                tr {
                                    attr.``class`` (
                                        if isSelectedForPreview then
                                            "is-selected"
                                        else
                                            "")
                                    td { text (string ledgerEvent.SequenceNumber) }
                                    td { text ledgerEvent.TimestampUtc }
                                    td { text ledgerEvent.EventKind }
                                    td { text ledgerEvent.TargetId }
                                    td { text ledgerEvent.Summary }
                                    td { text ledgerEvent.Detail }
                                    td {
                                        button {
                                            attr.``class`` (
                                                if isSelectedForPreview then
                                                    "button is-small is-link"
                                                else
                                                    "button is-small is-link is-light")
                                            attr.``type`` "button"
                                            on.click (fun _ -> dispatch (SelectReplayPreview ledgerEvent.SequenceNumber))
                                            text (
                                                if isSelectedForPreview then
                                                    "Selected"
                                                else
                                                    "Replay here")
                                        }
                                    }
                                }
                        }
                    }
                }
        }

        renderReplayPreviewPanel replayPreviewSequence ledgerEvents dispatch
    }
