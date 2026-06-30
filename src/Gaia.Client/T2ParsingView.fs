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

let renderMatchedGroup title names =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        match names with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No matches"
            }
        | xs ->
            div {
                attr.``class`` "tags"
                forEach xs <| fun name ->
                    span {
                        attr.``class`` "tag is-info is-light"
                        text name
                    }
            }
    }

let renderKnownContextGroup title (entries: SigmaContextEntry list) =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        match entries with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No known items yet."
            }
        | items ->
            div {
                forEach items <| fun entry ->
                    div {
                        attr.``class`` "mb-4"
                        p {
                            strong { text ("Value: " + entry.Value + " [" + string entry.SupportCount + "]") }
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-1"
                            text ("SourcePhiId: " + entry.SourcePhiId)
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey mb-1"
                            text ("SourcePhiStatement: " + entry.SourcePhiStatement)
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey"
                            text ("Parse sequence number: " + string entry.ParseSequenceNumber)
                        }
                        p {
                            attr.``class`` "is-size-7 has-text-grey"
                            text ("Provenance: " + entry.Provenance)
                        }
                    }
            }
    }

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

let renderDeltaSigmaAtomGroup title atoms =
    div {
        attr.``class`` "mb-3"
        h4 {
            attr.``class`` "title is-6 mb-2"
            text title
        }

        match atoms with
        | [] ->
            p {
                attr.``class`` "has-text-grey is-size-7"
                text "None."
            }
        | values ->
            ul {
                forEach values <| fun value ->
                    li { text value }
            }
    }

let renderDeltaSigmaAtomColumn title (atomGroups: DeltaSigmaAtomGroups) =
    div {
        attr.``class`` "column is-4"

        h3 {
            attr.``class`` "title is-6"
            text title
        }

        renderDeltaSigmaAtomGroup "Functions" atomGroups.FunctionAtoms
        renderDeltaSigmaAtomGroup "Modes" atomGroups.ModeAtoms
        renderDeltaSigmaAtomGroup "Interfaces" atomGroups.InterfaceAtoms
        renderDeltaSigmaAtomGroup "States" atomGroups.StateAtoms
        renderDeltaSigmaAtomGroup "Hosts" atomGroups.HostAtoms
        renderDeltaSigmaAtomGroup "Constraints" atomGroups.ConstraintAtoms
    }

let renderDeltaSigmaAnalysisPanel (lastReplayAction: DeltaSigmaAnalysis option) =
    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Replay Engine Lite"
        }

        h2 {
            attr.``class`` "title is-5"
            text "ΔΣ Analysis — Last Replay Action"
        }

        match lastReplayAction with
        | None ->
            p {
                attr.``class`` "has-text-grey"
                text "No replay action yet."
            }

        | Some analysis ->
            p {
                strong { text "Action: " }
                text analysis.Action
            }

            p {
                strong { text "Source statement: " }
                text analysis.SourceStatement
            }

            p {
                attr.``class`` "mb-4"
                strong { text "Why: " }
                text analysis.Reason
            }

            if hasDeltaSigmaAnalysisChanges analysis then
                div {
                    attr.``class`` "columns"
                    renderDeltaSigmaAtomColumn "Added atoms" analysis.AddedAtoms
                    renderDeltaSigmaAtomColumn "Removed atoms" analysis.RemovedAtoms
                    renderDeltaSigmaAtomColumn "Already known / reinforced atoms" analysis.AlreadyKnownAtoms
                }
            else
                p {
                    attr.``class`` "has-text-grey"
                    text "No Sigma atoms changed."
                }
    }

let renderSummaryBox title body =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        p {
            text body
        }
    }

let renderSummaryCard title body =
    div {
        attr.``class`` "card mb-4"
        div {
            attr.``class`` "card-content"
            p {
                attr.``class`` "heading"
                text title
            }
            p {
                text body
            }
        }
    }

let renderExecutionPathCard steps =
    div {
        attr.``class`` "card"
        div {
            attr.``class`` "card-content"
            h3 {
                attr.``class`` "title is-6"
                text "Execution path"
            }
            ol {
                forEach steps <| fun step ->
                    li { text step }
            }
        }
    }

let renderExposureChain (parse: PhiParse) =
    let chain =
        [
            "Function", parse.Exposure.Function
            "Mode", parse.Exposure.Mode
            "Interface", parse.Exposure.Interface
            "State", parse.Exposure.State
            "Host", parse.Exposure.HostCandidate
        ]
        |> List.map (fun (label, value) -> label, value, value = "")
        |> List.mapi (fun index step -> index, step)

    let lastIndex = List.length chain - 1

    div {
        attr.``class`` "card mb-4"

        div {
            attr.``class`` "card-content"

            h3 {
                attr.``class`` "title is-6"
                text "Exposure chain"
            }

            div {
                attr.``class`` "is-flex is-align-items-stretch is-flex-wrap-nowrap"

                forEach chain <| fun (index, (label, value, isMissing)) ->
                    div {
                        attr.``class`` "is-flex is-align-items-stretch is-flex-grow-1"

                        div {
                            attr.``class`` "box p-0 mb-0 is-flex-grow-1"

                            div {
                                attr.``class`` "has-background-black-ter has-text-white has-text-weight-semibold px-3 py-2"
                                text label
                            }

                            div {
                                attr.``class`` (
                                    if isMissing then
                                        "has-background-warning-light has-text-warning-dark has-text-weight-semibold px-3 py-3"
                                    else
                                        "has-background-white-ter has-text-dark px-3 py-3")
                                text (
                                    if isMissing then
                                        "Missing"
                                    else
                                        value)
                            }
                        }

                        if index < lastIndex then
                            div {
                                attr.``class`` "is-flex is-align-items-center px-2 has-text-grey has-text-weight-bold"
                                text "->"
                            }
                    }
            }
        }
    }

let renderRelevantSigmaContextPanel sequencedParsedPhis staleParsedPhiIds selectedPhiParse selectedPhiResolution =
    let sigmaContext = buildSigmaContext sequencedParsedPhis
    let staleParsedPhiCount =
        sequencedParsedPhis
        |> List.map snd
        |> getStaleParsedPhiCount staleParsedPhiIds

    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Current Σ Snapshot"
        }

        renderSigmaSnapshotCounts (List.length sequencedParsedPhis) staleParsedPhiCount sigmaContext

        h2 {
            attr.``class`` "title is-5"
            text "T3 — Relevant Σ Context"
        }

        match sequencedParsedPhis with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "Include a parsed Φ to reconstruct relevant Σ context."
            }

        | _ ->
            div {
                attr.``class`` "columns is-multiline"

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Functions" sigmaContext.Functions
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Modes" sigmaContext.Modes
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Interfaces" sigmaContext.Interfaces
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known States" sigmaContext.States
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Hosts" sigmaContext.Hosts
                }

                div {
                    attr.``class`` "column is-6"
                    renderKnownContextGroup "Known Constraints" sigmaContext.Constraints
                }
            }

            match selectedPhiParse, selectedPhiResolution with
            | Some parse, Some resolution ->
                let missingContext =
                    [
                        if parse.Exposure.Function <> "" && List.isEmpty resolution.MatchedFRs then
                            yield "Function not found in current Σ."

                        if parse.Exposure.Interface <> "" then
                            yield "Interface parsed; explicit interface storage is not yet modeled in Σ."

                        if parse.Exposure.State <> "" then
                            yield "State parsed; explicit state storage is not yet modeled in Σ."

                        if parse.Exposure.Mode <> "" then
                            yield "Mode parsed; explicit mode storage is not yet modeled in Σ."

                        if parse.Exposure.HostCandidate = "" then
                            yield "Host candidate missing."
                    ]

                div {
                    attr.``class`` "box"
                    h3 {
                        attr.``class`` "title is-6"
                        text "Missing / unresolved context"
                    }

                    match missingContext with
                    | [] ->
                        p {
                            attr.``class`` "has-text-grey"
                            text "No missing or unresolved context."
                        }
                    | messages ->
                        ul {
                            forEach messages <| fun message ->
                                li { text message }
                        }
                }

            | _ ->
                empty()
    }

let renderParseDetailsPanel selectedPhiParse selectedPhiResolution =
    div {
        attr.``class`` "box"
        h2 {
            attr.``class`` "title is-5"
            text "T2: Parse"
        }

        match selectedPhiParse, selectedPhiResolution with
        | Some parse, Some resolution ->
            let admissibility = getAdmissibilityResult parse

            p {
                attr.``class`` "is-size-7 has-text-grey"
                text parse.PhiId
            }

            h3 {
                attr.``class`` "title is-6"
                text "Selected Φ"
            }
            p { text parse.Statement }

            div {
                attr.``class`` "mb-4"
                span {
                    attr.``class`` (admissibilityBadgeClass admissibility)
                    text (formatAdmissibilityResult admissibility)
                }
            }

            renderExposureChain parse

            renderSummaryCard "ΔΣ" resolution.DeltaSigmaSummary
            renderSummaryCard "Γ" resolution.GammaSummary
            renderExecutionPathCard resolution.ExecutionPath

        | _ ->
            p {
                attr.``class`` "has-text-grey"
                text "Select an ingested Φ to prepare a structural parse."
            }
    }

