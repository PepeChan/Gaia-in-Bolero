module Gaia.Client.T3SummaryView

open Bolero
open Bolero.Html
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Workflow
open Gaia.Client.T2ParsingView

let renderCurrentSigmaSnapshotPanel sequencedParsedPhis sigmaContext =
    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Current Σ Snapshot"
        }

        renderSigmaSnapshotCounts (List.length sequencedParsedPhis) sigmaContext
    }

let getReinforcedAtomCount (entries: SigmaContextEntry list) =
    entries
    |> List.filter (fun entry -> entry.SupportCount > 1)
    |> List.length

let getSigmaSummaryRows (sigmaContext: SigmaContext) =
    [
        "Functions", sigmaContext.Functions
        "Modes", sigmaContext.Modes
        "Interfaces", sigmaContext.Interfaces
        "States", sigmaContext.States
        "Hosts", sigmaContext.Hosts
    ]

let hasExposureValue value =
    value <> ""

let hasFunctionExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Function

let hasModeExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Mode

let hasInterfaceExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.Interface

let hasStateExposure (parse: PhiParse) =
    hasExposureValue parse.Exposure.State

let hasAnyStructuralExposure (parse: PhiParse) =
    hasFunctionExposure parse
    || hasModeExposure parse
    || hasInterfaceExposure parse
    || hasStateExposure parse

let countIncludedParsedPhisWith (predicate: PhiParse -> bool) (sequencedParsedPhis: (int * PhiParse) list) =
    sequencedParsedPhis
    |> List.map snd
    |> List.filter predicate
    |> List.length

let interpretMissingContextCount count =
    if count = 0 then
        "No immediate gap detected."
    elif count <= 2 then
        "Low architectural gap."
    elif count <= 5 then
        "Medium architectural gap."
    else
        "High architectural gap."

let interpretPressure count =
    if count = 0 then
        "None"
    elif count <= 2 then
        "Low"
    elif count <= 5 then
        "Medium"
    else
        "High"

let getMissingContextSummaryRows (sequencedParsedPhis: (int * PhiParse) list) =
    [
        "Hosts",
        countIncludedParsedPhisWith
            (fun parse -> parse.Exposure.HostCandidate = "" && hasAnyStructuralExposure parse)
            sequencedParsedPhis

        "Interfaces",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.Interface = ""
                && hasFunctionExposure parse
                && (hasModeExposure parse || hasStateExposure parse))
            sequencedParsedPhis

        "Modes",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.Mode = ""
                && hasFunctionExposure parse
                && (hasStateExposure parse || hasInterfaceExposure parse))
            sequencedParsedPhis

        "States",
        countIncludedParsedPhisWith
            (fun parse ->
                parse.Exposure.State = ""
                && hasFunctionExposure parse
                && (hasModeExposure parse || hasInterfaceExposure parse))
            sequencedParsedPhis
    ]

let getArchitecturalPressureRows (sigmaContext: SigmaContext) =
    let hostBasisCount =
        if List.isEmpty sigmaContext.Hosts then
            List.length sigmaContext.Functions + List.length sigmaContext.States
        else
            0

    [
        "Host",
        hostBasisCount,
        "Functions/states exist but no host candidates are present."

        "Interface",
        List.length sigmaContext.Interfaces,
        "Interface atoms are available for boundary reasoning."

        "State",
        List.length sigmaContext.States,
        "State atoms are available for condition and behavior reasoning."

        "Mode",
        List.length sigmaContext.Modes,
        "Mode atoms are available for operational-context reasoning."
    ]

let getReinforcedAtoms (sigmaContext: SigmaContext) =
    getSigmaSummaryRows sigmaContext
    |> List.collect (fun (kind, entries) ->
        entries
        |> List.filter (fun entry -> entry.SupportCount > 1)
        |> List.map (fun entry -> kind, entry))

let countDeltaSigmaAtoms (atomGroups: DeltaSigmaAtomGroups) =
    [
        atomGroups.FunctionAtoms
        atomGroups.ModeAtoms
        atomGroups.InterfaceAtoms
        atomGroups.StateAtoms
        atomGroups.HostAtoms
    ]
    |> List.sumBy (fun atoms -> List.length atoms)

let renderCurrentSigmaSummaryTable sigmaContext =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Current Sigma Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Atom kind" }
                        th { text "Count" }
                        th { text "Reinforced atoms" }
                    }
                }

                tbody {
                    forEach (getSigmaSummaryRows sigmaContext) <| fun (kind, entries) ->
                        tr {
                            td { text kind }
                            td { text (string (List.length entries)) }
                            td { text (string (getReinforcedAtomCount entries)) }
                        }
                }
            }
        }
    }

let renderMissingContextSummaryTable sequencedParsedPhis =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Missing Context Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Missing area" }
                        th { text "Count" }
                        th { text "Interpretation" }
                    }
                }

                tbody {
                    forEach (getMissingContextSummaryRows sequencedParsedPhis) <| fun (missingArea, count) ->
                        tr {
                            td { text missingArea }
                            td { text (string count) }
                            td { text (interpretMissingContextCount count) }
                        }
                }
            }
        }
    }

let renderArchitecturalPressureSummaryTable sigmaContext =
    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Architectural Pressure Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate target" }
                        th { text "Basis count" }
                        th { text "Pressure" }
                        th { text "Meaning" }
                    }
                }

                tbody {
                    forEach (getArchitecturalPressureRows sigmaContext) <| fun (target, basisCount, meaning) ->
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

let renderTopReinforcedAtomsTable sigmaContext =
    let reinforcedAtoms = getReinforcedAtoms sigmaContext

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "Top Reinforced Atoms"
        }

        match reinforcedAtoms with
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
                                td { text (String.concat ", " entry.SupportingPhiIds) }
                            }
                    }
                }
            }
    }

let renderT4CandidateSummaryTable sigmaContext =
    let candidateDeltas = formulateCandidateDeltas sigmaContext

    div {
        attr.``class`` "mb-5"

        h3 {
            attr.``class`` "title is-6"
            text "T4 Candidate Summary"
        }

        div {
            attr.``class`` "table-container"
            table {
                attr.``class`` "table is-fullwidth is-striped is-narrow"

                thead {
                    tr {
                        th { text "Candidate type" }
                        th { text "Target" }
                        th { text "Basis count" }
                        th { text "Status" }
                    }
                }

                tbody {
                    forEach candidateDeltas <| fun candidate ->
                        tr {
                            td { text (formatCandidateDeltaKind candidate.Kind) }
                            td { text candidate.Target }
                            td { text (string (List.length candidate.RelevantSigmaBasis)) }
                            td { text "Candidate only" }
                        }
                }
            }
        }
    }
