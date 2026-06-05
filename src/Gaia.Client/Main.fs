module Gaia.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Gaia.Core

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Probe

/// The Elmish application's model.
type Model =
    {
        page: Page
        error: string option
        selectedScenarioId: string option
        scenarioResolution: ResolutionView option
        phiDraftRawStatement: string
        phiDraftTriggerContext: string
        phiDraftSource: string
        phiDraftQuickTags: string
        phiDraftConfidence: string
        ingestedPhis: PhiIntake list
        parsedPhis: PhiParse list
        selectedPhiId: string option
        selectedPhiParse: PhiParse option
        selectedPhiResolution: ResolutionView option
    }

let demoScenarios = DemoData.demoScenarios

let tryFindScenario scenarioId =
    demoScenarios
    |> List.tryFind (fun scenario -> scenario.Id = scenarioId)

let resolveScenario scenarioId =
    tryFindScenario scenarioId
    |> Option.map (fun scenario -> Engine.resolveParse DemoData.demoSigma scenario.Parse)

let initialScenario =
    demoScenarios
    |> List.tryHead

let initModel =
    let selectedScenarioId =
        initialScenario
        |> Option.map (fun scenario -> scenario.Id)

    let scenarioResolution =
        selectedScenarioId
        |> Option.bind resolveScenario

    {
        page = Probe
        error = None
        selectedScenarioId = selectedScenarioId
        scenarioResolution = scenarioResolution
        phiDraftRawStatement = ""
        phiDraftTriggerContext = ""
        phiDraftSource = ""
        phiDraftQuickTags = ""
        phiDraftConfidence = "Medium"
        ingestedPhis = DemoData.demoPhiIntakes
        parsedPhis = []
        selectedPhiId = None
        selectedPhiParse = None
        selectedPhiResolution = None
    }
/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SelectScenario of string
    | Error of exn
    | ClearError
    | SetPhiDraftRawStatement of string
    | SetPhiDraftTriggerContext of string
    | SetPhiDraftSource of string
    | SetPhiDraftQuickTags of string
    | SetPhiDraftConfidence of string
    | IngestPhiDraft
    | ParseIngestedPhi of string

let upsertParsedPhi parse parsedPhis =
    if parsedPhis |> List.exists (fun parsedPhi -> parsedPhi.PhiId = parse.PhiId) then
        parsedPhis
        |> List.map (fun parsedPhi ->
            if parsedPhi.PhiId = parse.PhiId then
                parse
            else
                parsedPhi)
    else
        parsedPhis @ [ parse ]

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | SelectScenario scenarioId ->
        match tryFindScenario scenarioId with
        | Some scenario ->
            { model with
                selectedScenarioId = Some scenario.Id
                scenarioResolution = Some (Engine.resolveParse DemoData.demoSigma scenario.Parse) }, Cmd.none
        | None ->
            model, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
    | SetPhiDraftRawStatement value ->
        { model with phiDraftRawStatement = value }, Cmd.none

    | SetPhiDraftTriggerContext value ->
        { model with phiDraftTriggerContext = value }, Cmd.none

    | SetPhiDraftSource value ->
        { model with phiDraftSource = value }, Cmd.none

    | SetPhiDraftQuickTags value ->
        { model with phiDraftQuickTags = value }, Cmd.none

    | SetPhiDraftConfidence value ->
        { model with phiDraftConfidence = value }, Cmd.none

    | ParseIngestedPhi phiId ->
        match model.ingestedPhis |> List.tryFind (fun phi -> phi.PhiId = phiId) with
        | Some phi ->
            let parse = Engine.parseIntake phi
            let resolution = Engine.resolveParse DemoData.demoSigma parse
            let parsedPhis = upsertParsedPhi parse model.parsedPhis

            { model with
                selectedPhiId = Some phi.PhiId
                selectedPhiParse = Some parse
                selectedPhiResolution = Some resolution
                parsedPhis = parsedPhis }, Cmd.none

        | None ->
            model, Cmd.none

    | IngestPhiDraft ->
        let intake =
            {
                PhiId = "PHI-" + DateTime.UtcNow.ToString("yyyyMMdd-HHmmss")
                Date = DateTime.UtcNow.ToString("yyyy-MM-dd")
                Source = model.phiDraftSource
                Context = model.phiDraftTriggerContext
                Confidence = model.phiDraftConfidence
                Status = "Ingested"
                RawStatement = model.phiDraftRawStatement
                Trigger = model.phiDraftTriggerContext
                Claim = ""
                About = ""
                Condition = ""
                Assumption = ""
                TypeText = ""
                Impact = ""
                UnresolvedSignal = ""
            }

        { model with
            ingestedPhis = intake :: model.ingestedPhis
            phiDraftRawStatement = ""
            phiDraftTriggerContext = ""
            phiDraftSource = ""
            phiDraftQuickTags = ""
            phiDraftConfidence = "Medium" }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate

type SigmaContextEntry =
    {
        Value: string
        SourcePhiId: string
        SourcePhiStatement: string
        ParseSequenceNumber: int
    }

type SigmaContext =
    {
        Functions: SigmaContextEntry list
        Modes: SigmaContextEntry list
        Interfaces: SigmaContextEntry list
        States: SigmaContextEntry list
        Hosts: SigmaContextEntry list
    }

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

let buildSigmaContextEntries getValue parsedPhis =
    parsedPhis
    |> List.mapi (fun index parse -> index + 1, parse)
    |> List.choose (fun (parseSequenceNumber, parse) ->
        let value = getValue parse

        if value = "" then
            None
        else
            Some
                {
                    Value = value
                    SourcePhiId = parse.PhiId
                    SourcePhiStatement = parse.Statement
                    ParseSequenceNumber = parseSequenceNumber
                })
    |> List.distinctBy (fun entry -> entry.Value)

let buildSigmaContext parsedPhis =
    {
        Functions = buildSigmaContextEntries (fun parse -> parse.Exposure.Function) parsedPhis
        Modes = buildSigmaContextEntries (fun parse -> parse.Exposure.Mode) parsedPhis
        Interfaces = buildSigmaContextEntries (fun parse -> parse.Exposure.Interface) parsedPhis
        States = buildSigmaContextEntries (fun parse -> parse.Exposure.State) parsedPhis
        Hosts = buildSigmaContextEntries (fun parse -> parse.Exposure.HostCandidate) parsedPhis
    }

let renderKnownContextGroup title entries =
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
                            strong { text ("Value: " + entry.Value) }
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
                    }
            }
    }

let renderSigmaSnapshotMetric label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let renderSigmaSnapshotCounts parsedPhiCount sigmaContext =
    div {
        attr.``class`` "tags are-medium mb-4"
        renderSigmaSnapshotMetric "Total parsed Φ" parsedPhiCount
        renderSigmaSnapshotMetric "Functions" (List.length sigmaContext.Functions)
        renderSigmaSnapshotMetric "Modes" (List.length sigmaContext.Modes)
        renderSigmaSnapshotMetric "Interfaces" (List.length sigmaContext.Interfaces)
        renderSigmaSnapshotMetric "States" (List.length sigmaContext.States)
        renderSigmaSnapshotMetric "Hosts" (List.length sigmaContext.Hosts)
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

let renderRelevantSigmaContextPanel parsedPhis selectedPhiParse selectedPhiResolution =
    let sigmaContext = buildSigmaContext parsedPhis

    div {
        attr.``class`` "box"

        p {
            attr.``class`` "heading"
            text "Current Σ Snapshot"
        }

        renderSigmaSnapshotCounts (List.length parsedPhis) sigmaContext

        h2 {
            attr.``class`` "title is-5"
            text "T3 — Relevant Σ Context"
        }

        match parsedPhis with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "Parse a Φ to reconstruct relevant Σ context."
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

let homePage model dispatch =
    match tryGetSelectedScenario model, model.scenarioResolution with
    | Some scenario, Some resolution ->
        let admissibility = getAdmissibilityResult scenario.Parse
        let matchedFrNames = mapIdsToNames (fun (fr: FR) -> fr.Id) (fun fr -> fr.Name) DemoData.demoSigma.FRs resolution.MatchedFRs
        let matchedDpNames = mapIdsToNames (fun (dp: DP) -> dp.Id) (fun dp -> dp.Name) DemoData.demoSigma.DPs resolution.MatchedDPs
        let matchedTfNames = mapIdsToNames (fun (tf: TF) -> tf.Id) (fun tf -> tf.Name) DemoData.demoSigma.TFs resolution.MatchedTFs
        let matchedCtqNames = mapIdsToNames (fun (ctq: CTQ) -> ctq.Id) (fun ctq -> ctq.Name) DemoData.demoSigma.CTQs resolution.MatchedCTQs

        div {
            attr.``class`` "content"
            h1 {
                attr.``class`` "title"
                text "Gaia Probe Dashboard"
            }
            p {
                attr.``class`` "subtitle is-6"
                text "Probe demo scenarios, resolve them through Gaia.Core, and inspect the resulting path and matches."
            }

            div {
                attr.``class`` "mb-6 pb-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Live Gaia Workflow"
                }

                div {
                    attr.``class`` "tags are-medium mb-5"
                    span {
                        attr.``class`` "tag is-link"
                        text "T1"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "Φ Set"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T2 Parse"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T3 Relevant Σ Context"
                    }
                }

                div {
                    attr.``class`` "columns is-variable is-5"

                    div {
                        attr.``class`` "column is-4"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "T1 — Φ Ingestion"
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Raw statement / observation"
                                }
                                div {
                                    attr.``class`` "control"
                                    textarea {
                                        attr.``class`` "textarea"
                                        attr.placeholder "Write the Φ as provided..."
                                        bind.input.string model.phiDraftRawStatement (fun v -> dispatch (SetPhiDraftRawStatement v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Trigger context"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "Why did this matter?"
                                        bind.input.string model.phiDraftTriggerContext (fun v -> dispatch (SetPhiDraftTriggerContext v))
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
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "User, observation, requirement, review..."
                                        bind.input.string model.phiDraftSource (fun v -> dispatch (SetPhiDraftSource v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Quick tags"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "function, mode, interface, state, unknown..."
                                        bind.input.string model.phiDraftQuickTags (fun v -> dispatch (SetPhiDraftQuickTags v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Confidence"
                                }
                                div {
                                    attr.``class`` "control"
                                    div {
                                        attr.``class`` "select is-fullwidth"
                                        select {
                                            bind.input.string model.phiDraftConfidence (fun v -> dispatch (SetPhiDraftConfidence v))
                                            option { text "High" }
                                            option { text "Medium" }
                                            option { text "Low" }
                                        }
                                    }
                                }
                            }

                            button {
                                attr.``class`` "button is-link is-fullwidth"
                                attr.``type`` "button"
                                on.click (fun _ -> dispatch IngestPhiDraft)
                                text "Ingest Φ"
                            }
                        }

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "Φ Set"
                            }

                            match model.ingestedPhis with
                            | [] ->
                                p {
                                    attr.``class`` "has-text-grey"
                                    text "No Φ ingested yet."
                                }
                            | phis ->
                                div {
                                    attr.``class`` "content"
                                    forEach phis <| fun phi ->
                                        div {
                                            attr.``class`` "box"
                                            p {
                                                strong { text phi.PhiId }
                                            }
                                            p {
                                                text phi.RawStatement
                                            }
                                            p {
                                                attr.``class`` "is-size-7 has-text-grey"
                                                text ("Source: " + phi.Source + " | Confidence: " + phi.Confidence)
                                            }
                                            button {
                                                attr.``class`` "button is-small is-link is-light"
                                                attr.``type`` "button"
                                                on.click (fun _ -> dispatch (ParseIngestedPhi phi.PhiId))
                                                text "Parse Φ"
                                            }
                                        }
                                }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "T2: Parse"
                            }

                            match model.selectedPhiParse, model.selectedPhiResolution with
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

                        renderRelevantSigmaContextPanel model.parsedPhis model.selectedPhiParse model.selectedPhiResolution
                    }
                }
            }

            hr {}

            div {
                attr.``class`` "pt-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Legacy Examples"
                }

                div {
                    attr.``class`` "columns is-variable is-5"

                    div {
                        attr.``class`` "column is-4"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "Demo Scenarios / Legacy Examples"
                            }
                            div {
                                attr.``class`` "buttons"
                                forEach demoScenarios <| fun candidate ->
                                    button {
                                        attr.``class`` (
                                            if Some candidate.Id = model.selectedScenarioId then
                                                "button is-link is-fullwidth"
                                            else
                                                "button is-fullwidth")
                                        attr.``type`` "button"
                                        on.click (fun _ -> dispatch (SelectScenario candidate.Id))
                                        text candidate.Title
                                    }
                            }
                            p {
                                attr.``class`` "has-text-grey"
                                text scenario.Description
                            }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-4"
                                text "Legacy Scenario Resolution"
                            }
                            p {
                                attr.``class`` "is-size-7 has-text-grey"
                                text scenario.Parse.PhiId
                            }
                            div {
                                attr.``class`` "mb-4"
                                h3 {
                                    attr.``class`` "title is-6"
                                    text "Admissibility Result"
                                }
                                span {
                                    attr.``class`` (admissibilityBadgeClass admissibility)
                                    text (formatAdmissibilityResult admissibility)
                                }
                            }

                            h3 {
                                attr.``class`` "title is-6"
                                text "Φ statement"
                            }
                            p {
                                text scenario.Parse.Statement
                            }
                        }

                        div {
                            attr.``class`` "columns"

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "Selected derivation entry"
                                    (formatDerivationEntry resolution.SelectedEntry)
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "DeltaSigmaSummary"
                                    resolution.DeltaSigmaSummary
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "Delta Candidate"
                                    resolution.DeltaCandidateSummary
                            }

                            div {
                                attr.``class`` "column is-3"
                                renderSummaryBox
                                    "GammaSummary"
                                    resolution.GammaSummary
                            }
                        }

                        div {
                            attr.``class`` "box"
                            h3 {
                                attr.``class`` "title is-6"
                                text "Execution path"
                            }
                            ol {
                                forEach resolution.ExecutionPath <| fun step ->
                                    li {
                                        text step
                                    }
                            }
                        }

                        div {
                            attr.``class`` "columns is-multiline"

                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched FR names" matchedFrNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched DP names" matchedDpNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched TF names" matchedTfNames
                            }
                            div {
                                attr.``class`` "column is-6"
                                renderMatchedGroup "Matched CTQ names" matchedCtqNames
                            }
                        }
                    }
                }
            }
        }
    | _ ->
        div {
            attr.``class`` "notification is-warning"
            text "No demo scenarios are available."
        }

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model Probe "Gaia Probe"            
        })
        .Body(
            cond model.page <| function
            | Probe -> homePage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
