module Gaia.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Persistence
open Gaia.Client.Ledger
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.Inquiry
open Gaia.Client.AppUpdate
open Gaia.Client.T2ParsingView
open Gaia.Client.T4CandidateView
open Gaia.Client.T3SummaryView
open Gaia.Client.T5GovernanceView
open Gaia.Client.LedgerView
open Gaia.Client.EvidenceView
open Gaia.Client.FactsReconstructionView
open Gaia.Client.T6RealizationView

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let private optionalMetadata label value =
    match value with
    | Some text when not (String.IsNullOrWhiteSpace(text)) ->
        Some (label + ": " + text)
    | _ ->
        None

let renderPersistenceTab (model: Model) dispatch =
    div {
        attr.``class`` "mb-6 pb-5"

        h2 {
            attr.``class`` "title is-4"
            text "Projects"
        }

        div {
            attr.``class`` "box"

            h3 {
                attr.``class`` "title is-5"
                text "Project Files"
            }

            p {
                attr.``class`` "has-text-grey mb-4"
                text "Save or open Cognopy project .json files."
            }

            div {
                attr.``class`` "columns is-variable is-5"

                div {
                    attr.``class`` "column is-4"

                    div {
                        attr.``class`` "field"
                        label {
                            attr.``class`` "label"
                            text "Project name"
                        }
                        div {
                            attr.``class`` "control"
                            input {
                                attr.``class`` "input"
                                bind.input.string model.projectName (fun value -> dispatch (SetProjectName value))
                            }
                        }
                    }

                    div {
                        attr.``class`` "buttons"
                        button {
                            attr.``class`` "button is-link"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch SaveProjectFile)
                            text "Save Project File"
                        }
                        button {
                            attr.``class`` "button is-success"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch OpenProjectFile)
                            text "Open Project File"
                        }
                    }

                    div {
                        attr.``class`` "buttons mt-4"
                        button {
                            attr.``class`` "button is-info is-light"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch LoadSphynxSampleProject)
                            text "Load Sphynx sample"
                        }
                        button {
                            attr.``class`` "button is-warning is-light"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch ClearProject)
                            text "Clear project"
                        }
                    }
                }

                div {
                    attr.``class`` "column is-8"

                    div {
                        attr.``class`` "tags"
                        span {
                            attr.``class`` "tag is-light"
                            text ("Forward inquiries / Phi intakes: " + string (List.length model.ingestedPhis))
                        }
                        span {
                            attr.``class`` "tag is-light"
                            text ("Parsed Phis: " + string (List.length model.parsedPhis))
                        }
                        span {
                            attr.``class`` "tag is-light"
                            text ("Ledger events: " + string (List.length model.LedgerEvents))
                        }
                        span {
                            attr.``class`` "tag is-light"
                            text ("Evidence records: " + string (List.length model.evidenceRecords))
                        }
                    }

                    match model.persistenceStatus with
                    | None ->
                        p {
                            attr.``class`` "has-text-grey"
                            text "No project file action yet."
                        }
                    | Some status ->
                        div {
                            attr.``class`` "notification is-info is-light"
                            text status
                        }
                }
            }
        }

        div {
            attr.``class`` "box"

            h3 {
                attr.``class`` "title is-5"
                text "JSON Import / Export"
            }

            p {
                attr.``class`` "has-text-grey mb-4"
                text "Advanced: copy/paste JSON project snapshots."
            }

            div {
                attr.``class`` "buttons"
                button {
                    attr.``class`` "button is-info"
                    attr.``type`` "button"
                    on.click (fun _ -> dispatch ExportProjectJson)
                    text "Export JSON"
                }
                button {
                    attr.``class`` "button is-success"
                    attr.``type`` "button"
                    on.click (fun _ -> dispatch ImportProjectJson)
                    text "Import JSON"
                }
            }

            div {
                attr.``class`` "columns is-variable is-5"

                div {
                    attr.``class`` "column is-6"
                    h4 {
                        attr.``class`` "title is-6"
                        text "Export JSON"
                    }
                    textarea {
                        attr.``class`` "textarea"
                        attr.style "min-height: 18rem; font-family: monospace;"
                        text model.exportJson
                    }
                }

                div {
                    attr.``class`` "column is-6"
                    h4 {
                        attr.``class`` "title is-6"
                        text "Import JSON"
                    }
                    textarea {
                        attr.``class`` "textarea"
                        attr.style "min-height: 18rem; font-family: monospace;"
                        bind.input.string model.importJson (fun value -> dispatch (SetImportJson value))
                    }
                }
            }
        }
    }

let renderTopNavigation activeTab dispatch =
    div {
        attr.``class`` "tabs is-toggle mb-5"
        ul {
            li {
                attr.``class`` (
                    if activeTab = GaiaProbeTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab GaiaProbeTab))
                    text "Inquiry Intake"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = DetailsTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab DetailsTab))
                    text "Details"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = DemoToolsTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab DemoToolsTab))
                    text "Demo Tools"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = DesignRealizationTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab DesignRealizationTab))
                    text "T6 Realization"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = FactsReconstructionTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab FactsReconstructionTab))
                    text "Inquiry Resolution"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = PersistenceTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab PersistenceTab))
                    text "Projects"
                }
            }

            li {
                attr.``class`` (
                    if activeTab = LedgerTab then
                        "is-active"
                    else
                        "")
                a {
                    on.click (fun _ -> dispatch (SelectTopNavigationTab LedgerTab))
                    text "Ledger"
                }
            }
        }
    }

let homePage model dispatch =
    match tryGetSelectedScenario model, model.scenarioResolution with
    | Some scenario, Some resolution ->
        let admissibility = getAdmissibilityResult scenario.Parse
        let matchedFrNames = mapIdsToNames (fun (fr: FR) -> fr.Id) (fun fr -> fr.Name) DemoData.demoSigma.FRs resolution.MatchedFRs
        let matchedDpNames = mapIdsToNames (fun (dp: DP) -> dp.Id) (fun dp -> dp.Name) DemoData.demoSigma.DPs resolution.MatchedDPs
        let matchedTfNames = mapIdsToNames (fun (tf: TF) -> tf.Id) (fun tf -> tf.Name) DemoData.demoSigma.TFs resolution.MatchedTFs
        let matchedCtqNames = mapIdsToNames (fun (ctq: CTQ) -> ctq.Id) (fun ctq -> ctq.Name) DemoData.demoSigma.CTQs resolution.MatchedCTQs
        let includedSequencedParsedPhis = getIncludedSequencedParsedPhis model.excludedPhiIds model.parsedPhis
        let currentSigmaContext = buildSigmaContextWithContextEntries model.phiContextEntries includedSequencedParsedPhis

        div {
            attr.``class`` "content"
            h1 {
                attr.``class`` "title"
                text "Cognopy Inquiry Console"
            }
            p {
                attr.``class`` "subtitle is-6"
                text "An inquiry resolution engine for adding stakeholder information and reconstructing answers from preserved reasoning history."
            }

            div {
                attr.``class`` "notification is-info is-light"
                text "Cognopy resolves stakeholder inquiries by translating them into structured reasoning, governing candidate changes, and reconstructing answers from preserved reasoning history."
            }

            div {
                attr.``class`` "columns is-variable is-4 mb-5"

                div {
                    attr.``class`` "column is-6"
                    div {
                        attr.``class`` "box"
                        p {
                            attr.``class`` "heading mb-2"
                            text "Tell Cognopy"
                        }
                        h2 {
                            attr.``class`` "title is-5"
                            text "Add information to the system"
                        }
                        p {
                            attr.``class`` "has-text-grey"
                            text "Use forward inquiry intake to capture stakeholder statements as Phi for the existing reasoning pipeline."
                        }
                        button {
                            attr.``class`` "button is-link is-light"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch (SelectTopNavigationTab GaiaProbeTab))
                            text "Tell Cognopy"
                        }
                    }
                }

                div {
                    attr.``class`` "column is-6"
                    div {
                        attr.``class`` "box"
                        p {
                            attr.``class`` "heading mb-2"
                            text "Ask Cognopy"
                        }
                        h2 {
                            attr.``class`` "title is-5"
                            text "Retrieve or explain information from the system"
                        }
                        p {
                            attr.``class`` "has-text-grey"
                            text "Use reverse inquiry resolution to answer questions from stored facts, decisions, provenance, and ledger history."
                        }
                        button {
                            attr.``class`` "button is-link"
                            attr.``type`` "button"
                            on.click (fun _ -> dispatch (SelectTopNavigationTab FactsReconstructionTab))
                            text "Ask Cognopy"
                        }
                    }
                }
            }

            renderTopNavigation model.activeTopNavigationTab dispatch

            match model.activeTopNavigationTab with
            | GaiaProbeTab -> div {
                attr.``class`` "mb-6 pb-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Live Inquiry Workflow"
                }

                p {
                    attr.``class`` "has-text-grey mb-4"
                    text "Inquiry is the user-facing layer. T1-T5 are the translation and reasoning machinery over Phi, candidates, governance, and ledger history."
                }

                p {
                    attr.``class`` "heading mb-2"
                    text "Reasoning pipeline"
                }

                div {
                    attr.``class`` "tags are-medium mb-5"
                    span {
                        attr.``class`` "tag is-link"
                        text "T1 Inquiry Intake"
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
                        text "Replay Control"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "Current Σ Snapshot"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T4 Candidate ΔΣ"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T5 Governance"
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
                                text "T1 - Inquiry Intake / Forward Inquiry"
                            }

                            p {
                                attr.``class`` "is-size-7 has-text-grey"
                                text "Forward inquiries are still captured as PhiIntake records and parsed by the existing T1-T5 machinery."
                            }

                            match model.phiDraftStatus with
                            | None -> empty()
                            | Some status ->
                                div {
                                    attr.``class`` "notification is-info is-light py-2"
                                    text status
                                }

                            div {
                                attr.``class`` "columns is-variable is-2 mb-0"

                                div {
                                    attr.``class`` "column is-5"
                                    div {
                                        attr.``class`` "field"
                                        label {
                                            attr.``class`` "label"
                                            text "Input class"
                                        }
                                        div {
                                            attr.``class`` "control"
                                            div {
                                                attr.``class`` "select is-fullwidth"
                                                select {
                                                    bind.input.string model.phiDraftInputClass (fun v -> dispatch (SetPhiDraftInputClass v))
                                                    option {
                                                        attr.value ""
                                                        text "Unclassified"
                                                    }
                                                    forEach phiInputClasses <| fun inputClass ->
                                                        option {
                                                            attr.value inputClass
                                                            text inputClass
                                                        }
                                                }
                                            }
                                        }
                                    }
                                }

                                div {
                                    attr.``class`` "column is-7"
                                    div {
                                        attr.``class`` "field"
                                        label {
                                            attr.``class`` "label"
                                            text "Actor"
                                        }
                                        div {
                                            attr.``class`` "control"
                                            input {
                                                attr.``class`` "input"
                                                attr.placeholder "Stakeholder, user, team..."
                                                bind.input.string model.phiDraftActor (fun v -> dispatch (SetPhiDraftActor v))
                                            }
                                        }
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Mission"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "What is the larger objective?"
                                        bind.input.string model.phiDraftMission (fun v -> dispatch (SetPhiDraftMission v))
                                    }
                                }
                            }

                            div {
                                attr.``class`` "field"
                                label {
                                    attr.``class`` "label"
                                    text "Operational context"
                                }
                                div {
                                    attr.``class`` "control"
                                    input {
                                        attr.``class`` "input"
                                        attr.placeholder "Where or when does this apply?"
                                        bind.input.string model.phiDraftOperationalContext (fun v -> dispatch (SetPhiDraftOperationalContext v))
                                    }
                                }
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
                                    text "Context entries / 1-second snip"
                                }
                                div {
                                    attr.``class`` "control"
                                    textarea {
                                        attr.``class`` "textarea"
                                        attr.placeholder "host=Tablet Module\ninterface=Display ↔ Base\nmode=Standby\nconstraint=Max 45 C\nassumption=Passive Cooling\nevidence=Test Report 17"
                                        bind.input.string model.phiContextSnipDraft (fun v -> dispatch (SetPhiContextSnipDraft v))
                                    }
                                }
                                p {
                                    attr.``class`` "help"
                                    text "Entries are stored as Phi context, with Provenance=OneSecSnip. Raw Phi text remains unchanged."
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
                                text "Ingest Forward Inquiry"
                            }
                        }

                        div {
                            attr.``class`` "box"
                            h2 {
                                attr.``class`` "title is-5"
                                text "Φ Set"
                            }

                            button {
                                attr.``class`` "button is-link is-light is-fullwidth mb-3"
                                attr.``type`` "button"
                                on.click (fun _ -> dispatch ParseAllIncludedPhi)
                                text "Parse All Included Φ"
                            }

                            match model.phiBatchParseStatus with
                            | None ->
                                empty()
                            | Some status ->
                                p {
                                    attr.``class`` "is-size-7 has-text-grey mb-3"
                                    text status
                                }

                            match model.ingestedPhis with
                            | [] ->
                                p {
                                    attr.``class`` "has-text-grey"
                                    text "No inquiries / Phi ingested yet."
                                }
                            | phis ->
                                div {
                                    attr.``class`` "content"
                                    forEach phis <| fun phi ->
                                        let inquiry = inquiryFromPhiIntake phi
                                        let intakeMetadata =
                                            [
                                                optionalMetadata "Class" phi.InputClass
                                                optionalMetadata "Actor" phi.Actor
                                                optionalMetadata "Mission" phi.Mission
                                                optionalMetadata "Operational context" phi.OperationalContext
                                                if String.IsNullOrWhiteSpace(phi.Source) then None else Some ("Source: " + phi.Source)
                                                if String.IsNullOrWhiteSpace(phi.Confidence) then None else Some ("Confidence: " + phi.Confidence)
                                            ]
                                            |> List.choose id
                                            |> String.concat " | "

                                        div {
                                            attr.``class`` "box"
                                            p {
                                                strong { text phi.PhiId }
                                            }
                                            div {
                                                attr.``class`` "tags mb-2"
                                                span {
                                                    attr.``class`` "tag is-link is-light"
                                                    text (formatInquiryMode inquiry.Mode)
                                                }
                                                span {
                                                    attr.``class`` "tag is-info is-light"
                                                    text (formatInquiryKind inquiry.Kind)
                                                }
                                            }
                                            p {
                                                text phi.RawStatement
                                            }
                                            if not (String.IsNullOrWhiteSpace(intakeMetadata)) then
                                                p {
                                                    attr.``class`` "is-size-7 has-text-grey"
                                                    text intakeMetadata
                                                }
                                            let contextEntries =
                                                model.phiContextEntries
                                                |> List.filter (fun entry -> entry.PhiId = phi.PhiId)

                                            let isInlineContextOpen = model.inlinePhiContextTargetId = Some phi.PhiId

                                            match contextEntries with
                                            | [] -> empty()
                                            | entries ->
                                                div {
                                                    attr.``class`` "tags mb-2"
                                                    forEach entries <| fun entry ->
                                                        span {
                                                            attr.``class`` "tag is-info is-light"
                                                            text (entry.Kind + ": " + entry.Value)
                                                        }
                                                }

                                            div {
                                                attr.``class`` "buttons are-small mb-2"

                                                button {
                                                    attr.``class`` (
                                                        if isInlineContextOpen then
                                                            "button is-small is-info"
                                                        else
                                                            "button is-small is-info is-light")
                                                    attr.``type`` "button"
                                                    on.click (fun _ -> dispatch (StartInlinePhiContextEntry phi.PhiId))
                                                    text "Add context/evidence"
                                                }

                                                button {
                                                    attr.``class`` "button is-small is-link is-light"
                                                    attr.``type`` "button"
                                                    on.click (fun _ -> dispatch (ParseIngestedPhi phi.PhiId))
                                                    text "Parse Φ"
                                                }
                                            }

                                            if isInlineContextOpen then
                                                div {
                                                    attr.``class`` "mt-2 p-3 has-background-light"

                                                    p {
                                                        attr.``class`` "is-size-7 has-text-grey mb-2"
                                                        text "Target: "
                                                        code { text phi.PhiId }
                                                    }

                                                    div {
                                                        attr.``class`` "field is-grouped is-grouped-multiline mb-2"

                                                        div {
                                                            attr.``class`` "control"
                                                            div {
                                                                attr.``class`` "select is-small"
                                                                select {
                                                                    bind.input.string model.phiContextEntryDraftKind (fun v -> dispatch (SetPhiContextEntryDraftKind v))
                                                                    forEach inlinePhiContextEntryKinds <| fun kind ->
                                                                        option {
                                                                            attr.value kind
                                                                            text kind
                                                                        }
                                                                }
                                                            }
                                                        }

                                                        div {
                                                            attr.``class`` "control is-expanded"
                                                            input {
                                                                attr.``class`` "input is-small"
                                                                attr.placeholder "Camera Shell, interface note, evidence ref..."
                                                                bind.input.string model.phiContextEntryDraftValue (fun v -> dispatch (SetPhiContextEntryDraftValue v))
                                                            }
                                                        }
                                                    }

                                                    div {
                                                        attr.``class`` "buttons are-small mb-0"

                                                        button {
                                                            attr.``class`` "button is-small is-info"
                                                            attr.``type`` "button"
                                                            on.click (fun _ -> dispatch (AddContextEntryToPhi phi.PhiId))
                                                            text "Add"
                                                        }

                                                        button {
                                                            attr.``class`` "button is-small is-light"
                                                            attr.``type`` "button"
                                                            on.click (fun _ -> dispatch CloseInlinePhiContextEntry)
                                                            text "Done"
                                                        }
                                                    }
                                                }
                                        }
                                }

                            if not (List.isEmpty model.ingestedPhis) then
                                hr {}

                                h3 {
                                    attr.``class`` "title is-6"
                                    text "Global Context Entry"
                                }

                                div {
                                    attr.``class`` "field"
                                    label {
                                        attr.``class`` "label is-size-7"
                                        text "Existing Φ"
                                    }
                                    div {
                                        attr.``class`` "control"
                                        div {
                                            attr.``class`` "select is-fullwidth is-small"
                                            select {
                                                bind.input.string model.existingPhiContextTargetId (fun v -> dispatch (SetExistingPhiContextTargetId v))
                                                option {
                                                    attr.value ""
                                                    text "Select Φ"
                                                }
                                                forEach model.ingestedPhis <| fun phi ->
                                                    option {
                                                        attr.value phi.PhiId
                                                        text phi.PhiId
                                                    }
                                            }
                                        }
                                    }
                                }

                                div {
                                    attr.``class`` "field"
                                    label {
                                        attr.``class`` "label is-size-7"
                                        text "Kind"
                                    }
                                    div {
                                        attr.``class`` "control"
                                        div {
                                            attr.``class`` "select is-fullwidth is-small"
                                            select {
                                                bind.input.string model.phiContextEntryDraftKind (fun v -> dispatch (SetPhiContextEntryDraftKind v))
                                                forEach phiContextEntryKinds <| fun kind ->
                                                    option { text kind }
                                            }
                                        }
                                    }
                                }

                                div {
                                    attr.``class`` "field"
                                    label {
                                        attr.``class`` "label is-size-7"
                                        text "Value"
                                    }
                                    div {
                                        attr.``class`` "control"
                                        input {
                                            attr.``class`` "input is-small"
                                            attr.placeholder "Tablet Module, Display ↔ Base, Max 45 C..."
                                            bind.input.string model.phiContextEntryDraftValue (fun v -> dispatch (SetPhiContextEntryDraftValue v))
                                        }
                                    }
                                }

                                button {
                                    attr.``class`` "button is-small is-info is-light is-fullwidth"
                                    attr.``type`` "button"
                                    on.click (fun _ -> dispatch AddContextEntryToExistingPhi)
                                    text "Add Context Entry"
                                }
                        }
                    }

                    div {
                        attr.``class`` "column is-8"

                        renderCurrentSigmaSnapshotPanel includedSequencedParsedPhis currentSigmaContext

                        renderOperationalSummaryTablesPanel
                            includedSequencedParsedPhis
                            currentSigmaContext
                            model.lastReplayAction
                            model.candidateDecisions
                            model.sigmaBasisItemDecisions
                            model.parseAmendmentDraft
                            model.parseAmendmentStatus
                            dispatch

                        renderCognitionReviewPanel model includedSequencedParsedPhis currentSigmaContext dispatch

                        renderParsedPhiLedgerPanel model.parsedPhis model.excludedPhiIds dispatch
                    }
                }
                }

            | DetailsTab -> div {
                attr.``class`` "mb-6 pb-5"

                h2 {
                    attr.``class`` "title is-4"
                    text "Reasoning Details"
                }

                p {
                    attr.``class`` "heading mb-2"
                    text "Translation machinery"
                }

                div {
                    attr.``class`` "tags are-medium mb-5"
                    span {
                        attr.``class`` "tag is-link"
                        text "T2 Parse"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "ΔΣ Analysis"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T3 Relevant Σ Context"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T4 Candidate ΔΣ"
                    }
                    span {
                        attr.``class`` "tag is-light"
                        text "->"
                    }
                    span {
                        attr.``class`` "tag is-link is-light"
                        text "T5 Governance"
                    }
                }

                renderParseDetailsPanel model.selectedPhiParse model.selectedPhiResolution

                renderDeltaSigmaAnalysisPanel model.lastReplayAction

                renderRelevantSigmaContextPanel includedSequencedParsedPhis model.selectedPhiParse model.selectedPhiResolution

                renderCandidateDeltaSigmaPanel
                    currentSigmaContext
                    model.candidateDecisions
                    model.sigmaBasisItemDecisions
                    includedSequencedParsedPhis
                    dispatch

                renderT5DecisionHistoryPanel model.candidateDecisions
                }

            | DemoToolsTab -> div {
                attr.``class`` "pt-2"

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

            | EvidenceTab ->
                renderEvidenceTab model dispatch

            | DesignRealizationTab ->
                renderT6RealizationTab model dispatch

            | FactsReconstructionTab ->
                renderFactsReconstructionTab model dispatch

            | PersistenceTab ->
                renderPersistenceTab model dispatch

            | LedgerTab ->
                renderLedgerTab model.LedgerEvents model.ReplayPreviewSequence dispatch
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
            menuItem model Probe "Inquiry Console"            
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

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) (update this.JSRuntime) view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
