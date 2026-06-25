module Gaia.Client.AppUpdate

open System
open Elmish
open Microsoft.JSInterop
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.Persistence
open Gaia.Client.Ledger
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.FactsReconstruction
open Gaia.Client.InquiryAnswer
open Gaia.Client.Realization

let projectFileModulePath = "./cognopy-files.js"

let cleanFormValue (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let normalizeProjectFileNamePart (value: string) =
    let source =
        if String.IsNullOrWhiteSpace(value) then
            "untitled-project"
        else
            value.Trim().ToLowerInvariant()

    let folder =
        source
        |> Seq.fold
            (fun (parts: string list, previousWasSeparator: bool) ch ->
                let isAsciiLetter = ch >= 'a' && ch <= 'z'
                let isDigit = ch >= '0' && ch <= '9'
                let isSeparator = ch = ' ' || ch = '-' || ch = '_'

                if isAsciiLetter || isDigit then
                    string ch :: parts, false
                elif isSeparator && not previousWasSeparator then
                    "-" :: parts, true
                else
                    parts, previousWasSeparator)
            ([], true)
        |> fst
        |> List.rev
        |> String.concat ""

    let normalized = folder.Trim('-')

    if String.IsNullOrWhiteSpace(normalized) then
        "untitled-project"
    else
        normalized

let createProjectFileName projectName =
    let safeProjectName = normalizeProjectFileNamePart projectName
    let timestamp = DateTime.UtcNow.ToString("yyyyMMdd-HHmmss")
    "cognopy-" + safeProjectName + "-" + timestamp + ".json"

let importProjectFileModule (jsRuntime: IJSRuntime) =
    jsRuntime.InvokeAsync<IJSObjectReference>("import", [| box projectFileModulePath |]).AsTask()

let saveJsonFileAsync (jsRuntime: IJSRuntime) filename content =
    task {
        let! fileModule = importProjectFileModule jsRuntime
        do! fileModule.InvokeVoidAsync("saveJsonFile", [| box filename; box content |]).AsTask()
        do! fileModule.DisposeAsync().AsTask()
    }

let openJsonFileAsync (jsRuntime: IJSRuntime) =
    task {
        let! fileModule = importProjectFileModule jsRuntime
        let! content = fileModule.InvokeAsync<string>("openJsonFile", Array.empty<obj>).AsTask()
        do! fileModule.DisposeAsync().AsTask()
        return content
    }

let saveJsonFileOperation (jsRuntime: IJSRuntime, filename: string, content: string) =
    async {
        do! saveJsonFileAsync jsRuntime filename content |> Async.AwaitTask
    }

let openJsonFileOperation (jsRuntime: IJSRuntime) =
    async {
        return! openJsonFileAsync jsRuntime |> Async.AwaitTask
    }

let saveProjectFileCmd jsRuntime filename content =
    Cmd.OfAsync.either
        saveJsonFileOperation
        (jsRuntime, filename, content)
        (fun () -> ProjectFileSaved filename)
        (fun ex -> ProjectFileSaveFailed ex.Message)

let openProjectFileCmd jsRuntime =
    Cmd.OfAsync.either
        openJsonFileOperation
        jsRuntime
        (fun content ->
            if isNull content then
                ProjectFileOpenCancelled
            else
                ProjectFileOpened content)
        (fun ex -> ProjectFileOpenFailed ex.Message)

let addContextEntryToPhi phiId (model: Model) =
    let value = model.phiContextEntryDraftValue.Trim()

    if String.IsNullOrWhiteSpace(phiId) || String.IsNullOrWhiteSpace(value) then
        model
    else
        let entry =
            createNextPhiContextEntry phiId model.phiContextEntryDraftKind value "Manual" model.phiContextEntries

        let contextModel =
            { model with
                existingPhiContextTargetId = phiId
                phiContextEntries = model.phiContextEntries @ [ entry ]
                phiContextEntryDraftValue = "" }

        let refreshedModel =
            if model.parsedPhis |> List.exists (fun parse -> parse.PhiId = phiId) then
                model.ingestedPhis
                |> List.tryFind (fun phi -> phi.PhiId = phiId)
                |> Option.map (fun phi -> parsePhiIntoModel phi contextModel |> fst)
                |> Option.defaultValue contextModel
            else
                contextModel

        refreshedModel
        |> appendPhiContextEntryLedgerEvent entry

let createInquiryResolvedLedgerEvent (result: FactsReconstructionResult) ledgerEvents =
    let answer =
        inquiryAnswerFromFactsReconstructionResult result
        |> profileInquiryAnswer

    let profile = inquiryIntentProfileForAnswer answer
    let maturity = answer.MaturityContext.MaturityStage
    let targetId =
        if String.IsNullOrWhiteSpace(result.TargetId) then
            "Auto-selected target"
        else
            result.TargetId

    let detail =
        [
            "Question: " + result.Question
            "Target kind: " + result.TargetKind
            "Target identifier: " + targetId
            "Answer profile: " + formatInquiryIntentProfile profile
            "Maturity stage: " + formatInquiryMaturityStage maturity
            "Answer summary: " + answer.Summary
        ]
        |> String.concat " | "

    createLedgerEvent
        inquiryResolvedLedgerEventKind
        targetId
        "Inquiry resolved"
        detail
        ledgerEvents

let update (jsRuntime: IJSRuntime) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | SelectTopNavigationTab tab ->
        { model with activeTopNavigationTab = tab }, Cmd.none
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
    | SelectReplayPreview sequenceNumber ->
        { model with ReplayPreviewSequence = Some sequenceNumber }, Cmd.none
    | ClearReplayPreview ->
        { model with ReplayPreviewSequence = None }, Cmd.none
    | SetFactsReconstructionQuestion value ->
        { model with
            factsReconstructionQuestion = value
            factsReconstructionTargetKind = suggestFactsTargetKind value
            factsReconstructionTargetId = ""
            factsReconstructionResult = None }, Cmd.none
    | SetFactsReconstructionTargetKind value ->
        { model with
            factsReconstructionTargetKind = value
            factsReconstructionTargetId = ""
            factsReconstructionResult = None }, Cmd.none
    | SetFactsReconstructionTargetId value ->
        { model with
            factsReconstructionTargetId = value
            factsReconstructionResult = None }, Cmd.none
    | SetFactsReconstructionDisplayMode value ->
        { model with factsReconstructionDisplayMode = value }, Cmd.none
    | RunFactsReconstruction ->
        let result = reconstructFacts model
        let inquiryResolvedEvent = createInquiryResolvedLedgerEvent result model.LedgerEvents

        { model with
            factsReconstructionResult = Some result
            LedgerEvents = model.LedgerEvents @ [ inquiryResolvedEvent ] }, Cmd.none
    | SetProjectName value ->
        { model with projectName = value }, Cmd.none
    | SaveProjectFile ->
        let snapshot = buildProjectSnapshot model
        let projectJson = serializeProjectSnapshot snapshot
        let filename = createProjectFileName snapshot.ProjectName

        model, saveProjectFileCmd jsRuntime filename projectJson
    | ProjectFileSaved filename ->
        { model with persistenceStatus = Some ("Project file saved: " + filename) }
        |> appendLedgerEvent
            "ProjectFileSaved"
            model.projectName
            "Project file saved"
            ("Downloaded project snapshot as " + filename + ".")
        |> fun updatedModel -> updatedModel, Cmd.none
    | ProjectFileSaveFailed message ->
        { model with persistenceStatus = Some ("Save failed: " + message) }, Cmd.none
    | OpenProjectFile ->
        model, openProjectFileCmd jsRuntime
    | ProjectFileOpened json ->
        match tryDeserializeProjectSnapshot json with
        | Ok snapshot ->
            model
            |> restoreProjectSnapshot snapshot
            |> fun restoredModel ->
                { restoredModel with
                    exportJson = ""
                    persistenceStatus = Some "Project file opened." }
            |> appendLedgerEvent
                "ProjectFileOpened"
                snapshot.ProjectName
                "Project file opened"
                ("Opened snapshot saved at " + snapshot.SavedAtUtc + ".")
            |> fun updatedModel -> updatedModel, Cmd.none
        | Result.Error message ->
            { model with persistenceStatus = Some ("Open failed: " + message) }, Cmd.none
    | ProjectFileOpenCancelled ->
        model, Cmd.none
    | ProjectFileOpenFailed message ->
        { model with persistenceStatus = Some ("Open failed: " + message) }, Cmd.none
    | ExportProjectJson ->
        let exportJson =
            model
            |> buildProjectSnapshot
            |> serializeProjectSnapshot

        { model with
            exportJson = exportJson
            persistenceStatus = Some "Project JSON exported." }
        |> appendLedgerEvent
            "ProjectExported"
            model.projectName
            "Project exported"
            "Project JSON exported from the Projects tab."
        |> fun updatedModel -> updatedModel, Cmd.none
    | SetImportJson value ->
        { model with importJson = value }, Cmd.none
    | ImportProjectJson ->
        match tryDeserializeProjectSnapshot model.importJson with
        | Ok snapshot ->
            model
            |> restoreProjectSnapshot snapshot
            |> fun restoredModel ->
                { restoredModel with
                    exportJson = ""
                    persistenceStatus = Some "Project JSON imported." }
            |> appendLedgerEvent
                "ProjectImported"
                snapshot.ProjectName
                "Project imported"
                ("Imported snapshot saved at " + snapshot.SavedAtUtc + ".")
            |> fun updatedModel -> updatedModel, Cmd.none
        | Result.Error message ->
            { model with persistenceStatus = Some ("Import failed: " + message) }, Cmd.none
    | LoadSphynxSampleProject ->
        model
        |> restoreProjectSnapshot (buildSphynxSampleSnapshot ())
        |> fun sampleModel ->
            { sampleModel with
                exportJson = ""
                importJson = ""
                persistenceStatus = Some "Sphynx sample project loaded." }
        |> appendLedgerEvent
            "ProjectSampleLoaded"
            "SphynxSampleProject"
            "Sphynx sample project loaded"
            "Loaded the built-in Sphynx demo Phi set."
        |> fun updatedModel -> updatedModel, Cmd.none
    | ClearProject ->
        { clearProjectModel model with persistenceStatus = Some "Project cleared." }, Cmd.none
    | SetEvidenceCaptureKind value ->
        { model with evidenceCaptureKind = value }, Cmd.none
    | SetEvidenceTargetKind value ->
        { model with
            evidenceTargetKind = value
            evidenceTargetId = "" }, Cmd.none
    | SetEvidenceTargetId value ->
        { model with evidenceTargetId = value }, Cmd.none
    | SetEvidenceTitle value ->
        { model with evidenceTitle = value }, Cmd.none
    | SetEvidenceNotes value ->
        { model with evidenceNotes = value }, Cmd.none
    | SetEvidenceContentRef value ->
        { model with evidenceContentRef = value }, Cmd.none
    | CreateEvidenceRecord ->
        let title = model.evidenceTitle.Trim()

        if String.IsNullOrWhiteSpace(title) then
            { model with evidenceStatus = Some "Evidence title is required." }, Cmd.none
        elif String.IsNullOrWhiteSpace(model.evidenceTargetKind) || String.IsNullOrWhiteSpace(model.evidenceTargetId) then
            { model with evidenceStatus = Some "Select an evidence target." }, Cmd.none
        else
            match tryResolveEvidenceTargetLabel model with
            | None ->
                { model with evidenceStatus = Some "Select a valid evidence target." }, Cmd.none
            | Some targetLabel ->
                let evidenceId = createEvidenceId model.evidenceRecords

                let evidenceRecord =
                    {
                        EvidenceId = evidenceId
                        TimestampUtc = getUtcTimestampString ()
                        Actor = "Demo user"
                        CaptureKind = model.evidenceCaptureKind
                        TargetKind = model.evidenceTargetKind
                        TargetId = model.evidenceTargetId
                        TargetLabel = targetLabel
                        Title = title
                        Notes = model.evidenceNotes
                        ContentRef = model.evidenceContentRef
                    }

                let detail =
                    model.evidenceCaptureKind
                    + " | "
                    + title
                    + " | "
                    + model.evidenceTargetKind
                    + " | "
                    + targetLabel

                { model with
                    evidenceRecords = model.evidenceRecords @ [ evidenceRecord ]
                    evidenceTitle = ""
                    evidenceNotes = ""
                    evidenceContentRef = ""
                    evidenceStatus = Some ("Evidence captured: " + evidenceId) }
                |> appendLedgerEvent "EvidenceCaptured" evidenceId "Evidence captured" detail
                |> fun updatedModel -> updatedModel, Cmd.none
    | SetRealizationObjectKindDraft value ->
        { model with realizationObjectKindDraft = value }, Cmd.none
    | SetRealizationObjectIdDraft value ->
        { model with realizationObjectIdDraft = value }, Cmd.none
    | SetRealizationObjectNameDraft value ->
        { model with realizationObjectNameDraft = value }, Cmd.none
    | SetRealizationObjectDescriptionDraft value ->
        { model with realizationObjectDescriptionDraft = value }, Cmd.none
    | SetRealizationObjectSourceNoteDraft value ->
        { model with realizationObjectSourceNoteDraft = value }, Cmd.none
    | CreateRealizationObject ->
        let objectKind = cleanFormValue model.realizationObjectKindDraft
        let objectId = cleanFormValue model.realizationObjectIdDraft
        let objectName = cleanFormValue model.realizationObjectNameDraft
        let description = cleanFormValue model.realizationObjectDescriptionDraft
        let sourceNote = cleanFormValue model.realizationObjectSourceNoteDraft

        match tryAddRealizationObject objectKind objectId objectName description sourceNote model.realizationState with
        | Result.Error message ->
            { model with realizationStatus = Some message }, Cmd.none
        | Result.Ok realizationState ->
            let detail =
                [
                    "Kind: " + objectKind
                    "Id: " + objectId
                    "Name: " + objectName
                    if not (String.IsNullOrWhiteSpace(description)) then
                        "Description: " + description
                    if not (String.IsNullOrWhiteSpace(sourceNote)) then
                        "Source/note: " + sourceNote
                ]
                |> String.concat "; "

            { model with
                realizationState = realizationState
                realizationObjectIdDraft = ""
                realizationObjectNameDraft = ""
                realizationObjectDescriptionDraft = ""
                realizationObjectSourceNoteDraft = ""
                realizationStatus = Some ("Created " + objectKind + " " + objectId + ".") }
            |> appendLedgerEvent "RealizationObjectCreated" objectId "Realization object created" detail
            |> fun updatedModel -> updatedModel, Cmd.none
    | SetRealizationLinkKindDraft value ->
        { model with
            realizationLinkKindDraft = value
            realizationLinkSourceIdDraft = ""
            realizationLinkTargetIdDraft = "" }, Cmd.none
    | SetRealizationLinkSourceIdDraft value ->
        { model with realizationLinkSourceIdDraft = value }, Cmd.none
    | SetRealizationLinkTargetIdDraft value ->
        { model with realizationLinkTargetIdDraft = value }, Cmd.none
    | CreateRealizationLink ->
        let linkKind = cleanFormValue model.realizationLinkKindDraft
        let sourceId = cleanFormValue model.realizationLinkSourceIdDraft
        let targetId = cleanFormValue model.realizationLinkTargetIdDraft

        match tryAddRealizationLink linkKind sourceId targetId model with
        | Result.Error message ->
            { model with realizationStatus = Some message }, Cmd.none
        | Result.Ok realizationState ->
            let linkId = sourceId + " -> " + targetId
            let detail = "Kind: " + linkKind + "; Source: " + sourceId + "; Target: " + targetId

            { model with
                realizationState = realizationState
                realizationLinkSourceIdDraft = ""
                realizationLinkTargetIdDraft = ""
                realizationStatus = Some ("Linked " + linkId + ".") }
            |> appendLedgerEvent "RealizationLinkCreated" linkId "Realization link created" detail
            |> fun updatedModel -> updatedModel, Cmd.none
    | SetRealizationNavigationOperator value ->
        { model with realizationNavigationOperator = value }, Cmd.none
    | SetRealizationNavigationTarget value ->
        { model with realizationNavigationTarget = value }, Cmd.none
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

    | SetPhiContextSnipDraft value ->
        { model with phiContextSnipDraft = value }, Cmd.none

    | StartInlinePhiContextEntry phiId ->
        { model with
            inlinePhiContextTargetId = Some phiId
            existingPhiContextTargetId = phiId
            phiContextEntryDraftKind = defaultPhiContextEntryKind
            phiContextEntryDraftValue = "" },
        Cmd.none

    | CloseInlinePhiContextEntry ->
        { model with inlinePhiContextTargetId = None }, Cmd.none

    | SetExistingPhiContextTargetId value ->
        { model with existingPhiContextTargetId = value }, Cmd.none

    | SetPhiContextEntryDraftKind value ->
        { model with phiContextEntryDraftKind = value }, Cmd.none

    | SetPhiContextEntryDraftValue value ->
        { model with phiContextEntryDraftValue = value }, Cmd.none

    | AddContextEntryToExistingPhi ->
        addContextEntryToPhi model.existingPhiContextTargetId model, Cmd.none

    | AddContextEntryToPhi phiId ->
        addContextEntryToPhi phiId model, Cmd.none

    | ParseIngestedPhi phiId ->
        match model.ingestedPhis |> List.tryFind (fun phi -> phi.PhiId = phiId) with
        | Some phi ->
            if model.parsedPhis |> List.exists (fun parse -> parse.PhiId = phiId) then
                { model with phiBatchParseStatus = None }
                |> appendLedgerEvent
                    "PhiParseIgnoredAlreadyParsed"
                    phi.PhiId
                    "Phi parse ignored; already parsed"
                    phi.RawStatement
                |> fun updatedModel -> updatedModel, Cmd.none
            else
                let parsedModel, parse = parsePhiIntoModel phi model

                parsedModel
                |> appendLedgerEvent "PhiParsed" parse.PhiId "Phi parsed" parse.Statement
                |> fun updatedModel -> updatedModel, Cmd.none

        | None ->
            model, Cmd.none

    | ParseAllIncludedPhi ->
        if List.isEmpty model.ingestedPhis then
            { model with phiBatchParseStatus = Some "No Phi available to parse." }, Cmd.none
        else
            let parsedModel, counts = parseAllIncludedPhis model
            let status = formatPhiBatchParseStatus (List.length model.ingestedPhis) counts
            let detail = formatPhiBatchParseDetail counts

            { parsedModel with phiBatchParseStatus = Some status }
            |> appendLedgerEvent
                "PhiBatchParsed"
                (getPhiBatchTargetId model.projectName)
                "Parsed all included Phi"
                detail
            |> fun updatedModel -> updatedModel, Cmd.none

    | ToggleExcludeParsedPhi phiId ->
        let beforeSigma =
            model.parsedPhis
            |> getIncludedSequencedParsedPhis model.excludedPhiIds
            |> buildSigmaContextWithContextEntries model.phiContextEntries

        let wasExcluded = model.excludedPhiIds |> List.contains phiId

        let excludedPhiIds =
            if wasExcluded then
                model.excludedPhiIds
                |> List.filter (fun excludedPhiId -> excludedPhiId <> phiId)
            else
                phiId :: model.excludedPhiIds

        let afterSigma =
            model.parsedPhis
            |> getIncludedSequencedParsedPhis excludedPhiIds
            |> buildSigmaContextWithContextEntries model.phiContextEntries

        let lastReplayAction =
            buildReplayDeltaSigmaAnalysis phiId wasExcluded beforeSigma afterSigma model.parsedPhis

        let eventKind, summary =
            if wasExcluded then
                "PhiIncludedInReplay", "Phi included in replay"
            else
                "PhiExcludedFromReplay", "Phi excluded from replay"

        let detail =
            model.parsedPhis
            |> List.tryFind (fun parse -> parse.PhiId = phiId)
            |> Option.map (fun parse -> parse.Statement)
            |> Option.defaultValue "Source statement unavailable."

        { model with
            excludedPhiIds = excludedPhiIds
            lastReplayAction = Some lastReplayAction
            phiBatchParseStatus = None }
        |> appendLedgerEvent eventKind phiId summary detail
        |> fun updatedModel -> updatedModel, Cmd.none

    | SetCognitionReviewTargetFilter value ->
        { model with cognitionReviewTargetFilter = value }, Cmd.none

    | SetCognitionReviewDecisionFilter value ->
        { model with cognitionReviewDecisionFilter = value }, Cmd.none

    | SetCognitionReviewTextFilter value ->
        { model with cognitionReviewTextFilter = value }, Cmd.none

    | AcceptCandidate candidateId ->
        decideCandidate candidateId Accepted model

    | RejectCandidate candidateId ->
        decideCandidate candidateId Rejected model

    | HoldCandidate candidateId ->
        decideCandidate candidateId Held model

    | SetSigmaBasisItemDecision (basisItemKey, decision) ->
        let updatedModel =
            { model with sigmaBasisItemDecisions = model.sigmaBasisItemDecisions |> Map.add basisItemKey decision }

        match tryFindCurrentSigmaBasisItemLedgerContext basisItemKey model with
        | Some context ->
            updatedModel
            |> appendSigmaBasisItemDecisionLedgerEvent "Individual" decision context
            |> fun ledgerModel -> ledgerModel, Cmd.none
        | None ->
            updatedModel, Cmd.none

    | SetSigmaBasisItemDecisions (basisItemKeys, decision) ->
        let sigmaBasisItemDecisions =
            basisItemKeys
            |> List.fold (fun decisions basisItemKey -> decisions |> Map.add basisItemKey decision) model.sigmaBasisItemDecisions

        let updatedModel =
            { model with sigmaBasisItemDecisions = sigmaBasisItemDecisions }

        let ledgerModel =
            basisItemKeys
            |> List.choose (fun basisItemKey -> tryFindCurrentSigmaBasisItemLedgerContext basisItemKey model)
            |> List.fold
                (fun workingModel context ->
                    workingModel
                    |> appendSigmaBasisItemDecisionLedgerEvent "Bulk" decision context)
                updatedModel

        ledgerModel, Cmd.none

    | IngestPhiDraft ->
        let timestamp = DateTime.UtcNow
        let phiId = "PHI-" + timestamp.ToString("yyyyMMdd-HHmmss")
        let contextEntries =
            parsePhiContextSnipLines phiId 1 "OneSecSnip" model.phiContextSnipDraft

        let intake =
            {
                PhiId = phiId
                Date = timestamp.ToString("yyyy-MM-dd")
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
                TypeText = model.phiDraftQuickTags
                Impact = ""
                UnresolvedSignal = ""
            }

        { model with
            ingestedPhis = intake :: model.ingestedPhis
            phiDraftRawStatement = ""
            phiDraftTriggerContext = ""
            phiDraftSource = ""
            phiDraftQuickTags = ""
            phiDraftConfidence = "Medium"
            phiContextSnipDraft = ""
            phiContextEntries = model.phiContextEntries @ contextEntries
            phiBatchParseStatus = None }
        |> appendLedgerEvent "PhiIngested" intake.PhiId "Phi ingested" intake.RawStatement
        |> appendPhiContextEntryLedgerEvents contextEntries
        |> fun updatedModel -> updatedModel, Cmd.none

