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

let optionalFormValue value =
    let cleaned = cleanFormValue value

    if String.IsNullOrWhiteSpace(cleaned) then
        None
    else
        Some cleaned

let isValidParsedExposureAtomKind value =
    parsedExposureAtomKinds
    |> List.exists (fun atomKind -> String.Equals(atomKind, value, StringComparison.Ordinal))

let formatAffectedParseAmendmentGroups oldKind newKind =
    [ oldKind; newKind ]
    |> List.filter (fun value -> not (String.IsNullOrWhiteSpace(value)))
    |> List.distinct
    |> String.concat ", "

let atomTextEquals left right =
    String.Equals(cleanFormValue left, cleanFormValue right, StringComparison.OrdinalIgnoreCase)

let atomListContains atom atoms =
    atoms
    |> List.exists (fun existing -> atomTextEquals existing atom)

let tryValidateParseAmendment (draft: ParseAmendmentDraft) (model: Model) =
    let proposedKind = cleanFormValue draft.ProposedAtomKind
    let proposedText = cleanFormValue draft.ProposedAtomText
    let reason = cleanFormValue draft.Reason

    if not (isValidParsedExposureAtomKind proposedKind) then
        Result.Error "Select a valid proposed atom kind."
    elif String.IsNullOrWhiteSpace(proposedText) then
        Result.Error "Proposed atom text is required."
    else
        match model.parsedPhis |> List.tryFind (fun parse -> parse.PhiId = draft.SourcePhiId) with
        | None ->
            Result.Error "The source Phi parse is no longer available."
        | Some parse ->
            let currentOriginalAtoms =
                getExposureAtomValue draft.OriginalAtomKind parse
                |> splitExposureAtomValues

            if not (atomListContains draft.OriginalAtomText currentOriginalAtoms) then
                Result.Error "This parsed atom has changed since the amendment draft was opened. Start the amendment again from the current row."
            else
                let currentTargetAtoms =
                    getExposureAtomValue proposedKind parse
                    |> splitExposureAtomValues

                if proposedKind <> draft.OriginalAtomKind && atomListContains proposedText currentTargetAtoms then
                    Result.Error
                        ("The target "
                         + proposedKind
                         + " slot already contains that atom. Amend that atom directly before moving this one.")
                else
                    Result.Ok
                        { draft with
                            ProposedAtomKind = proposedKind
                            ProposedAtomText = proposedText
                            Reason = reason }

let applyParseAmendment (draft: ParseAmendmentDraft) (parse: PhiParse) =
    applyParseAmendmentToPhiParse draft parse

let formatParseAmendmentLedgerDetail (draft: ParseAmendmentDraft) =
    [
        "Source Phi ID: " + draft.SourcePhiId
        "Original kind: " + draft.OriginalAtomKind
        "Original text: " + draft.OriginalAtomText
        "New kind: " + draft.ProposedAtomKind
        "New text: " + draft.ProposedAtomText
        "Original provenance: " + draft.Provenance
        "Reason: " + (if String.IsNullOrWhiteSpace(draft.Reason) then "(none supplied)" else draft.Reason)
        "Affected candidate groups require review: " + formatAffectedParseAmendmentGroups draft.OriginalAtomKind draft.ProposedAtomKind
    ]
    |> String.concat " | "

let formatParseAmendmentDecisionResetLedgerDetail (draft: ParseAmendmentDraft) (resetImpact: ParseAmendmentBasisDecisionResetImpact) =
    [
        "Source Phi ID: " + draft.SourcePhiId
        "Original kind: " + draft.OriginalAtomKind
        "Original text: " + draft.OriginalAtomText
        "New kind: " + draft.ProposedAtomKind
        "New text: " + draft.ProposedAtomText
        "Candidate ID: " + resetImpact.CandidateId
        "Candidate type: " + resetImpact.CandidateType
        "Candidate target: " + resetImpact.CandidateTarget
        "Basis item key: " + resetImpact.BasisItemKey
        "Previous decision: " + formatSigmaBasisItemDecisionValue resetImpact.PreviousDecision
        "Reason: Parse amendment changed or moved the interpreted atom that this basis-item decision governed."
        "Amendment reason: " + (if String.IsNullOrWhiteSpace(draft.Reason) then "(none supplied)" else draft.Reason)
    ]
    |> String.concat " | "

let appendParseAmendmentDecisionResetLedgerEvents draft resetImpacts model =
    resetImpacts
    |> List.fold
        (fun workingModel resetImpact ->
            workingModel
            |> appendLedgerEvent
                sigmaBasisItemDecisionResetLedgerKind
                resetImpact.BasisItemKey
                "Sigma basis-item decision reset"
                (formatParseAmendmentDecisionResetLedgerDetail draft resetImpact))
        model

let formatParsedAtomRetirementLedgerDetail sourcePhiId atomKind atomText provenance basisResetCount candidateResetCount =
    [
        "Source Phi ID: " + sourcePhiId
        "Atom kind: " + atomKind
        "Atom label: " + formatModelFittingAtomKindLabel atomKind
        "Atom text: " + atomText
        "Provenance: " + provenance
        "Basis decision resets: " + string basisResetCount
        "Candidate decision resets: " + string candidateResetCount
        "Reason: Parsed atom retired from active Model Fitting; original Phi remains unchanged."
    ]
    |> String.concat " | "

let formatParsedAtomRetirementDecisionResetLedgerDetail
    sourcePhiId
    atomKind
    atomText
    (resetImpact: ParseAmendmentBasisDecisionResetImpact) =
    [
        "Source Phi ID: " + sourcePhiId
        "Retired kind: " + atomKind
        "Retired text: " + atomText
        "Candidate ID: " + resetImpact.CandidateId
        "Candidate type: " + resetImpact.CandidateType
        "Candidate target: " + resetImpact.CandidateTarget
        "Basis item key: " + resetImpact.BasisItemKey
        "Previous decision: " + formatSigmaBasisItemDecisionValue resetImpact.PreviousDecision
        "Reason: Parsed atom retirement removed this interpreted atom from active Model Fitting."
    ]
    |> String.concat " | "

let appendParsedAtomRetirementDecisionResetLedgerEvents sourcePhiId atomKind atomText resetImpacts model =
    resetImpacts
    |> List.fold
        (fun workingModel resetImpact ->
            workingModel
            |> appendLedgerEvent
                sigmaBasisItemDecisionResetLedgerKind
                resetImpact.BasisItemKey
                "Sigma basis-item decision reset"
                (formatParsedAtomRetirementDecisionResetLedgerDetail sourcePhiId atomKind atomText resetImpact))
        model

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

let private isPhiTargetKind (targetKind: string) =
    String.Equals(cleanFormValue targetKind, "Phi", StringComparison.OrdinalIgnoreCase)

let private hasParsedPhi phiId (model: Model) =
    model.parsedPhis
    |> List.exists (fun parse -> parse.PhiId = phiId)

let private markPhiParseStaleIfParsed phiId statusMessage (model: Model) =
    if hasParsedPhi phiId model then
        let staleParsedPhiIds =
            if model.staleParsedPhiIds |> List.contains phiId then
                model.staleParsedPhiIds
            else
                model.staleParsedPhiIds @ [ phiId ]

        { model with
            staleParsedPhiIds = staleParsedPhiIds
            phiBatchParseStatus = Some statusMessage }
    else
        model

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
                phiContextEntryDraftValue = ""
                phiBatchParseStatus = model.phiBatchParseStatus }
            |> markPhiParseStaleIfParsed phiId ("Parse marked stale for " + phiId + ". Recompute parse to apply new Phi context.")

        contextModel
        |> appendPhiContextEntryLedgerEvent entry

let private containsDraftMarker (marker: string) (value: string) =
    not (String.IsNullOrWhiteSpace(value))
    && value.IndexOf(marker, StringComparison.OrdinalIgnoreCase) >= 0

let private isDerivedInquiryDraft (source: string) (quickTags: string) =
    String.Equals(source, t6RealizationInquirySource, StringComparison.OrdinalIgnoreCase)
    || containsDraftMarker derivedInquiryTag quickTags

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

                let evidenceModel =
                    { model with
                        evidenceRecords = model.evidenceRecords @ [ evidenceRecord ]
                        evidenceTitle = ""
                        evidenceNotes = ""
                        evidenceContentRef = ""
                        evidenceStatus = Some ("Evidence reference captured: " + evidenceId) }

                let contextAwareEvidenceModel =
                    if isPhiTargetKind evidenceRecord.TargetKind then
                        let staleStatus =
                            "Parse marked stale for "
                            + evidenceRecord.TargetId
                            + ". Recompute parse to apply new Phi-targeted 1sec Snip."

                        evidenceModel
                        |> markPhiParseStaleIfParsed evidenceRecord.TargetId staleStatus
                    else
                        evidenceModel

                contextAwareEvidenceModel
                |> appendLedgerEvent "EvidenceReferenceCreated" evidenceId "Evidence reference created" detail
                |> fun updatedModel -> updatedModel, Cmd.none
    | CreateEvidenceForParsedAtom (atomKind, atomText, sourcePhiId) ->
        let cleanedAtomKind = cleanFormValue atomKind
        let cleanedAtomText = cleanFormValue atomText
        let cleanedSourcePhiId = cleanFormValue sourcePhiId

        if not (isValidParsedExposureAtomKind cleanedAtomKind) || String.IsNullOrWhiteSpace(cleanedAtomText) then
            { model with evidenceStatus = Some "Select a valid Model Fitting item before adding note / evidence." }, Cmd.none
        else
            let evidenceId = createEvidenceId model.evidenceRecords
            let atomLabel = formatModelFittingAtomKindLabel cleanedAtomKind
            let title =
                match cleanFormValue model.evidenceTitle with
                | "" -> "Note for " + atomLabel
                | value -> value

            let captureKind =
                match cleanFormValue model.evidenceCaptureKind with
                | "" -> defaultEvidenceCaptureKind
                | value -> value

            let targetLabel =
                cleanedAtomText
                + (if String.IsNullOrWhiteSpace(cleanedSourcePhiId) then
                       ""
                   else
                       " (source " + cleanedSourcePhiId + ")")

            let evidenceRecord =
                {
                    EvidenceId = evidenceId
                    TimestampUtc = getUtcTimestampString ()
                    Actor = "Demo user"
                    CaptureKind = captureKind
                    TargetKind = cleanedAtomKind
                    TargetId = cleanedAtomText
                    TargetLabel = targetLabel
                    Title = title
                    Notes = model.evidenceNotes
                    ContentRef = model.evidenceContentRef
                }

            let detail =
                captureKind
                + " | "
                + title
                + " | "
                + cleanedAtomKind
                + " | "
                + targetLabel

            { model with
                evidenceRecords = model.evidenceRecords @ [ evidenceRecord ]
                evidenceTargetKind = cleanedAtomKind
                evidenceTargetId = cleanedAtomText
                evidenceTitle = ""
                evidenceNotes = ""
                evidenceContentRef = ""
                evidenceStatus = Some ("Evidence reference captured: " + evidenceId) }
            |> appendLedgerEvent "EvidenceReferenceCreated" evidenceId "Evidence reference created" detail
            |> fun updatedModel -> updatedModel, Cmd.none
    | CreatePhiFromParsedAtom (atomKind, atomText, sourcePhiId) ->
        let cleanedAtomKind = cleanFormValue atomKind
        let cleanedAtomText = cleanFormValue atomText
        let cleanedSourcePhiId = cleanFormValue sourcePhiId

        if not (isValidParsedExposureAtomKind cleanedAtomKind) || String.IsNullOrWhiteSpace(cleanedAtomText) then
            { model with phiDraftStatus = Some "Select a valid Model Fitting item before creating Phi." }, Cmd.none
        else
            let timestamp = DateTime.UtcNow
            let phiId =
                "PHI-"
                + timestamp.ToString("yyyyMMdd-HHmmss")
                + "-MF"
                + sprintf "%03d" (List.length model.ingestedPhis + 1)

            let atomLabel = formatModelFittingAtomKindLabel cleanedAtomKind

            let sourceStatement =
                model.parsedPhis
                |> List.tryFind (fun parse -> parse.PhiId = cleanedSourcePhiId)
                |> Option.map (fun parse -> parse.Statement)
                |> Option.defaultValue ""

            let generatedStatement =
                "Model fitting "
                + atomLabel.ToLowerInvariant()
                + ": "
                + cleanedAtomText
                + "."

            let context =
                [
                    "Created from Model Fitting item."
                    if not (String.IsNullOrWhiteSpace(cleanedSourcePhiId)) then
                        "Source Phi: " + cleanedSourcePhiId + "."
                    if not (String.IsNullOrWhiteSpace(sourceStatement)) then
                        "Source statement: " + sourceStatement
                ]
                |> String.concat " "

            let intake =
                {
                    PhiId = phiId
                    Date = timestamp.ToString("yyyy-MM-dd")
                    InputClass = Some "Model Fitting"
                    Actor = Some "Demo user"
                    Mission = None
                    OperationalContext = None
                    Source = "Model Fitting"
                    Context = context
                    Confidence = "Medium"
                    Status = "Ingested"
                    RawStatement = generatedStatement
                    Trigger = context
                    Claim = cleanedAtomText
                    About = atomLabel
                    Condition = ""
                    Assumption = ""
                    TypeText =
                        "model-fitting-derived; "
                        + cleanedAtomKind
                        + "; source-phi="
                        + cleanedSourcePhiId
                    Impact = ""
                    UnresolvedSignal = ""
                }

            { model with
                ingestedPhis = intake :: model.ingestedPhis
                phiDraftStatus = Some ("Phi created from Model Fitting item: " + intake.PhiId)
                phiBatchParseStatus = None }
            |> appendLedgerEvent "PhiIngested" intake.PhiId "Phi ingested" intake.RawStatement
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
    | SetRealizationInquiryQuestion value ->
        { model with realizationInquiryQuestion = value }, Cmd.none
    | PrefillPhiDraft draft ->
        { model with
            activeTopNavigationTab = GaiaProbeTab
            phiDraftStatus = Some draft.StatusMessage
            phiDraftInputClass = defaultPhiDraftInputClass
            phiDraftActor = ""
            phiDraftMission = ""
            phiDraftOperationalContext = ""
            phiDraftRawStatement = draft.RawStatement
            phiDraftTriggerContext = draft.TriggerContext
            phiDraftSource = draft.Source
            phiDraftQuickTags = draft.QuickTags
            phiDraftConfidence = draft.Confidence
            phiContextSnipDraft = draft.ContextSnip },
        Cmd.none
    | SetPhiDraftInputClass value ->
        { model with phiDraftInputClass = value }, Cmd.none

    | SetPhiDraftActor value ->
        { model with phiDraftActor = value }, Cmd.none

    | SetPhiDraftMission value ->
        { model with phiDraftMission = value }, Cmd.none

    | SetPhiDraftOperationalContext value ->
        { model with phiDraftOperationalContext = value }, Cmd.none

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
            let hasExistingParse =
                model.parsedPhis
                |> List.exists (fun parse -> parse.PhiId = phiId)

            let isStale =
                isPhiParseStale model.staleParsedPhiIds phiId

            if hasExistingParse && isStale then
                let parsedModel, parse = parsePhiIntoModel phi model

                parsedModel
                |> appendLedgerEvent
                    "PhiParseRecomputed"
                    parse.PhiId
                    "Phi parse recomputed"
                    ("Recomputed from original Phi and current Phi context entries. Statement: " + parse.Statement)
                |> fun updatedModel -> updatedModel, Cmd.none
            elif hasExistingParse then
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

    | StartParseAmendment (phiId, atomKind, atomText, provenance) ->
        match model.parsedPhis |> List.tryFind (fun parse -> parse.PhiId = phiId) with
        | None ->
            { model with parseAmendmentStatus = Some "The selected parsed Phi is no longer available." }, Cmd.none
        | Some parse ->
            let currentAtomAtoms =
                getExposureAtomValue atomKind parse
                |> splitExposureAtomValues

            let originalAtomText =
                if atomListContains atomText currentAtomAtoms then
                    atomText
                elif List.length currentAtomAtoms = 1 then
                    currentAtomAtoms |> List.head
                else
                    atomText

            { model with
                parseAmendmentDraft =
                    Some
                        {
                            SourcePhiId = phiId
                            OriginalAtomKind = atomKind
                            OriginalAtomText = originalAtomText
                            SourcePhiStatement = parse.Statement
                            Provenance = provenance
                            ProposedAtomKind = atomKind
                            ProposedAtomText = originalAtomText
                            Reason = ""
                            PreviewRequested = false
                        }
                parseAmendmentStatus = None
                selectedParsedAtomReviewKind = Some atomKind },
            Cmd.none

    | SetParseAmendmentProposedKind value ->
        { model with
            parseAmendmentDraft =
                model.parseAmendmentDraft
                |> Option.map (fun draft ->
                    { draft with
                        ProposedAtomKind = value
                        PreviewRequested = false })
            parseAmendmentStatus = None },
        Cmd.none

    | SetParseAmendmentProposedText value ->
        { model with
            parseAmendmentDraft =
                model.parseAmendmentDraft
                |> Option.map (fun draft ->
                    { draft with
                        ProposedAtomText = value
                        PreviewRequested = false })
            parseAmendmentStatus = None },
        Cmd.none

    | SetParseAmendmentReason value ->
        { model with
            parseAmendmentDraft =
                model.parseAmendmentDraft
                |> Option.map (fun draft ->
                    { draft with
                        Reason = value
                        PreviewRequested = false })
            parseAmendmentStatus = None },
        Cmd.none

    | PreviewParseAmendment ->
        match model.parseAmendmentDraft with
        | None ->
            { model with parseAmendmentStatus = Some "Select a parsed atom to amend." }, Cmd.none
        | Some draft ->
            match tryValidateParseAmendment draft model with
            | Result.Error message ->
                { model with parseAmendmentStatus = Some message }, Cmd.none
            | Result.Ok validatedDraft ->
                { model with
                    parseAmendmentDraft = Some { validatedDraft with PreviewRequested = true }
                    parseAmendmentStatus = Some "Preview ready. Confirm to apply this amendment to the current parse." },
                Cmd.none

    | ConfirmParseAmendment ->
        match model.parseAmendmentDraft with
        | None ->
            { model with parseAmendmentStatus = Some "Select a parsed atom to amend." }, Cmd.none
        | Some draft when not draft.PreviewRequested ->
            match tryValidateParseAmendment draft model with
            | Result.Error message ->
                { model with parseAmendmentStatus = Some message }, Cmd.none
            | Result.Ok validatedDraft ->
                { model with
                    parseAmendmentDraft = Some { validatedDraft with PreviewRequested = true }
                    parseAmendmentStatus = Some "Preview ready. Confirm to apply this amendment to the current parse." },
                Cmd.none
        | Some draft ->
            match tryValidateParseAmendment draft model with
            | Result.Error message ->
                { model with parseAmendmentStatus = Some message }, Cmd.none
            | Result.Ok validatedDraft ->
                let impactPreview =
                    model.parsedPhis
                    |> getIncludedSequencedParsedPhis model.excludedPhiIds
                    |> applyParsedAtomRetirementsToSequencedPhis model.LedgerEvents
                    |> buildParseAmendmentImpactPreview validatedDraft model.candidateDecisions model.sigmaBasisItemDecisions

                let amendedParse =
                    model.parsedPhis
                    |> List.tryFind (fun parse -> parse.PhiId = validatedDraft.SourcePhiId)
                    |> Option.map (applyParseAmendment validatedDraft)

                match amendedParse with
                | None ->
                    { model with parseAmendmentStatus = Some "The source Phi parse is no longer available." }, Cmd.none
                | Some updatedParse ->
                    let parsedPhis =
                        model.parsedPhis
                        |> List.map (fun parse ->
                            if parse.PhiId = updatedParse.PhiId then
                                updatedParse
                            else
                                parse)

                    let selectedPhiParse =
                        match model.selectedPhiParse with
                        | Some selected when selected.PhiId = updatedParse.PhiId ->
                            Some updatedParse
                        | selected ->
                            selected

                    let selectedPhiResolution =
                        match selectedPhiParse with
                        | Some selected when selected.PhiId = updatedParse.PhiId ->
                            Some (Engine.resolveParse DemoData.demoSigma updatedParse)
                        | _ ->
                            model.selectedPhiResolution

                    let sigmaBasisItemDecisions =
                        model.sigmaBasisItemDecisions
                        |> removeParseAmendmentResetDecisions impactPreview

                    let resetSummary =
                        match List.length impactPreview.ResetImpacts with
                        | 0 -> "No basis-item decisions were reset."
                        | 1 -> "Reset 1 basis-item decision to pending."
                        | count -> "Reset " + string count + " basis-item decisions to pending."

                    { model with
                        parsedPhis = parsedPhis
                        sigmaBasisItemDecisions = sigmaBasisItemDecisions
                        selectedPhiParse = selectedPhiParse
                        selectedPhiResolution = selectedPhiResolution
                        parseAmendmentDraft = None
                        parseAmendmentStatus =
                            Some
                                ("Parse amendment confirmed for "
                                 + validatedDraft.SourcePhiId
                                 + ". Review affected T4/T5 candidate groups: "
                                 + formatAffectedParseAmendmentGroups validatedDraft.OriginalAtomKind validatedDraft.ProposedAtomKind
                                 + ". "
                                 + resetSummary)
                        phiBatchParseStatus = None }
                    |> appendLedgerEvent
                        "ParseAmendmentConfirmed"
                        validatedDraft.SourcePhiId
                        "Parse amendment confirmed"
                        (formatParseAmendmentLedgerDetail validatedDraft)
                    |> appendParseAmendmentDecisionResetLedgerEvents validatedDraft impactPreview.ResetImpacts
                    |> appendT6RealizationReviewNeededLedgerEvents impactPreview
                    |> fun updatedModel -> updatedModel, Cmd.none

    | CancelParseAmendment ->
        { model with
            parseAmendmentDraft = None
            parseAmendmentStatus = Some "Parse amendment cancelled." },
        Cmd.none

    | RetireParsedAtom (sourcePhiId, atomKind, atomText, provenance) ->
        let cleanedSourcePhiId = cleanFormValue sourcePhiId
        let cleanedAtomKind = cleanFormValue atomKind
        let cleanedAtomText = cleanFormValue atomText
        let atomKey = createParsedAtomReviewKey cleanedSourcePhiId cleanedAtomKind cleanedAtomText

        if String.IsNullOrWhiteSpace(cleanedSourcePhiId)
           || String.IsNullOrWhiteSpace(cleanedAtomKind)
           || String.IsNullOrWhiteSpace(cleanedAtomText) then
            { model with parseAmendmentStatus = Some "Select a parsed atom to retire." }, Cmd.none
        elif not (isValidParsedExposureAtomKind cleanedAtomKind) then
            { model with parseAmendmentStatus = Some "Select a valid parsed atom kind." }, Cmd.none
        elif isParsedAtomRetired model.LedgerEvents cleanedSourcePhiId cleanedAtomKind cleanedAtomText then
            { model with parseAmendmentStatus = Some "This interpretation is already retired from active Model Fitting." }, Cmd.none
        else
            match model.parsedPhis |> List.tryFind (fun parse -> parse.PhiId = cleanedSourcePhiId) with
            | None ->
                { model with parseAmendmentStatus = Some "The source Phi parse is no longer available." }, Cmd.none
            | Some parse ->
                let currentAtoms =
                    getExposureAtomValue cleanedAtomKind parse
                    |> splitExposureAtomValues

                if not (atomListContains cleanedAtomText currentAtoms) then
                    { model with parseAmendmentStatus = Some "This parsed atom has changed since the card was opened." }, Cmd.none
                else
                    let activeSequencedParsedPhis =
                        model.parsedPhis
                        |> getIncludedSequencedParsedPhis model.excludedPhiIds
                        |> applyParsedAtomRetirementsToSequencedPhis model.LedgerEvents

                    let candidateDeltas =
                        activeSequencedParsedPhis
                        |> buildSigmaContextWithContextEntries model.phiContextEntries
                        |> formulateCandidateDeltas

                    let resetImpacts =
                        buildParsedAtomRetirementResetImpacts
                            cleanedAtomKind
                            cleanedAtomText
                            cleanedSourcePhiId
                            candidateDeltas
                            model.sigmaBasisItemDecisions
                            activeSequencedParsedPhis

                    let affectedCandidateIds =
                        getParsedAtomRetirementAffectedCandidateIds
                            cleanedAtomKind
                            cleanedAtomText
                            cleanedSourcePhiId
                            candidateDeltas
                            activeSequencedParsedPhis

                    let sigmaBasisItemDecisions =
                        resetImpacts
                        |> List.fold
                            (fun decisions resetImpact -> decisions |> Map.remove resetImpact.BasisItemKey)
                            model.sigmaBasisItemDecisions

                    let candidateDecisionResetCount =
                        model.candidateDecisions
                        |> List.filter (fun decision -> affectedCandidateIds |> List.contains decision.CandidateId)
                        |> List.length

                    let candidateDecisions =
                        model.candidateDecisions
                        |> List.filter (fun decision -> not (affectedCandidateIds |> List.contains decision.CandidateId))

                    let parseAmendmentDraft =
                        match model.parseAmendmentDraft with
                        | Some draft
                            when draft.SourcePhiId = cleanedSourcePhiId
                                 && draft.OriginalAtomKind = cleanedAtomKind
                                 && atomTextEquals draft.OriginalAtomText cleanedAtomText ->
                            None
                        | draft -> draft

                    let resetSummary =
                        match List.length resetImpacts with
                        | 0 -> "No basis-item decisions were reset."
                        | 1 -> "Reset 1 basis-item decision to pending."
                        | count -> "Reset " + string count + " basis-item decisions to pending."

                    let candidateResetSummary =
                        match candidateDecisionResetCount with
                        | 0 -> "No candidate group decisions were reset."
                        | 1 -> "Reset 1 candidate group decision to pending."
                        | count -> "Reset " + string count + " candidate group decisions to pending."

                    { model with
                        candidateDecisions = candidateDecisions
                        sigmaBasisItemDecisions = sigmaBasisItemDecisions
                        parseAmendmentDraft = parseAmendmentDraft
                        parseAmendmentStatus =
                            Some
                                ("Retired interpretation from active Model Fitting for "
                                 + cleanedSourcePhiId
                                 + ". "
                                 + resetSummary
                                 + " "
                                 + candidateResetSummary) }
                    |> appendLedgerEvent
                        parsedAtomRetiredLedgerKind
                        atomKey
                        "Parsed atom retired"
                        (formatParsedAtomRetirementLedgerDetail
                            cleanedSourcePhiId
                            cleanedAtomKind
                            cleanedAtomText
                            provenance
                            (List.length resetImpacts)
                            candidateDecisionResetCount)
                    |> appendParsedAtomRetirementDecisionResetLedgerEvents
                        cleanedSourcePhiId
                        cleanedAtomKind
                        cleanedAtomText
                        resetImpacts
                    |> fun updatedModel -> updatedModel, Cmd.none

    | SelectParsedAtomReviewKind atomKind ->
        if isValidParsedExposureAtomKind atomKind then
            { model with selectedParsedAtomReviewKind = Some atomKind }, Cmd.none
        else
            model, Cmd.none

    | ClearParsedAtomReviewKind ->
        { model with
            selectedParsedAtomReviewKind = None
            parseAmendmentDraft = None },
        Cmd.none

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
        let contextProvenance =
            if isDerivedInquiryDraft model.phiDraftSource model.phiDraftQuickTags then
                "T6RealizationInquiry"
            else
                "OneSecSnip"

        let contextEntries =
            parsePhiContextSnipLines phiId 1 contextProvenance model.phiContextSnipDraft

        let intake =
            {
                PhiId = phiId
                Date = timestamp.ToString("yyyy-MM-dd")
                InputClass = optionalFormValue model.phiDraftInputClass
                Actor = optionalFormValue model.phiDraftActor
                Mission = optionalFormValue model.phiDraftMission
                OperationalContext = optionalFormValue model.phiDraftOperationalContext
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
            phiDraftInputClass = defaultPhiDraftInputClass
            phiDraftActor = ""
            phiDraftMission = ""
            phiDraftOperationalContext = ""
            phiDraftRawStatement = ""
            phiDraftTriggerContext = ""
            phiDraftSource = ""
            phiDraftQuickTags = ""
            phiDraftConfidence = "Medium"
            phiDraftStatus = None
            phiContextSnipDraft = ""
            phiContextEntries = model.phiContextEntries @ contextEntries
            phiBatchParseStatus = None }
        |> appendLedgerEvent "PhiIngested" intake.PhiId "Phi ingested" intake.RawStatement
        |> appendPhiContextEntryLedgerEvents contextEntries
        |> fun updatedModel -> updatedModel, Cmd.none

