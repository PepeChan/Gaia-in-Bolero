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

let private formatReviewNeededMarkLedgerDetail (mark: ReviewNeededMark) =
    [
        "State: " + reviewNeededLabel
        "Target kind: " + mark.TargetKind
        "Target ID: " + mark.TargetId
        "Source Phi ID: " + mark.SourcePhiId
        "Trigger: " + mark.Trigger
        "Reason: " + mark.Reason
    ]
    |> String.concat " | "

let private appendReviewNeededMark targetKind targetId sourcePhiId trigger reason (model: Model) =
    if hasReviewNeededMark targetKind targetId model.reviewNeededMarks then
        model
    else
        let mark =
            {
                TargetKind = targetKind
                TargetId = targetId
                SourcePhiId = sourcePhiId
                Trigger = trigger
                Reason = reason
                CreatedAtUtc = getUtcTimestampString ()
            }

        { model with reviewNeededMarks = model.reviewNeededMarks @ [ mark ] }
        |> appendLedgerEvent
            reviewNeededMarkedLedgerKind
            mark.TargetId
            "Review needed marked"
            (formatReviewNeededMarkLedgerDetail mark)

let private equalsReviewText left right =
    String.Equals(cleanFormValue left, cleanFormValue right, StringComparison.OrdinalIgnoreCase)

let private hasCandidateDecision candidateId (model: Model) =
    model.candidateDecisions
    |> List.exists (fun decision -> equalsReviewText decision.CandidateId candidateId)

let private phiIdIsSupportingSource phiId (supportingPhiIds: string list) =
    supportingPhiIds
    |> List.exists (equalsReviewText phiId)

let private markRealizationLinkReviewNeeded linkKind sourceId targetId sourcePhiId trigger reason model =
    model
    |> appendReviewNeededMark
        reviewTargetKindRealizationLink
        (realizationLinkReviewTargetId linkKind sourceId targetId)
        sourcePhiId
        trigger
        reason

let rec private markRealizationObjectDownstream sourcePhiId trigger reason objectKind objectId visited model =
    let objectTargetId = realizationObjectReviewTargetId objectKind objectId

    if visited |> Set.contains objectTargetId then
        model
    else
        let visited = visited |> Set.add objectTargetId

        let markedModel =
            model
            |> appendReviewNeededMark reviewTargetKindRealizationObject objectTargetId sourcePhiId trigger reason

        match objectKind with
        | kind when kind = realizationObjectKindFR ->
            getDpIdsForFR objectId markedModel.realizationState
            |> List.fold
                (fun workingModel dpId ->
                    workingModel
                    |> markRealizationLinkReviewNeeded realizationLinkKindFRToDP objectId dpId sourcePhiId trigger reason
                    |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindDP dpId visited)
                markedModel
        | kind when kind = realizationObjectKindPart ->
            getDpIdsForPart objectId markedModel.realizationState
            |> List.fold
                (fun workingModel dpId ->
                    workingModel
                    |> markRealizationLinkReviewNeeded realizationLinkKindPartToDP objectId dpId sourcePhiId trigger reason
                    |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindDP dpId visited)
                markedModel
        | kind when kind = realizationObjectKindDP ->
            getTfIdsForDp objectId markedModel.realizationState
            |> List.fold
                (fun workingModel tfId ->
                    workingModel
                    |> markRealizationLinkReviewNeeded realizationLinkKindDPToTF objectId tfId sourcePhiId trigger reason
                    |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindTF tfId visited)
                markedModel
        | kind when kind = realizationObjectKindTF ->
            getCtqIdsForTf objectId markedModel.realizationState
            |> List.fold
                (fun workingModel ctqId ->
                    workingModel
                    |> markRealizationLinkReviewNeeded realizationLinkKindTFToCTQ objectId ctqId sourcePhiId trigger reason
                    |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindCTQ ctqId visited)
                markedModel
        | kind when kind = realizationObjectKindCTQ ->
            getVvIdsForCtq objectId markedModel.realizationState
            |> List.fold
                (fun workingModel vvId ->
                    workingModel
                    |> markRealizationLinkReviewNeeded realizationLinkKindCTQToVV objectId vvId sourcePhiId trigger reason
                    |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindVV vvId visited)
                markedModel
        | _ ->
            markedModel

let private markHostRealizationReviewNeeded sourcePhiId trigger reason hostValue model =
    let pathTargetId = realizationPathReviewTargetId realizationSourceKindHost hostValue

    let markedModel =
        model
        |> appendReviewNeededMark reviewTargetKindRealizationPath pathTargetId sourcePhiId trigger reason

    markedModel.realizationState.Host_to_Part
    |> List.filter (fun (sourceId, _) -> equalsReviewText sourceId hostValue)
    |> List.fold
        (fun workingModel (sourceId, partId) ->
            workingModel
            |> markRealizationLinkReviewNeeded realizationLinkKindHostToPart sourceId partId sourcePhiId trigger reason
            |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindPart partId Set.empty)
        markedModel

let private markFunctionRealizationReviewNeeded sourcePhiId trigger reason functionValue model =
    let pathTargetId = realizationPathReviewTargetId realizationSourceKindFunction functionValue

    let markedModel =
        model
        |> appendReviewNeededMark reviewTargetKindRealizationPath pathTargetId sourcePhiId trigger reason

    markedModel.realizationState.Function_to_FR
    |> List.filter (fun (sourceId, _) -> equalsReviewText sourceId functionValue)
    |> List.fold
        (fun workingModel (sourceId, frId) ->
            workingModel
            |> markRealizationLinkReviewNeeded realizationLinkKindFunctionToFR sourceId frId sourcePhiId trigger reason
            |> markRealizationObjectDownstream sourcePhiId trigger reason realizationObjectKindFR frId Set.empty)
        markedModel

let private getSigmaSourceEntries atomKind sigmaContext =
    match atomKind with
    | "Host" -> sigmaContext.Hosts
    | "Function" -> sigmaContext.Functions
    | _ -> []

let private getAffectedSigmaSourceValues atomKind sourcePhiId (model: Model) =
    model
    |> getCurrentSigmaContext
    |> getSigmaSourceEntries atomKind
    |> List.filter (fun entry -> phiIdIsSupportingSource sourcePhiId entry.SupportingPhiIds)
    |> List.map (fun entry -> entry.Value)
    |> List.distinct

let private markGovernanceReviewNeededForPhi sourcePhiId trigger reason (model: Model) =
    let basisMarkedModel =
        model
        |> getCurrentSigmaBasisItemLedgerContexts
        |> List.filter (fun context ->
            phiIdIsSupportingSource sourcePhiId context.BasisItem.SupportingPhiIds
            && getSigmaBasisItemDecisionValue context.BasisItem.Key model.sigmaBasisItemDecisions <> Pending)
        |> List.fold
            (fun workingModel context ->
                workingModel
                |> appendReviewNeededMark
                    reviewTargetKindSigmaBasisItemDecision
                    context.BasisItem.Key
                    sourcePhiId
                    trigger
                    reason)
            model

    basisMarkedModel
    |> getCurrentCandidateDeltas
    |> List.filter (fun candidate -> getCandidateSupportingPhiIds candidate |> phiIdIsSupportingSource sourcePhiId)
    |> List.map (fun candidate -> candidate.CandidateId)
    |> List.distinct
    |> List.filter (fun candidateId -> hasCandidateDecision candidateId basisMarkedModel)
    |> List.fold
        (fun workingModel candidateId ->
            workingModel
            |> appendReviewNeededMark reviewTargetKindCandidateDecision candidateId sourcePhiId trigger reason)
        basisMarkedModel

let private markRealizationReviewNeededForPhi sourcePhiId trigger reason (model: Model) =
    let hostValues = getAffectedSigmaSourceValues "Host" sourcePhiId model
    let functionValues = getAffectedSigmaSourceValues "Function" sourcePhiId model

    model
    |> fun workingModel ->
        hostValues
        |> List.fold
            (fun modelWithHostMarks hostValue ->
                modelWithHostMarks
                |> markHostRealizationReviewNeeded sourcePhiId trigger reason hostValue)
            workingModel
    |> fun workingModel ->
        functionValues
        |> List.fold
            (fun modelWithFunctionMarks functionValue ->
                modelWithFunctionMarks
                |> markFunctionRealizationReviewNeeded sourcePhiId trigger reason functionValue)
            workingModel

let private markReviewNeededForPhiImpact sourcePhiId trigger reason (model: Model) =
    model
    |> markGovernanceReviewNeededForPhi sourcePhiId trigger reason
    |> markRealizationReviewNeededForPhi sourcePhiId trigger reason

let private markReviewNeededForParseAmendment (draft: ParseAmendmentDraft) (impact: ParseAmendmentImpactPreview) model =
    let trigger = "Parse amendment confirmed"
    let reason = "A parsed interpretation was amended; dependent governance or realization should be reviewed."

    let basisMarkedModel =
        impact.ResetImpacts
        |> List.fold
            (fun workingModel impact ->
                workingModel
                |> appendReviewNeededMark
                    reviewTargetKindSigmaBasisItemDecision
                    impact.BasisItemKey
                    draft.SourcePhiId
                    trigger
                    reason)
            model

    let governanceMarkedModel =
        impact.CandidateGroupImpacts
        |> List.map (fun impact -> impact.CandidateId)
        |> List.distinct
        |> List.filter (fun candidateId -> hasCandidateDecision candidateId basisMarkedModel)
        |> List.fold
            (fun workingModel candidateId ->
                workingModel
                |> appendReviewNeededMark reviewTargetKindCandidateDecision candidateId draft.SourcePhiId trigger reason)
            basisMarkedModel

    if equalsReviewText draft.OriginalAtomKind "Host" then
        governanceMarkedModel
        |> markHostRealizationReviewNeeded draft.SourcePhiId trigger reason draft.OriginalAtomText
    elif equalsReviewText draft.OriginalAtomKind "Function" then
        governanceMarkedModel
        |> markFunctionRealizationReviewNeeded draft.SourcePhiId trigger reason draft.OriginalAtomText
    else
        governanceMarkedModel

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
            |> markReviewNeededForPhiImpact
                phiId
                "Phi context changed"
                "A Phi context entry changed the source interpretation; dependent decisions and realization should be reviewed."

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
                        |> markReviewNeededForPhiImpact
                            evidenceRecord.TargetId
                            "Phi evidence changed"
                            "Phi-targeted evidence changed the source interpretation; dependent decisions and realization should be reviewed."
                    else
                        evidenceModel

                contextAwareEvidenceModel
                |> appendLedgerEvent "EvidenceReferenceCreated" evidenceId "Evidence reference created" detail
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

        let reviewMarkedModel =
            if wasExcluded then
                model
            else
                model
                |> markReviewNeededForPhiImpact
                    phiId
                    "Phi excluded from replay"
                    "A parsed Phi was excluded from replay; dependent decisions and realization should be reviewed."

        { reviewMarkedModel with
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

                    let reviewSummary =
                        match List.length impactPreview.ResetImpacts with
                        | 0 -> "No existing basis-item decision needed review marking."
                        | 1 -> "Marked 1 basis-item decision Review Needed."
                        | count -> "Marked " + string count + " basis-item decisions Review Needed."

                    { model with
                        parsedPhis = parsedPhis
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
                                 + reviewSummary)
                        phiBatchParseStatus = None }
                    |> appendLedgerEvent
                        "ParseAmendmentConfirmed"
                        validatedDraft.SourcePhiId
                        "Parse amendment confirmed"
                        (formatParseAmendmentLedgerDetail validatedDraft)
                    |> markReviewNeededForParseAmendment validatedDraft impactPreview
                    |> fun updatedModel -> updatedModel, Cmd.none

    | CancelParseAmendment ->
        { model with
            parseAmendmentDraft = None
            parseAmendmentStatus = Some "Parse amendment cancelled." },
        Cmd.none

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

