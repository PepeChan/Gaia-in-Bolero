module Gaia.Client.Realization

open System
open Gaia.Core
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow

let private clean (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private hasText value =
    not (String.IsNullOrWhiteSpace(value))

let private appendUniqueLink link links =
    if links |> List.exists (fun existing -> existing = link) then
        links
    else
        links @ [ link ]

let private removeObjectNote objectKind objectId notes =
    notes
    |> List.filter (fun note -> not (note.ObjectKind = objectKind && note.ObjectId = objectId))

let private upsertObjectNote objectKind objectId description sourceNote (state: RealizationState) =
    let description = clean description
    let sourceNote = clean sourceNote
    let notesWithoutExisting = removeObjectNote objectKind objectId state.ObjectNotes

    if hasText description || hasText sourceNote then
        { state with
            ObjectNotes =
                notesWithoutExisting
                @ [
                    {
                        ObjectKind = objectKind
                        ObjectId = objectId
                        Description = description
                        SourceNote = sourceNote
                    }
                ] }
    else
        { state with ObjectNotes = notesWithoutExisting }

let tryFindObjectNote objectKind objectId (state: RealizationState) =
    state.ObjectNotes
    |> List.tryFind (fun note -> note.ObjectKind = objectKind && note.ObjectId = objectId)

let private idExists objectId getId values =
    values
    |> List.exists (fun value -> getId value = objectId)

let private realizationObjectExists objectKind objectId (state: RealizationState) =
    match objectKind with
    | kind when kind = realizationObjectKindFR ->
        state.Sigma.FRs |> idExists objectId (fun (item: FR) -> item.Id)
    | kind when kind = realizationObjectKindDP ->
        state.Sigma.DPs |> idExists objectId (fun (item: DP) -> item.Id)
    | kind when kind = realizationObjectKindTF ->
        state.Sigma.TFs |> idExists objectId (fun (item: TF) -> item.Id)
    | kind when kind = realizationObjectKindCTQ ->
        state.Sigma.CTQs |> idExists objectId (fun (item: CTQ) -> item.Id)
    | kind when kind = realizationObjectKindPart ->
        state.Sigma.Parts |> idExists objectId (fun (item: Part) -> item.Id)
    | kind when kind = realizationObjectKindVV ->
        state.VVItems |> idExists objectId (fun (item: VVItem) -> item.Id)
    | _ -> false

let tryAddRealizationObject objectKind objectId objectName description sourceNote (state: RealizationState) : Result<RealizationState, string> =
    let objectKind = clean objectKind
    let objectId = clean objectId
    let objectName = clean objectName

    if not (realizationObjectKinds |> List.contains objectKind) then
        Result.Error "Select a realization object kind."
    elif not (hasText objectId) then
        Result.Error "Object Id is required."
    elif not (hasText objectName) then
        Result.Error "Object Name is required."
    elif realizationObjectExists objectKind objectId state then
        Result.Error (objectKind + " already exists with Id " + objectId + ".")
    else
        let updatedState =
            match objectKind with
            | kind when kind = realizationObjectKindFR ->
                let item: FR = { Id = objectId; Name = objectName }
                { state with Sigma = { state.Sigma with FRs = state.Sigma.FRs @ [ item ] } }
            | kind when kind = realizationObjectKindDP ->
                let item: DP = { Id = objectId; Name = objectName }
                { state with Sigma = { state.Sigma with DPs = state.Sigma.DPs @ [ item ] } }
            | kind when kind = realizationObjectKindTF ->
                let item: TF = { Id = objectId; Name = objectName }
                { state with Sigma = { state.Sigma with TFs = state.Sigma.TFs @ [ item ] } }
            | kind when kind = realizationObjectKindCTQ ->
                let item: CTQ = { Id = objectId; Name = objectName }
                { state with Sigma = { state.Sigma with CTQs = state.Sigma.CTQs @ [ item ] } }
            | kind when kind = realizationObjectKindPart ->
                let item: Part = { Id = objectId; Name = objectName }
                { state with Sigma = { state.Sigma with Parts = state.Sigma.Parts @ [ item ] } }
            | _ ->
                let item: VVItem = { Id = objectId; Name = objectName }
                { state with VVItems = state.VVItems @ [ item ] }

        updatedState
        |> upsertObjectNote objectKind objectId description sourceNote
        |> Result.Ok

let private acceptedSigmaBasisAtomValues atomKind (model: Model) =
    model
    |> getCurrentSigmaBasisItemLedgerContexts
    |> List.choose (fun context ->
        let decision = getSigmaBasisItemDecisionValue context.BasisItem.Key model.sigmaBasisItemDecisions

        if context.BasisItem.Kind = atomKind && decision = Accepted then
            Some context.BasisItem.AtomValue
        else
            None)
    |> Set.ofList

let private getAcceptedOrKnownSigmaEntries atomKind (entries: SigmaContextEntry list) (model: Model) =
    let acceptedValues = acceptedSigmaBasisAtomValues atomKind model

    if Set.isEmpty acceptedValues then
        entries
    else
        entries
        |> List.filter (fun entry -> acceptedValues |> Set.contains entry.Value)

let getRealizationSourceHosts (model: Model) =
    model
    |> getCurrentSigmaContext
    |> fun sigmaContext -> getAcceptedOrKnownSigmaEntries "Host" sigmaContext.Hosts model

let getRealizationSourceFunctions (model: Model) =
    model
    |> getCurrentSigmaContext
    |> fun sigmaContext -> getAcceptedOrKnownSigmaEntries "Function" sigmaContext.Functions model

let private sigmaEntryOption atomKind (entry: SigmaContextEntry) =
    entry.Value,
    entry.Value
    + " ("
    + atomKind
    + "; support "
    + string entry.SupportCount
    + ")"

let private frOptions (state: RealizationState) =
    state.Sigma.FRs
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let private dpOptions (state: RealizationState) =
    state.Sigma.DPs
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let private tfOptions (state: RealizationState) =
    state.Sigma.TFs
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let private ctqOptions (state: RealizationState) =
    state.Sigma.CTQs
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let private partOptions (state: RealizationState) =
    state.Sigma.Parts
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let private vvOptions (state: RealizationState) =
    state.VVItems
    |> List.map (fun item -> item.Id, item.Id + " - " + item.Name)

let getRealizationLinkSourceOptions linkKind (model: Model) =
    let state = model.realizationState

    match linkKind with
    | kind when kind = realizationLinkKindHostToPart ->
        getRealizationSourceHosts model
        |> List.map (sigmaEntryOption "Host")
    | kind when kind = realizationLinkKindFunctionToFR ->
        getRealizationSourceFunctions model
        |> List.map (sigmaEntryOption "Function")
    | kind when kind = realizationLinkKindFRToDP -> frOptions state
    | kind when kind = realizationLinkKindDPToTF -> dpOptions state
    | kind when kind = realizationLinkKindTFToCTQ -> tfOptions state
    | kind when kind = realizationLinkKindCTQToVV -> ctqOptions state
    | kind when kind = realizationLinkKindDPToPart -> dpOptions state
    | _ -> []

let getRealizationLinkTargetOptions linkKind (model: Model) =
    let state = model.realizationState

    match linkKind with
    | kind when kind = realizationLinkKindHostToPart -> partOptions state
    | kind when kind = realizationLinkKindFunctionToFR -> frOptions state
    | kind when kind = realizationLinkKindFRToDP -> dpOptions state
    | kind when kind = realizationLinkKindDPToTF -> tfOptions state
    | kind when kind = realizationLinkKindTFToCTQ -> ctqOptions state
    | kind when kind = realizationLinkKindCTQToVV -> vvOptions state
    | kind when kind = realizationLinkKindDPToPart -> partOptions state
    | _ -> []

let private optionExists selected options =
    options
    |> List.exists (fun (value, _) -> value = selected)

let private addRealizationLinkUnchecked linkKind sourceId targetId (state: RealizationState) =
    let link = sourceId, targetId

    match linkKind with
    | kind when kind = realizationLinkKindHostToPart ->
        { state with Host_to_Part = appendUniqueLink link state.Host_to_Part }
    | kind when kind = realizationLinkKindFunctionToFR ->
        { state with Function_to_FR = appendUniqueLink link state.Function_to_FR }
    | kind when kind = realizationLinkKindFRToDP ->
        { state with Sigma = { state.Sigma with FR_to_DP = appendUniqueLink link state.Sigma.FR_to_DP } }
    | kind when kind = realizationLinkKindDPToTF ->
        { state with Sigma = { state.Sigma with DP_to_TF = appendUniqueLink link state.Sigma.DP_to_TF } }
    | kind when kind = realizationLinkKindTFToCTQ ->
        { state with Sigma = { state.Sigma with TF_to_CTQ = appendUniqueLink link state.Sigma.TF_to_CTQ } }
    | kind when kind = realizationLinkKindCTQToVV ->
        { state with CTQ_to_VV = appendUniqueLink link state.CTQ_to_VV }
    | kind when kind = realizationLinkKindDPToPart ->
        { state with Sigma = { state.Sigma with DP_to_Part = appendUniqueLink link state.Sigma.DP_to_Part } }
    | _ -> state

let private realizationLinkExists linkKind sourceId targetId (state: RealizationState) =
    let link = sourceId, targetId

    match linkKind with
    | kind when kind = realizationLinkKindHostToPart -> state.Host_to_Part |> List.contains link
    | kind when kind = realizationLinkKindFunctionToFR -> state.Function_to_FR |> List.contains link
    | kind when kind = realizationLinkKindFRToDP -> state.Sigma.FR_to_DP |> List.contains link
    | kind when kind = realizationLinkKindDPToTF -> state.Sigma.DP_to_TF |> List.contains link
    | kind when kind = realizationLinkKindTFToCTQ -> state.Sigma.TF_to_CTQ |> List.contains link
    | kind when kind = realizationLinkKindCTQToVV -> state.CTQ_to_VV |> List.contains link
    | kind when kind = realizationLinkKindDPToPart -> state.Sigma.DP_to_Part |> List.contains link
    | _ -> false

let tryAddRealizationLink linkKind sourceId targetId (model: Model) : Result<RealizationState, string> =
    let linkKind = clean linkKind
    let sourceId = clean sourceId
    let targetId = clean targetId
    let sourceOptions = getRealizationLinkSourceOptions linkKind model
    let targetOptions = getRealizationLinkTargetOptions linkKind model

    if not (realizationLinkKinds |> List.contains linkKind) then
        Result.Error "Select a realization link kind."
    elif not (hasText sourceId) then
        Result.Error "Select a link source."
    elif not (hasText targetId) then
        Result.Error "Select a link target."
    elif not (optionExists sourceId sourceOptions) then
        Result.Error "Select a valid link source."
    elif not (optionExists targetId targetOptions) then
        Result.Error "Select a valid link target."
    elif realizationLinkExists linkKind sourceId targetId model.realizationState then
        Result.Error "That realization link already exists."
    else
        model.realizationState
        |> addRealizationLinkUnchecked linkKind sourceId targetId
        |> Result.Ok

let private targetsFor sourceId links =
    links
    |> List.choose (fun (source, target) ->
        if source = sourceId then
            Some target
        else
            None)

let private sourcesFor targetId links =
    links
    |> List.choose (fun (source, target) ->
        if target = targetId then
            Some source
        else
            None)

let getPartIdsForHost hostValue (state: RealizationState) =
    targetsFor hostValue state.Host_to_Part

let getDpIdsForPart partId (state: RealizationState) =
    sourcesFor partId state.Sigma.DP_to_Part

let getTfIdsForDp dpId (state: RealizationState) =
    targetsFor dpId state.Sigma.DP_to_TF

let getCtqIdsForTf tfId (state: RealizationState) =
    targetsFor tfId state.Sigma.TF_to_CTQ

let getVvIdsForCtq ctqId (state: RealizationState) =
    targetsFor ctqId state.CTQ_to_VV

let getHostRealizationStatus hostValue (state: RealizationState) =
    let partIds = getPartIdsForHost hostValue state

    if List.isEmpty partIds then
        "Not realized"
    else
        let dpIds =
            partIds
            |> List.collect (fun partId -> getDpIdsForPart partId state)
            |> List.distinct

        if List.isEmpty dpIds then
            "Partially realized"
        else
            let tfIds =
                dpIds
                |> List.collect (fun dpId -> getTfIdsForDp dpId state)
                |> List.distinct

            if List.isEmpty tfIds then
                "Realization started"
            else
                let ctqIds =
                    tfIds
                    |> List.collect (fun tfId -> getCtqIdsForTf tfId state)
                    |> List.distinct

                if List.isEmpty ctqIds then
                    "Behavior linked"
                else
                    "Verification path started"

let getHostsWithoutParts (hostEntries: SigmaContextEntry list) (state: RealizationState) =
    hostEntries
    |> List.filter (fun entry -> getPartIdsForHost entry.Value state |> List.isEmpty)

let getPartsWithoutDPs (state: RealizationState) =
    state.Sigma.Parts
    |> List.filter (fun part -> getDpIdsForPart part.Id state |> List.isEmpty)

let getDPsWithoutTFs (state: RealizationState) =
    state.Sigma.DPs
    |> List.filter (fun dp -> getTfIdsForDp dp.Id state |> List.isEmpty)

let getTFsWithoutCTQs (state: RealizationState) =
    state.Sigma.TFs
    |> List.filter (fun tf -> getCtqIdsForTf tf.Id state |> List.isEmpty)

let getCTQsWithoutVV (state: RealizationState) =
    state.Sigma.CTQs
    |> List.filter (fun ctq -> getVvIdsForCtq ctq.Id state |> List.isEmpty)

let getRealizationLinkRows (state: RealizationState) =
    [
        yield! state.Host_to_Part |> List.map (fun (source, target) -> realizationLinkKindHostToPart, source, target)
        yield! state.Function_to_FR |> List.map (fun (source, target) -> realizationLinkKindFunctionToFR, source, target)
        yield! state.Sigma.FR_to_DP |> List.map (fun (source, target) -> realizationLinkKindFRToDP, source, target)
        yield! state.Sigma.DP_to_TF |> List.map (fun (source, target) -> realizationLinkKindDPToTF, source, target)
        yield! state.Sigma.TF_to_CTQ |> List.map (fun (source, target) -> realizationLinkKindTFToCTQ, source, target)
        yield! state.CTQ_to_VV |> List.map (fun (source, target) -> realizationLinkKindCTQToVV, source, target)
        yield! state.Sigma.DP_to_Part |> List.map (fun (source, target) -> realizationLinkKindDPToPart, source, target)
    ]
