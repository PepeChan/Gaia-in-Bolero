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

let private tryFindById objectId getId values =
    values
    |> List.tryFind (fun value -> String.Equals(clean objectId, clean (getId value), StringComparison.OrdinalIgnoreCase))

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

let private equalsId left right =
    String.Equals(clean left, clean right, StringComparison.OrdinalIgnoreCase)

let private distinctIds values =
    values
    |> List.map clean
    |> List.filter hasText
    |> List.fold
        (fun selected value ->
            if selected |> List.exists (equalsId value) then
                selected
            else
                selected @ [ value ])
        []

let private distinctLinkPairs links =
    links
    |> List.map (fun (source, target) -> clean source, clean target)
    |> List.filter (fun (source, target) -> hasText source && hasText target)
    |> List.fold
        (fun selected (source, target) ->
            if selected |> List.exists (fun (existingSource, existingTarget) -> equalsId source existingSource && equalsId target existingTarget) then
                selected
            else
                selected @ [ source, target ])
        []

let private reachableTargetIds sourceIds links =
    let sourceIds = distinctIds sourceIds

    links
    |> List.choose (fun (source, target) ->
        if sourceIds |> List.exists (equalsId source) then
            Some target
        else
            None)
    |> distinctIds

let private collectReachableTargetIds sourceIds links =
    sourceIds
    |> List.collect (fun sourceId -> reachableTargetIds [ sourceId ] links)
    |> distinctIds

let private isPartId state objectId =
    state.Sigma.Parts
    |> List.exists (fun part -> equalsId objectId part.Id)

let private isDpId state objectId =
    state.Sigma.DPs
    |> List.exists (fun dp -> equalsId objectId dp.Id)

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
    | kind when kind = realizationLinkKindPartToDP -> partOptions state
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
    | kind when kind = realizationLinkKindPartToDP -> dpOptions state
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
    | kind when kind = realizationLinkKindPartToDP ->
        { state with Sigma = { state.Sigma with DP_to_Part = appendUniqueLink link state.Sigma.DP_to_Part } }
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
    | kind when kind = realizationLinkKindPartToDP ->
        (state.Sigma.DP_to_Part |> List.contains link)
        || (state.Sigma.DP_to_Part |> List.contains (targetId, sourceId))
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

let private partToDpLinks (state: RealizationState) =
    state.Sigma.DP_to_Part
    |> List.map (fun (source, target) ->
        if isPartId state source && isDpId state target then
            source, target
        elif isDpId state source && isPartId state target then
            target, source
        else
            source, target)
    |> distinctLinkPairs

let getPartIdsForHost hostValue (state: RealizationState) =
    reachableTargetIds [ hostValue ] state.Host_to_Part

let getDpIdsForPart partId (state: RealizationState) =
    reachableTargetIds [ partId ] (partToDpLinks state)

let getTfIdsForDp dpId (state: RealizationState) =
    reachableTargetIds [ dpId ] state.Sigma.DP_to_TF

let getCtqIdsForTf tfId (state: RealizationState) =
    reachableTargetIds [ tfId ] state.Sigma.TF_to_CTQ

let getVvIdsForCtq ctqId (state: RealizationState) =
    reachableTargetIds [ ctqId ] state.CTQ_to_VV

let getPartContinuityIds partId (state: RealizationState) =
    let dpIds =
        getDpIdsForPart partId state

    let tfIds =
        dpIds
        |> List.collect (fun dpId -> getTfIdsForDp dpId state)
        |> distinctIds

    let ctqIds =
        tfIds
        |> List.collect (fun tfId -> getCtqIdsForTf tfId state)
        |> distinctIds

    let vvIds =
        ctqIds
        |> List.collect (fun ctqId -> getVvIdsForCtq ctqId state)
        |> distinctIds

    dpIds, tfIds, ctqIds, vvIds

let getHostContinuityIds hostValue (state: RealizationState) =
    let partIds =
        getPartIdsForHost hostValue state

    let dpIds =
        collectReachableTargetIds partIds (partToDpLinks state)

    let tfIds =
        collectReachableTargetIds dpIds state.Sigma.DP_to_TF

    let ctqIds =
        collectReachableTargetIds tfIds state.Sigma.TF_to_CTQ

    let vvIds =
        collectReachableTargetIds ctqIds state.CTQ_to_VV

    partIds, dpIds, tfIds, ctqIds, vvIds

let getHostRealizationStatus hostValue (state: RealizationState) =
    let partIds, dpIds, tfIds, ctqIds, vvIds = getHostContinuityIds hostValue state

    if List.isEmpty partIds then
        "Not realized"
    elif List.isEmpty dpIds then
        "Part linked"
    elif List.isEmpty tfIds then
        "DP linked"
    elif List.isEmpty ctqIds then
        "TF linked"
    elif List.isEmpty vvIds then
        "CTQ linked"
    else
        "Continuity complete"

type ReadinessState =
    | Missing
    | Partial
    | Complete

type HostReadiness =
    {
        Part: ReadinessState
        DP: ReadinessState
        TF: ReadinessState
        CTQ: ReadinessState
        VV: ReadinessState
        Overall: ReadinessState
    }

let getReadinessLabel readiness =
    match readiness with
    | Missing -> "Missing"
    | Partial -> "Partial"
    | Complete -> "Complete"

let getReadinessSymbol readiness =
    match readiness with
    | Missing -> "◯"
    | Partial -> "◐"
    | Complete -> "●"

let private aggregateReadiness readinessValues =
    match readinessValues with
    | [] -> Missing
    | values when values |> List.forall ((=) Complete) -> Complete
    | values when values |> List.forall ((=) Missing) -> Missing
    | _ -> Partial

let private readinessFromLinkedTargets targetIds =
    if targetIds |> List.isEmpty then
        Missing
    else
        Complete

let private readinessFromObjectLinks objectIds getTargetIds (state: RealizationState) =
    objectIds
    |> List.map (fun objectId -> getTargetIds objectId state |> readinessFromLinkedTargets)
    |> aggregateReadiness

let getRealizationObjectReadiness objectKind objectId (state: RealizationState) =
    match objectKind with
    | kind when kind = realizationObjectKindFR ->
        reachableTargetIds [ objectId ] state.Sigma.FR_to_DP
        |> readinessFromLinkedTargets
    | kind when kind = realizationObjectKindPart ->
        getDpIdsForPart objectId state
        |> readinessFromLinkedTargets
    | kind when kind = realizationObjectKindDP ->
        getTfIdsForDp objectId state
        |> readinessFromLinkedTargets
    | kind when kind = realizationObjectKindTF ->
        getCtqIdsForTf objectId state
        |> readinessFromLinkedTargets
    | kind when kind = realizationObjectKindCTQ ->
        getVvIdsForCtq objectId state
        |> readinessFromLinkedTargets
    | kind when kind = realizationObjectKindVV ->
        if realizationObjectExists kind objectId state then
            Complete
        else
            Missing
    | _ -> Missing

let private getRealizationObjectName objectKind objectId (state: RealizationState) =
    let name =
        match objectKind with
        | kind when kind = realizationObjectKindFR ->
            state.Sigma.FRs
            |> tryFindById objectId (fun (item: FR) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | kind when kind = realizationObjectKindDP ->
            state.Sigma.DPs
            |> tryFindById objectId (fun (item: DP) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | kind when kind = realizationObjectKindTF ->
            state.Sigma.TFs
            |> tryFindById objectId (fun (item: TF) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | kind when kind = realizationObjectKindCTQ ->
            state.Sigma.CTQs
            |> tryFindById objectId (fun (item: CTQ) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | kind when kind = realizationObjectKindPart ->
            state.Sigma.Parts
            |> tryFindById objectId (fun (item: Part) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | kind when kind = realizationObjectKindVV ->
            state.VVItems
            |> tryFindById objectId (fun (item: VVItem) -> item.Id)
            |> Option.map (fun item -> item.Name)
        | _ ->
            None

    name |> Option.defaultValue ""

let getHostReadiness hostValue (state: RealizationState) =
    let partIds, dpIds, tfIds, ctqIds, vvIds = getHostContinuityIds hostValue state

    let partReadiness =
        readinessFromObjectLinks partIds getDpIdsForPart state

    let dpReadiness =
        readinessFromObjectLinks dpIds getTfIdsForDp state

    let tfReadiness =
        readinessFromObjectLinks tfIds getCtqIdsForTf state

    let ctqReadiness =
        readinessFromObjectLinks ctqIds getVvIdsForCtq state

    let vvReadiness =
        vvIds
        |> List.map (fun vvId -> getRealizationObjectReadiness realizationObjectKindVV vvId state)
        |> aggregateReadiness

    let overall =
        aggregateReadiness [ partReadiness; dpReadiness; tfReadiness; ctqReadiness; vvReadiness ]

    {
        Part = partReadiness
        DP = dpReadiness
        TF = tfReadiness
        CTQ = ctqReadiness
        VV = vvReadiness
        Overall = overall
    }

type RealizationTraceNode =
    {
        ObjectKind: string
        ObjectId: string
        ObjectName: string
        Readiness: ReadinessState
        MissingNextKind: string option
        Children: RealizationTraceNode list
    }

type HostRealizationTrace =
    {
        HostValue: string
        Readiness: ReadinessState
        Parts: RealizationTraceNode list
    }

let private makeRealizationTraceNode objectKind objectId missingNextKind children (state: RealizationState) =
    {
        ObjectKind = objectKind
        ObjectId = objectId
        ObjectName = getRealizationObjectName objectKind objectId state
        Readiness = getRealizationObjectReadiness objectKind objectId state
        MissingNextKind =
            if children |> List.isEmpty then
                missingNextKind
            else
                None
        Children = children
    }

let getHostRealizationTrace hostValue (state: RealizationState) =
    let rec buildPartNode partId =
        let children =
            getDpIdsForPart partId state
            |> List.map buildDpNode

        makeRealizationTraceNode realizationObjectKindPart partId (Some realizationObjectKindDP) children state

    and buildDpNode dpId =
        let children =
            getTfIdsForDp dpId state
            |> List.map buildTfNode

        makeRealizationTraceNode realizationObjectKindDP dpId (Some realizationObjectKindTF) children state

    and buildTfNode tfId =
        let children =
            getCtqIdsForTf tfId state
            |> List.map buildCtqNode

        makeRealizationTraceNode realizationObjectKindTF tfId (Some realizationObjectKindCTQ) children state

    and buildCtqNode ctqId =
        let children =
            getVvIdsForCtq ctqId state
            |> List.map buildVvNode

        makeRealizationTraceNode realizationObjectKindCTQ ctqId (Some "VV") children state

    and buildVvNode vvId =
        makeRealizationTraceNode realizationObjectKindVV vvId None [] state

    {
        HostValue = hostValue
        Readiness = (getHostReadiness hostValue state).Overall
        Parts =
            getPartIdsForHost hostValue state
            |> List.map buildPartNode
    }

let getGapReadiness totalCount gapCount =
    if gapCount = 0 then
        Complete
    elif totalCount > gapCount then
        Partial
    else
        Missing

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
    let partToDpRows =
        partToDpLinks state
        |> List.map (fun (source, target) -> realizationLinkKindPartToDP, source, target)

    [
        yield! state.Host_to_Part |> List.map (fun (source, target) -> realizationLinkKindHostToPart, source, target)
        yield! state.Function_to_FR |> List.map (fun (source, target) -> realizationLinkKindFunctionToFR, source, target)
        yield! state.Sigma.FR_to_DP |> List.map (fun (source, target) -> realizationLinkKindFRToDP, source, target)
        yield! partToDpRows
        yield! state.Sigma.DP_to_TF |> List.map (fun (source, target) -> realizationLinkKindDPToTF, source, target)
        yield! state.Sigma.TF_to_CTQ |> List.map (fun (source, target) -> realizationLinkKindTFToCTQ, source, target)
        yield! state.CTQ_to_VV |> List.map (fun (source, target) -> realizationLinkKindCTQToVV, source, target)
    ]
