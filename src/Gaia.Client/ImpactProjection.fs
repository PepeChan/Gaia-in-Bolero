module Gaia.Client.ImpactProjection

open System
open Bolero
open Bolero.Html
open Gaia.Client.Types
open Gaia.Client.AppState
open Gaia.Client.Workflow
open Gaia.Client.Realization

type ImpactProjectionRowScope =
    | GovernanceImpact
    | RealizationImpact

type ImpactProjectionRow =
    {
        Scope: ImpactProjectionRowScope
        RowId: string
        TriggerSource: string
        ReviewNeededTarget: string
        Explanation: string
        CandidateGroupIds: string list
        CandidateGroupLabel: string option
        BasisItemKeys: string list
        SupportingPhiId: string option
        SourceInterpretation: string
        AffectedDownstreamObjects: string list
        AffectedPaths: string list
        MissingDownstreamLevels: string list
        CurrentDecisionStatus: string
    }

type ImpactProjection =
    {
        TotalReviewNeededMarks: int
        AffectedCandidateGroups: int
        AffectedBasisItems: int
        AffectedRealizationPaths: int
        AffectedRealizationObjects: int
        GovernanceRows: ImpactProjectionRow list
        RealizationRows: ImpactProjectionRow list
    }

type private ParsedBasisTarget =
    {
        CandidateId: string
        AtomKind: string
        AtomValue: string
    }

type private RealizationProjectionData =
    {
        Objects: string list
        Paths: string list
        Missing: string list
    }

let private emptyRealizationProjectionData =
    {
        Objects = []
        Paths = []
        Missing = []
    }

let private clean (value: string) =
    if isNull value then
        ""
    else
        value.Trim()

let private hasText value =
    not (String.IsNullOrWhiteSpace(value))

let private equalsText left right =
    String.Equals(clean left, clean right, StringComparison.OrdinalIgnoreCase)

let private compact maxLength (value: string) =
    let value = clean value

    if value.Length <= maxLength then
        value
    else
        value.Substring(0, maxLength).TrimEnd() + "..."

let private distinctText values =
    values
    |> List.map clean
    |> List.filter hasText
    |> List.fold
        (fun selected value ->
            if selected |> List.exists (equalsText value) then
                selected
            else
                selected @ [ value ])
        []

let private splitReviewTargetId (targetId: string) =
    if isNull targetId then
        []
    else
        targetId.Split([| '|' |], StringSplitOptions.None)
        |> Array.toList
        |> List.map clean

let private tryParseBasisTarget (targetId: string) =
    if isNull targetId then
        None
    else
        let parts = targetId.Split([| "::" |], StringSplitOptions.None)

        if parts.Length < 3 then
            None
        else
            Some
                {
                    CandidateId = clean parts.[0]
                    AtomKind = clean parts.[1]
                    AtomValue = parts.[2..] |> String.concat "::" |> clean
                }

let private canonicalAtomKind atomKind =
    parsedExposureAtomKinds
    |> List.tryFind (fun knownKind -> normalizeBasisItemKeyPart knownKind = normalizeBasisItemKeyPart atomKind)
    |> Option.defaultValue atomKind

let private tryFindPhiStatement sourcePhiId (model: Model) =
    model.parsedPhis
    |> List.tryPick (fun parse ->
        if equalsText parse.PhiId sourcePhiId then
            Some parse.Statement
        else
            None)
    |> Option.orElseWith (fun () ->
        model.ingestedPhis
        |> List.tryPick (fun phi ->
            if equalsText phi.PhiId sourcePhiId then
                Some phi.RawStatement
            else
                None))

let private formatTriggerSource (mark: ReviewNeededMark) (model: Model) =
    let source =
        if hasText mark.SourcePhiId then
            "Phi " + mark.SourcePhiId
        else
            "Source unavailable"

    match tryFindPhiStatement mark.SourcePhiId model with
    | Some statement when hasText statement ->
        mark.Trigger + " | " + source + ": " + compact 90 statement
    | _ ->
        mark.Trigger + " | " + source

let private formatReviewTarget (mark: ReviewNeededMark) =
    mark.TargetKind + " " + mark.TargetId

let private formatSourceInterpretation atomKind atomValue (mark: ReviewNeededMark) (model: Model) =
    [
        if hasText atomKind || hasText atomValue then
            let label =
                if hasText atomKind then
                    formatModelFittingAtomKindLabel (canonicalAtomKind atomKind)
                else
                    "Source interpretation"

            yield label + ": " + atomValue

        if hasText mark.SourcePhiId then
            yield "Phi " + mark.SourcePhiId

        match tryFindPhiStatement mark.SourcePhiId model with
        | Some statement when hasText statement -> yield compact 110 statement
        | _ -> ()
    ]
    |> String.concat " | "

let private getActiveSequencedParsedPhis (model: Model) =
    model.parsedPhis
    |> getIncludedSequencedParsedPhis model.excludedPhiIds
    |> applyParsedAtomRetirementsToSequencedPhis model.LedgerEvents

let private tryFindCandidate (candidateId: string) (candidates: CandidateDelta list) : CandidateDelta option =
    candidates
    |> List.tryFind (fun candidate -> equalsText candidate.CandidateId candidateId)

let private tryFindCandidateDecision (candidateId: string) (model: Model) : CandidateDecision option =
    model.candidateDecisions
    |> List.tryFind (fun decision -> equalsText decision.CandidateId candidateId)

let private formatCandidateLabel (candidate: CandidateDelta) =
    formatCandidateDeltaKind candidate.Kind + " / " + formatModelFittingAtomKindLabel candidate.Target

let private formatCandidateStatus (candidate: CandidateDelta) sequencedParsedPhis (model: Model) =
    let governance =
        buildCandidateGroupGovernance
            candidate
            model.candidateDecisions
            model.sigmaBasisItemDecisions
            sequencedParsedPhis

    "Group "
    + formatCandidateGroupStatus governance.Status
    + "; class "
    + formatSigmaBasisItemDecisionValue governance.ClassDecision
    + "; "
    + reviewNeededLabel

let private formatMissingCandidateStatus candidateId (model: Model) =
    match tryFindCandidateDecision candidateId model with
    | Some decision ->
        "Class "
        + formatSigmaBasisItemDecisionValue decision.Decision
        + "; current group not present; "
        + reviewNeededLabel
    | None ->
        "Current group not present; " + reviewNeededLabel

let private formatBasisStatus basisItemKey candidateStatusText (model: Model) =
    let basisDecision =
        getSigmaBasisItemDecisionValue basisItemKey model.sigmaBasisItemDecisions

    "Basis "
    + formatSigmaBasisItemDecisionValue basisDecision
    + "; "
    + candidateStatusText

let private formatNodeLabel (node: RealizationNavigationNode) =
    let label = formatRealizationNavigationNodeLabel node

    if node.ObjectKind = realizationSourceKindHost || node.ObjectKind = realizationSourceKindFunction then
        node.ObjectKind + ": " + label
    else
        node.ObjectKind + " " + label

let rec private collectTreeObjectLabels includeRoot (node: RealizationNavigationNode) =
    [
        if includeRoot && node.ObjectKind <> realizationSourceKindHost && node.ObjectKind <> realizationSourceKindFunction then
            yield formatNodeLabel node

        for child in node.Children do
            yield! collectTreeObjectLabels true child
    ]

let rec private collectTreePaths prefix (node: RealizationNavigationNode) =
    let path = prefix @ [ formatNodeLabel node ]

    match node.Children with
    | [] -> [ path ]
    | children ->
        children
        |> List.collect (collectTreePaths path)

let private formatGap (gap: RealizationNavigationGap) =
    let owner =
        if hasText gap.OwnerName then
            gap.OwnerKind + " " + gap.OwnerId + " - " + gap.OwnerName
        else
            gap.OwnerKind + " " + gap.OwnerId

    "Missing " + gap.MissingKind + " after " + owner

let private getRealizationProjectionData includeRootAsObject objectKind objectId (model: Model) =
    let topology = getTargetTopology objectKind objectId model
    let paths =
        topology.DownstreamTree
        |> collectTreePaths []
        |> List.map (String.concat " -> ")
        |> distinctText

    {
        Objects =
            topology.DownstreamTree
            |> collectTreeObjectLabels includeRootAsObject
            |> distinctText
        Paths = paths
        Missing =
            topology.MissingGaps
            |> List.map formatGap
            |> distinctText
    }

let private mergeRealizationProjectionData values =
    {
        Objects = values |> List.collect (fun value -> value.Objects) |> distinctText
        Paths = values |> List.collect (fun value -> value.Paths) |> distinctText
        Missing = values |> List.collect (fun value -> value.Missing) |> distinctText
    }

let private getSourceRealizationProjectionData atomKind atomValue model =
    if equalsText atomKind "Host" then
        getRealizationProjectionData false realizationSourceKindHost atomValue model
    elif equalsText atomKind "Function" then
        getRealizationProjectionData false realizationSourceKindFunction atomValue model
    else
        emptyRealizationProjectionData

let private tryGetRealizationLinkTargetKind linkKind =
    if equalsText linkKind realizationLinkKindHostToPart then
        Some realizationObjectKindPart
    elif equalsText linkKind realizationLinkKindFunctionToFR then
        Some realizationObjectKindFR
    elif equalsText linkKind realizationLinkKindFRToDP || equalsText linkKind realizationLinkKindPartToDP then
        Some realizationObjectKindDP
    elif equalsText linkKind realizationLinkKindDPToTF then
        Some realizationObjectKindTF
    elif equalsText linkKind realizationLinkKindTFToCTQ then
        Some realizationObjectKindCTQ
    elif equalsText linkKind realizationLinkKindCTQToVV then
        Some realizationObjectKindVV
    elif equalsText linkKind realizationLinkKindDPToPart then
        Some realizationObjectKindPart
    else
        None

let private buildCandidateGovernanceRow model sequencedParsedPhis (candidates: CandidateDelta list) (mark: ReviewNeededMark) =
    let candidate = tryFindCandidate mark.TargetId candidates
    let basisItems =
        candidate
        |> Option.map (fun value ->
            buildSigmaBasisItemReviews value sequencedParsedPhis
            |> List.filter (fun basisItem ->
                basisItem.SupportingPhiIds
                |> List.exists (equalsText mark.SourcePhiId)))
        |> Option.defaultValue []

    let realizationData =
        basisItems
        |> List.map (fun basisItem -> getSourceRealizationProjectionData basisItem.Kind basisItem.AtomValue model)
        |> mergeRealizationProjectionData

    let status =
        match candidate with
        | Some value -> formatCandidateStatus value sequencedParsedPhis model
        | None -> formatMissingCandidateStatus mark.TargetId model

    {
        Scope = GovernanceImpact
        RowId = "governance|" + mark.TargetKind + "|" + mark.TargetId
        TriggerSource = formatTriggerSource mark model
        ReviewNeededTarget = formatReviewTarget mark
        Explanation = "Source interpretation changed"
        CandidateGroupIds = [ mark.TargetId ] |> distinctText
        CandidateGroupLabel = candidate |> Option.map formatCandidateLabel
        BasisItemKeys = basisItems |> List.map (fun basisItem -> basisItem.Key) |> distinctText
        SupportingPhiId = if hasText mark.SourcePhiId then Some mark.SourcePhiId else None
        SourceInterpretation = formatSourceInterpretation "" "" mark model
        AffectedDownstreamObjects = realizationData.Objects
        AffectedPaths = realizationData.Paths
        MissingDownstreamLevels = realizationData.Missing
        CurrentDecisionStatus = status
    }

let private buildBasisGovernanceRow model sequencedParsedPhis (candidates: CandidateDelta list) basisContexts (mark: ReviewNeededMark) =
    let currentContext =
        basisContexts
        |> List.tryFind (fun context -> equalsText context.BasisItem.Key mark.TargetId)

    let parsedTarget = tryParseBasisTarget mark.TargetId
    let candidate =
        match currentContext, parsedTarget with
        | Some context, _ -> Some context.Candidate
        | None, Some parsed -> tryFindCandidate parsed.CandidateId candidates
        | None, None -> None

    let atomKind =
        match currentContext, parsedTarget with
        | Some context, _ -> context.BasisItem.Kind
        | None, Some parsed -> canonicalAtomKind parsed.AtomKind
        | None, None -> ""

    let atomValue =
        match currentContext, parsedTarget with
        | Some context, _ -> context.BasisItem.AtomValue
        | None, Some parsed -> parsed.AtomValue
        | None, None -> ""

    let candidateIds =
        [
            match candidate with
            | Some value -> yield value.CandidateId
            | None -> ()

            match parsedTarget with
            | Some parsed -> yield parsed.CandidateId
            | None -> ()
        ]
        |> distinctText

    let candidateStatus =
        match candidate with
        | Some value -> formatCandidateStatus value sequencedParsedPhis model
        | None ->
            match candidateIds with
            | candidateId :: _ -> formatMissingCandidateStatus candidateId model
            | [] -> "Current group not present; " + reviewNeededLabel

    let realizationData = getSourceRealizationProjectionData atomKind atomValue model

    {
        Scope = GovernanceImpact
        RowId = "basis|" + mark.TargetKind + "|" + mark.TargetId
        TriggerSource = formatTriggerSource mark model
        ReviewNeededTarget = formatReviewTarget mark
        Explanation = "Basis decision depends on amended item"
        CandidateGroupIds = candidateIds
        CandidateGroupLabel = candidate |> Option.map formatCandidateLabel
        BasisItemKeys = [ mark.TargetId ]
        SupportingPhiId = if hasText mark.SourcePhiId then Some mark.SourcePhiId else None
        SourceInterpretation = formatSourceInterpretation atomKind atomValue mark model
        AffectedDownstreamObjects = realizationData.Objects
        AffectedPaths = realizationData.Paths
        MissingDownstreamLevels = realizationData.Missing
        CurrentDecisionStatus = formatBasisStatus mark.TargetId candidateStatus model
    }

let private buildGovernanceRows model sequencedParsedPhis (candidates: CandidateDelta list) basisContexts =
    model.reviewNeededMarks
    |> List.choose (fun mark ->
        if mark.TargetKind = reviewTargetKindCandidateDecision then
            Some (buildCandidateGovernanceRow model sequencedParsedPhis candidates mark)
        elif mark.TargetKind = reviewTargetKindSigmaBasisItemDecision then
            Some (buildBasisGovernanceRow model sequencedParsedPhis candidates basisContexts mark)
        else
            None)
    |> List.distinctBy (fun row -> row.RowId)

let private buildRealizationPathRow model (mark: ReviewNeededMark) sourceKind sourceValue =
    let realizationData = getRealizationProjectionData false sourceKind sourceValue model
    let status =
        if equalsText sourceKind realizationSourceKindHost then
            getHostRealizationStatus sourceValue model.realizationState + "; " + reviewNeededLabel
        elif equalsText sourceKind realizationSourceKindFunction then
            let state =
                if getFrIdsForFunction sourceValue model.realizationState |> List.isEmpty then
                    "Not realized"
                else
                    "Realization started"

            state + "; " + reviewNeededLabel
        else
            reviewNeededLabel

    {
        Scope = RealizationImpact
        RowId = "realization-path|" + mark.TargetKind + "|" + mark.TargetId
        TriggerSource = formatTriggerSource mark model
        ReviewNeededTarget = formatReviewTarget mark
        Explanation = "Realization path depends on amended " + sourceKind
        CandidateGroupIds = []
        CandidateGroupLabel = None
        BasisItemKeys = []
        SupportingPhiId = if hasText mark.SourcePhiId then Some mark.SourcePhiId else None
        SourceInterpretation = formatSourceInterpretation sourceKind sourceValue mark model
        AffectedDownstreamObjects = realizationData.Objects
        AffectedPaths = realizationData.Paths
        MissingDownstreamLevels = realizationData.Missing
        CurrentDecisionStatus = status
    }

let private buildRealizationObjectRow model (mark: ReviewNeededMark) objectKind objectId =
    let realizationData = getRealizationProjectionData true objectKind objectId model
    let readiness =
        getRealizationObjectReadiness objectKind objectId model.realizationState
        |> getReadinessLabel

    {
        Scope = RealizationImpact
        RowId = "realization-object|" + mark.TargetKind + "|" + mark.TargetId
        TriggerSource = formatTriggerSource mark model
        ReviewNeededTarget = formatReviewTarget mark
        Explanation = "Realization path depends on amended source"
        CandidateGroupIds = []
        CandidateGroupLabel = None
        BasisItemKeys = []
        SupportingPhiId = if hasText mark.SourcePhiId then Some mark.SourcePhiId else None
        SourceInterpretation = formatSourceInterpretation objectKind objectId mark model
        AffectedDownstreamObjects = realizationData.Objects
        AffectedPaths = realizationData.Paths
        MissingDownstreamLevels = realizationData.Missing
        CurrentDecisionStatus = "Object " + readiness + "; " + reviewNeededLabel
    }

let private buildRealizationLinkRow model (mark: ReviewNeededMark) linkKind sourceId targetId =
    let realizationData =
        match tryGetRealizationLinkTargetKind linkKind with
        | Some targetKind ->
            let data = getRealizationProjectionData true targetKind targetId model
            let linkStart = linkKind + ": " + sourceId + " -> " + targetId

            { data with
                Paths =
                    match data.Paths with
                    | [] -> [ linkStart ]
                    | paths -> paths |> List.map (fun path -> linkStart + " -> " + path) }
        | None ->
            emptyRealizationProjectionData

    {
        Scope = RealizationImpact
        RowId = "realization-link|" + mark.TargetKind + "|" + mark.TargetId
        TriggerSource = formatTriggerSource mark model
        ReviewNeededTarget = formatReviewTarget mark
        Explanation = "Realization path depends on amended source"
        CandidateGroupIds = []
        CandidateGroupLabel = None
        BasisItemKeys = []
        SupportingPhiId = if hasText mark.SourcePhiId then Some mark.SourcePhiId else None
        SourceInterpretation = formatSourceInterpretation linkKind (sourceId + " -> " + targetId) mark model
        AffectedDownstreamObjects = realizationData.Objects
        AffectedPaths = realizationData.Paths |> distinctText
        MissingDownstreamLevels = realizationData.Missing
        CurrentDecisionStatus = "Link " + reviewNeededLabel
    }

let private buildRealizationRows model =
    model.reviewNeededMarks
    |> List.choose (fun mark ->
        match mark.TargetKind, splitReviewTargetId mark.TargetId with
        | targetKind, [ sourceKind; sourceValue ] when targetKind = reviewTargetKindRealizationPath ->
            Some (buildRealizationPathRow model mark sourceKind sourceValue)
        | targetKind, [ objectKind; objectId ] when targetKind = reviewTargetKindRealizationObject ->
            Some (buildRealizationObjectRow model mark objectKind objectId)
        | targetKind, [ linkKind; sourceId; targetId ] when targetKind = reviewTargetKindRealizationLink ->
            Some (buildRealizationLinkRow model mark linkKind sourceId targetId)
        | _ ->
            None)
    |> List.distinctBy (fun row -> row.RowId)

let buildImpactProjection (model: Model) =
    let sequencedParsedPhis = getActiveSequencedParsedPhis model
    let candidates = getCurrentCandidateDeltas model
    let basisContexts = getCurrentSigmaBasisItemLedgerContexts model
    let governanceRows = buildGovernanceRows model sequencedParsedPhis candidates basisContexts
    let realizationRows = buildRealizationRows model
    let allRows = governanceRows @ realizationRows

    {
        TotalReviewNeededMarks = List.length model.reviewNeededMarks
        AffectedCandidateGroups =
            allRows
            |> List.collect (fun row -> row.CandidateGroupIds)
            |> distinctText
            |> List.length
        AffectedBasisItems =
            allRows
            |> List.collect (fun row -> row.BasisItemKeys)
            |> distinctText
            |> List.length
        AffectedRealizationPaths =
            allRows
            |> List.collect (fun row -> row.AffectedPaths)
            |> distinctText
            |> List.length
        AffectedRealizationObjects =
            allRows
            |> List.collect (fun row -> row.AffectedDownstreamObjects)
            |> distinctText
            |> List.length
        GovernanceRows = governanceRows
        RealizationRows = realizationRows
    }

let private renderProjectionCount label count =
    span {
        attr.``class`` "tag is-light"
        text (label + ": " + string count)
    }

let private renderLimitedTags emptyText values =
    match values with
    | [] ->
        span {
            attr.``class`` "has-text-grey"
            text emptyText
        }
    | items ->
        let visible = items |> List.truncate 3
        let remaining = List.length items - List.length visible

        div {
            attr.``class`` "tags mb-0"

            forEach visible <| fun value ->
                span {
                    attr.``class`` "tag is-light"
                    text value
                }

            if remaining > 0 then
                span {
                    attr.``class`` "tag is-light"
                    text ("+" + string remaining + " more")
                }
        }

let private renderLimitedLines emptyText values =
    match values with
    | [] ->
        span {
            attr.``class`` "has-text-grey"
            text emptyText
        }
    | items ->
        let visible = items |> List.truncate 3
        let remaining = List.length items - List.length visible

        div {
            forEach visible <| fun value ->
                p {
                    attr.``class`` "is-size-7 mb-1"
                    text value
                }

            if remaining > 0 then
                span {
                    attr.``class`` "tag is-light"
                    text ("+" + string remaining + " more")
                }
        }

let private renderProjectionRow (row: ImpactProjectionRow) =
    tr {
        td {
            p {
                attr.``class`` "mb-1"
                text row.TriggerSource
            }
            p {
                attr.``class`` "is-size-7 has-text-grey mb-0"
                text row.SourceInterpretation
            }
        }
        td {
            p {
                attr.``class`` "mb-1"
                code { text row.ReviewNeededTarget }
            }

            match row.CandidateGroupLabel with
            | Some label ->
                p {
                    attr.``class`` "is-size-7 has-text-grey mb-0"
                    text label
                }
            | None -> empty()

            p {
                attr.``class`` "is-size-7 has-text-grey mb-0"
                text row.Explanation
            }
        }
        td {
            renderLimitedTags "None projected" row.AffectedDownstreamObjects

            if not (List.isEmpty row.MissingDownstreamLevels) then
                div {
                    attr.``class`` "is-size-7 has-text-warning-dark mt-1"
                    text ("Missing: " + (row.MissingDownstreamLevels |> List.truncate 2 |> String.concat "; "))
                }
        }
        td {
            renderLimitedLines "No path projected" row.AffectedPaths
        }
        td {
            span {
                attr.``class`` "tag is-warning is-light"
                text reviewNeededLabel
            }
            p {
                attr.``class`` "is-size-7 mt-1 mb-0"
                text row.CurrentDecisionStatus
            }
        }
    }

let renderImpactProjectionSection emptyText (projection: ImpactProjection) rows =
    div {
        attr.``class`` "notification is-warning is-light impact-projection-section"

        h3 {
            attr.``class`` "title is-6 mb-2"
            text "Impact Projection"
        }

        div {
            attr.``class`` "tags mb-3"
            renderProjectionCount "Review Needed marks" projection.TotalReviewNeededMarks
            renderProjectionCount "Affected candidate groups" projection.AffectedCandidateGroups
            renderProjectionCount "Affected basis items" projection.AffectedBasisItems
            renderProjectionCount "Affected realization paths" projection.AffectedRealizationPaths
            renderProjectionCount "Affected realization objects" projection.AffectedRealizationObjects
        }

        match rows with
        | [] ->
            p {
                attr.``class`` "has-text-grey mb-0"
                text emptyText
            }
        | values ->
            div {
                attr.``class`` "table-container"
                table {
                    attr.``class`` "table is-fullwidth is-striped is-narrow"

                    thead {
                        tr {
                            th { text "Trigger/source" }
                            th { text "Review-needed target" }
                            th { text "Affected downstream objects" }
                            th { text "Affected path" }
                            th { text "Current decision/status" }
                        }
                    }

                    tbody {
                        forEach values <| fun row ->
                            renderProjectionRow row
                    }
                }
            }
    }
