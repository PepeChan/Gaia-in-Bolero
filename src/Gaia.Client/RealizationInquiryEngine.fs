module Gaia.Client.RealizationInquiryEngine

open System
open Gaia.Core
open Gaia.Client.AppState
open Gaia.Client.Realization
open Gaia.Client.Types

type RealizationInquiryQuestion =
    | WhyDoesThisExist
    | WhatDependsOnThis
    | WhatIsMissing
    | ShowRealizationPath

type RealizationInquiryNode =
    {
        ObjectKind: string
        ObjectId: string
        Label: string
        Readiness: ReadinessState
        DetailLines: string list
    }

type RealizationInquiryResult =
    {
        Question: RealizationInquiryQuestion
        Target: RealizationInquiryNode
        Summary: string
        AnswerLines: string list
        RelatedNodes: RealizationInquiryNode list
        MissingGaps: RealizationNavigationGap list
        RecommendedNextSteps: string list
        PathLines: string list
    }

let defaultRealizationInquiryQuestion = WhyDoesThisExist

let encodeRealizationInquiryQuestion = function
    | WhyDoesThisExist -> "WhyDoesThisExist"
    | WhatDependsOnThis -> "WhatDependsOnThis"
    | WhatIsMissing -> "WhatIsMissing"
    | ShowRealizationPath -> "ShowRealizationPath"

let formatRealizationInquiryQuestion = function
    | WhyDoesThisExist -> "Why does this exist?"
    | WhatDependsOnThis -> "What depends on this?"
    | WhatIsMissing -> "What is missing?"
    | ShowRealizationPath -> "Show realization path"

let realizationInquiryQuestionOptions =
    [
        WhyDoesThisExist
        WhatDependsOnThis
        WhatIsMissing
        ShowRealizationPath
    ]
    |> List.map (fun question -> encodeRealizationInquiryQuestion question, formatRealizationInquiryQuestion question)

let tryDecodeRealizationInquiryQuestion = function
    | "WhyDoesThisExist" -> Some WhyDoesThisExist
    | "WhatDependsOnThis" -> Some WhatDependsOnThis
    | "WhatIsMissing" -> Some WhatIsMissing
    | "ShowRealizationPath" -> Some ShowRealizationPath
    | _ -> None

let getRealizationInquiryQuestionOrDefault value =
    tryDecodeRealizationInquiryQuestion value
    |> Option.defaultValue defaultRealizationInquiryQuestion

let getRealizationInquiryQuestionKeyOrDefault value =
    value
    |> getRealizationInquiryQuestionOrDefault
    |> encodeRealizationInquiryQuestion

let private nodeFromNavigationNode (node: RealizationNavigationNode) =
    {
        ObjectKind = node.ObjectKind
        ObjectId = node.ObjectId
        Label = formatRealizationNavigationNodeLabel node
        Readiness = node.Readiness
        DetailLines = node.DetailLines
    }

let private formatNodeReference (node: RealizationInquiryNode) =
    node.ObjectKind + " " + node.Label

let private formatReadiness readiness =
    getReadinessLabel readiness

let private formatPath (nodes: RealizationNavigationNode list) =
    nodes
    |> List.map formatRealizationNavigationNodeLabel
    |> String.concat " -> "

let private flattenDownstreamNodes (root: RealizationNavigationNode) : RealizationInquiryNode list =
    let rec collect (node: RealizationNavigationNode) : RealizationInquiryNode list =
        [
            yield! node.Children |> List.map nodeFromNavigationNode
            yield! node.Children |> List.collect collect
        ]

    collect root

let private downstreamPathLines (root: RealizationNavigationNode) : string list =
    let rec collect prefix (node: RealizationNavigationNode) : string list =
        let label = formatRealizationNavigationNodeLabel node
        let path = prefix @ [ label ]

        [
            if List.length path > 1 then
                String.concat " -> " path
            yield! node.Children |> List.collect (collect path)
        ]

    collect [] root

let private formatMissingGap (gap: RealizationNavigationGap) =
    let ownerLabel =
        formatRealizationNavigationObjectLabel gap.OwnerKind gap.OwnerId gap.OwnerName

    ownerLabel + " is missing " + gap.MissingKind + "."

let private formatRecommendedNextStepForGap (gap: RealizationNavigationGap) =
    let ownerLabel =
        formatRealizationNavigationObjectLabel gap.OwnerKind gap.OwnerId gap.OwnerName

    "Identify or create " + gap.MissingKind + " for " + ownerLabel + "."

let private normalizeKeyText (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        ""
    else
        value.Trim().ToLowerInvariant().Split([| ' '; '\t'; '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
        |> String.concat " "

let private buildRealizationInquiryTargetKey (result: RealizationInquiryResult) =
    normalizeKeyText result.Target.ObjectKind + "|" + normalizeKeyText result.Target.ObjectId

let private buildGapKeyPart (gap: RealizationNavigationGap) =
    [
        gap.OwnerKind
        gap.OwnerId
        gap.MissingKind
        String.concat " -> " gap.PathLabels
    ]
    |> List.map normalizeKeyText
    |> String.concat "|"

let private buildRealizationInquiryGapKey (result: RealizationInquiryResult) =
    let parts =
        if List.isEmpty result.MissingGaps then
            result.RecommendedNextSteps
            |> List.map normalizeKeyText
        else
            result.MissingGaps
            |> List.map buildGapKeyPart

    parts
    |> List.filter (fun value -> value <> "")
    |> List.sort
    |> String.concat "||"

let private affectedPathCount (gaps: RealizationNavigationGap list) =
    let count =
        gaps
        |> List.map (fun gap -> gap.PathLabels |> List.map normalizeKeyText |> String.concat " -> ")
        |> List.filter (fun path -> path <> "")
        |> List.distinct
        |> List.length

    if count = 0 && not (List.isEmpty gaps) then
        List.length gaps
    else
        count

let private formatMissingKindBreakdown (gaps: RealizationNavigationGap list) =
    let parts =
        gaps
        |> List.countBy (fun gap -> gap.MissingKind)
        |> List.sortBy fst
        |> List.map (fun (missingKind, count) -> missingKind + " " + string count)

    match parts with
    | [] -> None
    | values -> Some ("Missing items: " + String.concat ", " values)

let private formatMissingSummary target gaps =
    if List.isEmpty gaps then
        formatNodeReference target + " has no missing next realization link in the current projection."
    else
        formatNodeReference target
        + " has "
        + string (List.length gaps)
        + " missing item(s) across "
        + string (affectedPathCount gaps)
        + " affected realization path(s)."

let private normalizeContextKind (value: string) =
    let normalized = normalizeKeyText value

    normalized.Replace(" ", "").Replace("-", "").Replace("_", "")

let private equalsContextKind (left: string) (right: string) =
    normalizeContextKind left = normalizeContextKind right

let private tryFindPhiContextValue phiId kind (model: Model) =
    model.phiContextEntries
    |> List.tryFind (fun entry -> entry.PhiId = phiId && equalsContextKind entry.Kind kind && not (String.IsNullOrWhiteSpace(entry.Value)))
    |> Option.map (fun entry -> entry.Value.Trim())

let private containsText (needle: string) (value: string) =
    not (String.IsNullOrWhiteSpace(needle))
    && not (String.IsNullOrWhiteSpace(value))
    && value.IndexOf(needle, StringComparison.OrdinalIgnoreCase) >= 0

let private containsDerivedInquiryTag (value: string) =
    containsText derivedInquiryTag value

let private isT6RealizationInquiryPhi (phi: PhiIntake) =
    String.Equals(phi.Source, t6RealizationInquirySource, StringComparison.OrdinalIgnoreCase)
    || containsDerivedInquiryTag phi.TypeText

let private upstreamPathLines (topology: RealizationTargetTopology) : string list =
    topology.UpstreamPaths
    |> List.filter (fun path -> List.length path > 1)
    |> List.map formatPath

let private buildWhyDoesThisExist (topology: RealizationTargetTopology) : RealizationInquiryResult =
    let target = nodeFromNavigationNode topology.Target
    let upstreamLines = upstreamPathLines topology

    let answerLines =
        [
            "Readiness: " + formatReadiness target.Readiness + "."
            yield! target.DetailLines
            if List.isEmpty upstreamLines then
                "No upstream realization parent is linked."
            else
                "Upstream realization paths: " + string (List.length upstreamLines) + "."
        ]

    {
        Question = WhyDoesThisExist
        Target = target
        Summary = formatNodeReference target + " exists in the current realization navigation projection."
        AnswerLines = answerLines
        RelatedNodes =
            topology.UpstreamPaths
            |> List.collect id
            |> List.filter (fun node -> node.ObjectKind <> topology.Target.ObjectKind || node.ObjectId <> topology.Target.ObjectId)
            |> List.map nodeFromNavigationNode
        MissingGaps = []
        RecommendedNextSteps = []
        PathLines = upstreamLines
    }

let private buildWhatDependsOnThis (topology: RealizationTargetTopology) : RealizationInquiryResult =
    let target = nodeFromNavigationNode topology.Target
    let downstreamNodes = flattenDownstreamNodes topology.DownstreamTree
    let downstreamPaths = downstreamPathLines topology.DownstreamTree

    let summary =
        if List.isEmpty downstreamNodes then
            "No downstream realization nodes are linked from " + formatNodeReference target + "."
        else
            string (List.length downstreamNodes) + " downstream realization node(s) are linked from " + formatNodeReference target + "."

    let answerLines =
        if List.isEmpty downstreamNodes then
            [
                match topology.DownstreamTree.MissingNextKind with
                | Some missingKind -> "The next expected downstream link is " + missingKind + "."
                | None -> "No downstream realization dependency is currently available."
            ]
        else
            downstreamNodes
            |> List.map formatNodeReference

    let recommendedNextSteps =
        if List.isEmpty downstreamNodes then
            match topology.DownstreamTree.MissingNextKind with
            | Some missingKind -> [ "Identify or create the next " + missingKind + " for " + formatNodeReference target + "." ]
            | None -> []
        else
            []

    {
        Question = WhatDependsOnThis
        Target = target
        Summary = summary
        AnswerLines = answerLines
        RelatedNodes = downstreamNodes
        MissingGaps = []
        RecommendedNextSteps = recommendedNextSteps
        PathLines = downstreamPaths
    }

let private buildWhatIsMissing (topology: RealizationTargetTopology) : RealizationInquiryResult =
    let target = nodeFromNavigationNode topology.Target
    let missingLines =
        topology.MissingGaps
        |> List.map formatMissingGap

    let recommendedNextSteps =
        topology.MissingGaps
        |> List.map formatRecommendedNextStepForGap

    let answerLines =
        if List.isEmpty missingLines then
            [ "No missing realization gaps found." ]
        else
            [
                match formatMissingKindBreakdown topology.MissingGaps with
                | Some breakdown -> yield breakdown
                | None -> ()
                yield! missingLines
            ]

    {
        Question = WhatIsMissing
        Target = target
        Summary = formatMissingSummary target topology.MissingGaps
        AnswerLines = answerLines
        RelatedNodes = []
        MissingGaps = topology.MissingGaps
        RecommendedNextSteps = recommendedNextSteps
        PathLines =
            topology.MissingGaps
            |> List.map (fun gap -> String.concat " -> " gap.PathLabels)
    }

let private buildShowRealizationPath (topology: RealizationTargetTopology) : RealizationInquiryResult =
    let target = nodeFromNavigationNode topology.Target
    let upstreamLines = upstreamPathLines topology
    let downstreamLines = downstreamPathLines topology.DownstreamTree
    let pathLines = upstreamLines @ downstreamLines

    let answerLines =
        if List.isEmpty pathLines then
            [ "No realization path is linked beyond the selected target." ]
        else
            pathLines

    {
        Question = ShowRealizationPath
        Target = target
        Summary = "Realization path projection for " + formatNodeReference target + "."
        AnswerLines = answerLines
        RelatedNodes = flattenDownstreamNodes topology.DownstreamTree
        MissingGaps = topology.MissingGaps
        RecommendedNextSteps =
            topology.MissingGaps
            |> List.map formatRecommendedNextStepForGap
        PathLines = pathLines
    }

let canConvertRealizationInquiryToIntake (result: RealizationInquiryResult) =
    not (List.isEmpty result.MissingGaps)
    || not (List.isEmpty result.RecommendedNextSteps)

let private formatDraftList prefix values =
    values
    |> List.map (fun value -> prefix + value)

let private getMissingKnowledgeLines (result: RealizationInquiryResult) =
    result.MissingGaps
    |> List.map formatMissingGap

let private getDraftTags (result: RealizationInquiryResult) =
    [
        "realization"
        "missing-knowledge"
        derivedInquiryTag
        yield! result.MissingGaps |> List.map (fun gap -> gap.MissingKind.ToLowerInvariant())
        if List.isEmpty result.MissingGaps && not (List.isEmpty result.RecommendedNextSteps) then
            "recommended-next-step"
    ]
    |> List.distinct
    |> String.concat ", "

let buildPhiDraftFromRealizationInquiry (result: RealizationInquiryResult) : PhiDraftPrefill =
    let missingKnowledgeLines = getMissingKnowledgeLines result
    let targetLine = "Target: " + result.Target.ObjectKind + " " + result.Target.Label + "."
    let targetKey = buildRealizationInquiryTargetKey result
    let gapKey = buildRealizationInquiryGapKey result

    let rawLines =
        [
            "Realization inquiry found a knowledge gap."
            targetLine
            result.Summary
            yield! formatDraftList "Missing knowledge: " missingKnowledgeLines
            yield! formatDraftList "Recommended next step: " result.RecommendedNextSteps
        ]
        |> List.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))

    let contextLines =
        [
            derivedInquiryContextKind + "=true"
            t6InquiryTargetContextKind + "=" + targetKey
            t6InquiryGapKeyContextKind + "=" + gapKey
            t6InquiryQuestionContextKind + "=" + formatRealizationInquiryQuestion result.Question
            "note=Created from T6 realization inquiry: " + formatRealizationInquiryQuestion result.Question
            "note=" + result.Summary
            yield! missingKnowledgeLines |> List.map (fun line -> "gap=" + line)
            yield! result.RecommendedNextSteps |> List.map (fun line -> "next-step=" + line)
        ]

    {
        RawStatement = String.concat Environment.NewLine rawLines
        TriggerContext = "T6 inquiry exposed missing realization knowledge for " + result.Target.Label + "."
        Source = t6RealizationInquirySource
        QuickTags = getDraftTags result
        Confidence = "Medium"
        ContextSnip = String.concat Environment.NewLine contextLines
        StatusMessage = "Draft investigation created from T6 inquiry. Review and edit before ingestion."
    }

let hasDuplicateRealizationInquiryIntake (result: RealizationInquiryResult) (model: Model) =
    let targetKey = buildRealizationInquiryTargetKey result
    let gapKey = buildRealizationInquiryGapKey result
    let targetLine = "Target: " + result.Target.ObjectKind + " " + result.Target.Label + "."
    let duplicateContentLines =
        [
            targetLine
            yield! getMissingKnowledgeLines result
            yield! result.RecommendedNextSteps
        ]
        |> List.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))

    model.ingestedPhis
    |> List.exists (fun phi ->
        if List.isEmpty duplicateContentLines || not (isT6RealizationInquiryPhi phi) then
            false
        else
            let markerMatch =
                match tryFindPhiContextValue phi.PhiId t6InquiryTargetContextKind model,
                      tryFindPhiContextValue phi.PhiId t6InquiryGapKeyContextKind model with
                | Some existingTargetKey, Some existingGapKey ->
                    normalizeKeyText existingTargetKey = targetKey
                    && normalizeKeyText existingGapKey = gapKey
                | _ -> false

            let fallbackTextMatch =
                duplicateContentLines
                |> List.forall (fun line -> containsText line phi.RawStatement)

            markerMatch || fallbackTextMatch)

let resolveRealizationInquiryForTopology question (topology: RealizationTargetTopology) : RealizationInquiryResult =
    match question with
    | WhyDoesThisExist -> buildWhyDoesThisExist topology
    | WhatDependsOnThis -> buildWhatDependsOnThis topology
    | WhatIsMissing -> buildWhatIsMissing topology
    | ShowRealizationPath -> buildShowRealizationPath topology

let resolveRealizationInquiry question objectKind objectId (model: Model) =
    getTargetTopology objectKind objectId model
    |> resolveRealizationInquiryForTopology question

let resolveRealizationInquiryForTarget question (target: RealizationNavigationTarget) (model: Model) =
    resolveRealizationInquiry question target.ObjectKind target.ObjectId model

let tryResolveRealizationInquiry question selectedTargetValue (model: Model) =
    tryFindRealizationNavigationTarget selectedTargetValue model
    |> Option.map (fun target -> resolveRealizationInquiryForTarget question target model)
