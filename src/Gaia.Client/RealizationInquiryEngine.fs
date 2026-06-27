module Gaia.Client.RealizationInquiryEngine

open System
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

    let summary =
        if List.isEmpty topology.MissingGaps then
            formatNodeReference target + " has no missing next realization link in the current projection."
        else
            formatNodeReference target + " has " + string (List.length topology.MissingGaps) + " missing realization gap(s)."

    {
        Question = WhatIsMissing
        Target = target
        Summary = summary
        AnswerLines = if List.isEmpty missingLines then [ "No missing realization gaps found." ] else missingLines
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
        yield! result.MissingGaps |> List.map (fun gap -> gap.MissingKind.ToLowerInvariant())
        if List.isEmpty result.MissingGaps && not (List.isEmpty result.RecommendedNextSteps) then
            "recommended-next-step"
    ]
    |> List.distinct
    |> String.concat ", "

let buildPhiDraftFromRealizationInquiry (result: RealizationInquiryResult) : PhiDraftPrefill =
    let missingKnowledgeLines = getMissingKnowledgeLines result
    let targetLine = "Target: " + result.Target.ObjectKind + " " + result.Target.Label + "."

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
            "note=Created from T6 realization inquiry: " + formatRealizationInquiryQuestion result.Question
            "note=" + result.Summary
            yield! missingKnowledgeLines |> List.map (fun line -> "gap=" + line)
            yield! result.RecommendedNextSteps |> List.map (fun line -> "next-step=" + line)
        ]

    {
        RawStatement = String.concat Environment.NewLine rawLines
        TriggerContext = "T6 inquiry exposed missing realization knowledge for " + result.Target.Label + "."
        Source = "T6 Realization Inquiry"
        QuickTags = getDraftTags result
        Confidence = "Medium"
        ContextSnip = String.concat Environment.NewLine contextLines
        StatusMessage = "Draft Phi created from T6 inquiry. Review and edit before ingestion."
    }

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
