module Gaia.Client.RealizationInquiryEngine

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
        PathLines: string list
    }

let formatRealizationInquiryQuestion = function
    | WhyDoesThisExist -> "Why does this exist?"
    | WhatDependsOnThis -> "What depends on this?"
    | WhatIsMissing -> "What is missing?"
    | ShowRealizationPath -> "Show realization path"

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

    {
        Question = WhatDependsOnThis
        Target = target
        Summary = summary
        AnswerLines = answerLines
        RelatedNodes = downstreamNodes
        MissingGaps = []
        PathLines = downstreamPaths
    }

let private buildWhatIsMissing (topology: RealizationTargetTopology) : RealizationInquiryResult =
    let target = nodeFromNavigationNode topology.Target
    let missingLines =
        topology.MissingGaps
        |> List.map formatMissingGap

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
        PathLines = pathLines
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
