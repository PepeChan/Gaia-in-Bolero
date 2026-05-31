module Gaia.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client
open Gaia.Core

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Probe

/// The Elmish application's model.
type Model =
    {
        page: Page
        error: string option
        selectedScenarioId: string option
        scenarioResolution: ResolutionView option
    }

let demoScenarios = DemoData.demoScenarios

let tryFindScenario scenarioId =
    demoScenarios
    |> List.tryFind (fun scenario -> scenario.Id = scenarioId)

let resolveScenario scenarioId =
    tryFindScenario scenarioId
    |> Option.map (fun scenario -> Engine.resolveParse DemoData.demoSigma scenario.Parse)

let initialScenario =
    demoScenarios
    |> List.tryHead

let initModel =
    let selectedScenarioId =
        initialScenario
        |> Option.map (fun scenario -> scenario.Id)

    let scenarioResolution =
        selectedScenarioId
        |> Option.bind resolveScenario

    {
        page = Probe
        error = None
        selectedScenarioId = selectedScenarioId
        scenarioResolution = scenarioResolution
    }
/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | SelectScenario of string
    | Error of exn
    | ClearError

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none

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

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

type AdmissibilityResult =
    | Admit
    | Hold
    | Reject
    | Escalate

let tryGetSelectedScenario model =
    model.selectedScenarioId
    |> Option.bind tryFindScenario
    |> Option.orElse initialScenario

let getAdmissibilityResult (parse: PhiParse) =
    if parse.OutcomeEscalate then
        Escalate
    elif parse.ResultRejected then
        Reject
    elif parse.OutcomeHold || parse.ResultIndeterminate then
        Hold
    elif parse.ResultValid then
        Admit
    else
        Hold

let formatAdmissibilityResult = function
    | Admit -> "ADMIT"
    | Hold -> "HOLD"
    | Reject -> "REJECT"
    | Escalate -> "ESCALATE"

let admissibilityBadgeClass = function
    | Admit -> "tag is-success is-medium"
    | Hold -> "tag is-warning is-medium"
    | Reject -> "tag is-danger is-medium"
    | Escalate -> "tag is-black is-medium"

let formatDerivationEntry = function
    | Some FromFR -> "From FR"
    | Some FromMode -> "From Mode"
    | Some FromInterface -> "From Interface"
    | Some FromState -> "From State"
    | Some FromParametric -> "From Parametric"
    | Some GammaOnly -> "Gamma Only"
    | None -> "Not resolved"

let mapIdsToNames getId getName items ids =
    ids
    |> List.map (fun id ->
        items
        |> List.tryFind (fun item -> getId item = id)
        |> Option.map getName
        |> Option.defaultValue id)

let renderMatchedGroup title names =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        match names with
        | [] ->
            p {
                attr.``class`` "has-text-grey"
                text "No matches"
            }
        | xs ->
            div {
                attr.``class`` "tags"
                forEach xs <| fun name ->
                    span {
                        attr.``class`` "tag is-info is-light"
                        text name
                    }
            }
    }

let renderSummaryBox title body =
    div {
        attr.``class`` "box"
        h3 {
            attr.``class`` "title is-6"
            text title
        }
        p {
            text body
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

        div {
            attr.``class`` "content"
            h1 {
                attr.``class`` "title"
                text "Gaia Probe Dashboard"
            }
            p {
                attr.``class`` "subtitle is-6"
                text "Probe demo scenarios, resolve them through Gaia.Core, and inspect the resulting path and matches."
            }
            div {
                attr.``class`` "columns is-variable is-5"
                div {
                    attr.``class`` "column is-4"
                    div {
                        attr.``class`` "box"
                        h2 {
                            attr.``class`` "title is-5"
                            text "Scenarios"
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
                            text scenario.Title
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
            menuItem model Probe "Gaia Probe"            
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

    override _.CssScope = CssScopes.MyApp

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.none) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
