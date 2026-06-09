module Gaia.Client.Persistence

open System
open System.Globalization
open System.Text.Json
open System.Text.Json.Nodes
open Gaia.Core
open Gaia.Client.Types

let projectSnapshotVersion = "T6.v0"

exception ProjectJsonReadError of string

let failProjectJson message : 'T =
    raise (ProjectJsonReadError message)

let projectJsonSerializerOptions =
    let options = JsonSerializerOptions()
    options.WriteIndented <- true
    options

let jsonStringArray (values: string list) =
    let array = JsonArray()

    values
    |> List.iter (fun value -> array.Add(JsonValue.Create(value)))

    array

let candidateDecisionValueToJsonText = function
    | Pending -> "Pending"
    | Accepted -> "Accepted"
    | Rejected -> "Rejected"
    | Held -> "Held"

let parseCandidateDecisionValue context value =
    match value with
    | "Pending" -> Pending
    | "Accepted" -> Accepted
    | "Rejected" -> Rejected
    | "Held" -> Held
    | _ -> failProjectJson (context + " has unknown candidate decision value '" + value + "'.")

let derivationEntryToJsonText = function
    | FromFR -> "FromFR"
    | FromMode -> "FromMode"
    | FromInterface -> "FromInterface"
    | FromState -> "FromState"
    | FromParametric -> "FromParametric"
    | GammaOnly -> "GammaOnly"

let parseDerivationEntry context value =
    match value with
    | "FromFR"
    | "From FR" -> FromFR
    | "FromMode"
    | "From Mode" -> FromMode
    | "FromInterface"
    | "From Interface" -> FromInterface
    | "FromState"
    | "From State" -> FromState
    | "FromParametric"
    | "From Parametric" -> FromParametric
    | "GammaOnly"
    | "Gamma Only" -> GammaOnly
    | _ -> failProjectJson (context + " has unknown derivation entry '" + value + "'.")

let phiIntakeToJson (phi: PhiIntake) =
    let json = JsonObject()
    json["PhiId"] <- JsonValue.Create(phi.PhiId)
    json["Date"] <- JsonValue.Create(phi.Date)
    json["Source"] <- JsonValue.Create(phi.Source)
    json["Context"] <- JsonValue.Create(phi.Context)
    json["Confidence"] <- JsonValue.Create(phi.Confidence)
    json["Status"] <- JsonValue.Create(phi.Status)
    json["RawStatement"] <- JsonValue.Create(phi.RawStatement)
    json["Trigger"] <- JsonValue.Create(phi.Trigger)
    json["Claim"] <- JsonValue.Create(phi.Claim)
    json["About"] <- JsonValue.Create(phi.About)
    json["Condition"] <- JsonValue.Create(phi.Condition)
    json["Assumption"] <- JsonValue.Create(phi.Assumption)
    json["TypeText"] <- JsonValue.Create(phi.TypeText)
    json["Impact"] <- JsonValue.Create(phi.Impact)
    json["UnresolvedSignal"] <- JsonValue.Create(phi.UnresolvedSignal)
    json

let exposureToJson (exposure: Exposure) =
    let json = JsonObject()
    json["Function"] <- JsonValue.Create(exposure.Function)
    json["Mode"] <- JsonValue.Create(exposure.Mode)
    json["Interface"] <- JsonValue.Create(exposure.Interface)
    json["State"] <- JsonValue.Create(exposure.State)
    json["HostCandidate"] <- JsonValue.Create(exposure.HostCandidate)
    json

let phiParseToJson (parse: PhiParse) =
    let json = JsonObject()
    json["PhiId"] <- JsonValue.Create(parse.PhiId)
    json["Date"] <- JsonValue.Create(parse.Date)
    json["Statement"] <- JsonValue.Create(parse.Statement)
    json["InScope"] <- JsonValue.Create(parse.InScope)
    json["OutOfScope"] <- JsonValue.Create(parse.OutOfScope)
    json["Exposure"] <- exposureToJson parse.Exposure
    json["ExposureNotes"] <- JsonValue.Create(parse.ExposureNotes)
    json["DeltaAdd"] <- JsonValue.Create(parse.DeltaAdd)
    json["DeltaRemove"] <- JsonValue.Create(parse.DeltaRemove)
    json["DeltaConstrain"] <- JsonValue.Create(parse.DeltaConstrain)
    json["DeltaSplit"] <- JsonValue.Create(parse.DeltaSplit)
    json["DeltaRevealMissing"] <- JsonValue.Create(parse.DeltaRevealMissing)
    json["DeltaNotes"] <- JsonValue.Create(parse.DeltaNotes)
    json["GammaInconsistencyFlagged"] <- JsonValue.Create(parse.GammaInconsistencyFlagged)
    json["GammaEvidenceNeeded"] <- JsonValue.Create(parse.GammaEvidenceNeeded)
    json["GammaHypothesisLogged"] <- JsonValue.Create(parse.GammaHypothesisLogged)
    json["GammaDetails"] <- JsonValue.Create(parse.GammaDetails)
    json["Falsifiable"] <- JsonValue.Create(parse.Falsifiable)
    json["Traceable"] <- JsonValue.Create(parse.Traceable)
    json["PhaseCorrect"] <- JsonValue.Create(parse.PhaseCorrect)
    json["ContextBounded"] <- JsonValue.Create(parse.ContextBounded)
    json["ResultValid"] <- JsonValue.Create(parse.ResultValid)
    json["ResultIndeterminate"] <- JsonValue.Create(parse.ResultIndeterminate)
    json["ResultRejected"] <- JsonValue.Create(parse.ResultRejected)
    json["FormalNoFormalization"] <- JsonValue.Create(parse.FormalNoFormalization)
    json["OutcomeUpdateSigma"] <- JsonValue.Create(parse.OutcomeUpdateSigma)
    json["OutcomeRecordGamma"] <- JsonValue.Create(parse.OutcomeRecordGamma)
    json["OutcomeEscalate"] <- JsonValue.Create(parse.OutcomeEscalate)
    json["OutcomeHold"] <- JsonValue.Create(parse.OutcomeHold)
    json["DerivationEntry"] <-
        match parse.DerivationEntry with
        | Some entry -> JsonValue.Create(derivationEntryToJsonText entry)
        | None -> null
    json

let candidateDecisionToJson (decision: CandidateDecision) =
    let json = JsonObject()
    json["CandidateId"] <- JsonValue.Create(decision.CandidateId)
    json["CandidateType"] <- JsonValue.Create(decision.CandidateType)
    json["Target"] <- JsonValue.Create(decision.Target)
    json["Decision"] <- JsonValue.Create(candidateDecisionValueToJsonText decision.Decision)
    json["Timestamp"] <- JsonValue.Create(decision.Timestamp.ToString("O"))
    json["Rationale"] <- JsonValue.Create(decision.Rationale)
    json

let ledgerEventToJson (ledgerEvent: LedgerEvent) =
    let json = JsonObject()
    json["EventId"] <- JsonValue.Create(ledgerEvent.EventId)
    json["SequenceNumber"] <- JsonValue.Create(ledgerEvent.SequenceNumber)
    json["TimestampUtc"] <- JsonValue.Create(ledgerEvent.TimestampUtc)
    json["Actor"] <- JsonValue.Create(ledgerEvent.Actor)
    json["EventKind"] <- JsonValue.Create(ledgerEvent.EventKind)
    json["TargetId"] <- JsonValue.Create(ledgerEvent.TargetId)
    json["Summary"] <- JsonValue.Create(ledgerEvent.Summary)
    json["Detail"] <- JsonValue.Create(ledgerEvent.Detail)
    json

let evidenceRecordToJson (evidenceRecord: EvidenceRecord) =
    let json = JsonObject()
    json["EvidenceId"] <- JsonValue.Create(evidenceRecord.EvidenceId)
    json["TimestampUtc"] <- JsonValue.Create(evidenceRecord.TimestampUtc)
    json["Actor"] <- JsonValue.Create(evidenceRecord.Actor)
    json["CaptureKind"] <- JsonValue.Create(evidenceRecord.CaptureKind)
    json["TargetKind"] <- JsonValue.Create(evidenceRecord.TargetKind)
    json["TargetId"] <- JsonValue.Create(evidenceRecord.TargetId)
    json["TargetLabel"] <- JsonValue.Create(evidenceRecord.TargetLabel)
    json["Title"] <- JsonValue.Create(evidenceRecord.Title)
    json["Notes"] <- JsonValue.Create(evidenceRecord.Notes)
    json["ContentRef"] <- JsonValue.Create(evidenceRecord.ContentRef)
    json

let jsonArrayFrom values toJson =
    let array = JsonArray()

    values
    |> List.iter (fun value -> array.Add(toJson value))

    array

let projectSnapshotToJson (snapshot: ProjectSnapshot) =
    let json = JsonObject()
    json["SnapshotVersion"] <- JsonValue.Create(snapshot.SnapshotVersion)
    json["SavedAtUtc"] <- JsonValue.Create(snapshot.SavedAtUtc)
    json["ProjectName"] <- JsonValue.Create(snapshot.ProjectName)
    json["PhiIntakes"] <- jsonArrayFrom snapshot.PhiIntakes phiIntakeToJson
    json["ParsedPhis"] <- jsonArrayFrom snapshot.ParsedPhis phiParseToJson
    json["ExcludedPhiIds"] <- jsonStringArray snapshot.ExcludedPhiIds
    json["CandidateDecisions"] <- jsonArrayFrom snapshot.CandidateDecisions candidateDecisionToJson
    json["LedgerEvents"] <- jsonArrayFrom snapshot.LedgerEvents ledgerEventToJson
    json["EvidenceRecords"] <- jsonArrayFrom snapshot.EvidenceRecords evidenceRecordToJson
    json

let tryReadProperty propertyName (json: JsonObject) =
    let mutable node: JsonNode = null

    if json.TryGetPropertyValue(propertyName, &node) then
        Some node
    else
        None

let readProperty context propertyName (json: JsonObject) =
    let mutable node: JsonNode = null

    if json.TryGetPropertyValue(propertyName, &node) then
        node
    else
        failProjectJson (context + "." + propertyName + " is missing.")

let asObject context (node: JsonNode) =
    if isNull node then
        failProjectJson (context + " must be an object.")
    else
        try
            node.AsObject()
        with _ ->
            failProjectJson (context + " must be an object.")

let asArray context (node: JsonNode) =
    if isNull node then
        failProjectJson (context + " must be an array.")
    else
        try
            node.AsArray()
        with _ ->
            failProjectJson (context + " must be an array.")

let readString context propertyName (json: JsonObject) =
    let node = readProperty context propertyName json

    if isNull node then
        failProjectJson (context + "." + propertyName + " must be a string.")
    else
        try
            node.GetValue<string>()
        with _ ->
            failProjectJson (context + "." + propertyName + " must be a string.")

let readInt context propertyName (json: JsonObject) =
    let node = readProperty context propertyName json

    if isNull node then
        failProjectJson (context + "." + propertyName + " must be a number.")
    else
        try
            node.GetValue<int>()
        with _ ->
            failProjectJson (context + "." + propertyName + " must be a whole number.")

let readBool context propertyName (json: JsonObject) =
    let node = readProperty context propertyName json

    if isNull node then
        failProjectJson (context + "." + propertyName + " must be true or false.")
    else
        try
            node.GetValue<bool>()
        with _ ->
            failProjectJson (context + "." + propertyName + " must be true or false.")

let readDateTime context propertyName (json: JsonObject) =
    let value = readString context propertyName json
    let mutable timestamp = DateTime.MinValue

    if DateTime.TryParse(value, CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind, &timestamp) then
        timestamp
    else
        failProjectJson (context + "." + propertyName + " must be an ISO timestamp.")

let readStringList context propertyName (json: JsonObject) =
    let array = readProperty context propertyName json |> asArray (context + "." + propertyName)

    array
    |> Seq.mapi (fun index node ->
        let itemContext = context + "." + propertyName + "[" + string index + "]"

        if isNull node then
            failProjectJson (itemContext + " must be a string.")
        else
            try
                node.GetValue<string>()
            with _ ->
                failProjectJson (itemContext + " must be a string."))
    |> Seq.toList

let readObjectList context propertyName readItem (json: JsonObject) =
    let array = readProperty context propertyName json |> asArray (context + "." + propertyName)

    array
    |> Seq.mapi (fun index node ->
        let itemContext = context + "." + propertyName + "[" + string index + "]"
        readItem itemContext (asObject itemContext node))
    |> Seq.toList

let readOptionalObjectList context propertyName readItem (json: JsonObject) =
    match tryReadProperty propertyName json with
    | None ->
        []
    | Some node ->
        let array = node |> asArray (context + "." + propertyName)

        array
        |> Seq.mapi (fun index node ->
            let itemContext = context + "." + propertyName + "[" + string index + "]"
            readItem itemContext (asObject itemContext node))
        |> Seq.toList

let readExposure context (json: JsonObject) =
    {
        Function = readString context "Function" json
        Mode = readString context "Mode" json
        Interface = readString context "Interface" json
        State = readString context "State" json
        HostCandidate = readString context "HostCandidate" json
    }

let readPhiIntake context (json: JsonObject) =
    {
        PhiId = readString context "PhiId" json
        Date = readString context "Date" json
        Source = readString context "Source" json
        Context = readString context "Context" json
        Confidence = readString context "Confidence" json
        Status = readString context "Status" json
        RawStatement = readString context "RawStatement" json
        Trigger = readString context "Trigger" json
        Claim = readString context "Claim" json
        About = readString context "About" json
        Condition = readString context "Condition" json
        Assumption = readString context "Assumption" json
        TypeText = readString context "TypeText" json
        Impact = readString context "Impact" json
        UnresolvedSignal = readString context "UnresolvedSignal" json
    }

let readOptionalDerivationEntry context propertyName (json: JsonObject) =
    let node = readProperty context propertyName json

    if isNull node then
        None
    else
        try
            Some (node.GetValue<string>() |> parseDerivationEntry (context + "." + propertyName))
        with
        | ProjectJsonReadError message ->
            failProjectJson message
        | _ ->
            failProjectJson (context + "." + propertyName + " must be a string or null.")

let readPhiParse context (json: JsonObject) =
    let exposure = readProperty context "Exposure" json |> asObject (context + ".Exposure") |> readExposure (context + ".Exposure")

    {
        PhiId = readString context "PhiId" json
        Date = readString context "Date" json
        Statement = readString context "Statement" json
        InScope = readString context "InScope" json
        OutOfScope = readString context "OutOfScope" json
        Exposure = exposure
        ExposureNotes = readString context "ExposureNotes" json
        DeltaAdd = readBool context "DeltaAdd" json
        DeltaRemove = readBool context "DeltaRemove" json
        DeltaConstrain = readBool context "DeltaConstrain" json
        DeltaSplit = readBool context "DeltaSplit" json
        DeltaRevealMissing = readBool context "DeltaRevealMissing" json
        DeltaNotes = readString context "DeltaNotes" json
        GammaInconsistencyFlagged = readBool context "GammaInconsistencyFlagged" json
        GammaEvidenceNeeded = readBool context "GammaEvidenceNeeded" json
        GammaHypothesisLogged = readBool context "GammaHypothesisLogged" json
        GammaDetails = readString context "GammaDetails" json
        Falsifiable = readBool context "Falsifiable" json
        Traceable = readBool context "Traceable" json
        PhaseCorrect = readBool context "PhaseCorrect" json
        ContextBounded = readBool context "ContextBounded" json
        ResultValid = readBool context "ResultValid" json
        ResultIndeterminate = readBool context "ResultIndeterminate" json
        ResultRejected = readBool context "ResultRejected" json
        FormalNoFormalization = readBool context "FormalNoFormalization" json
        OutcomeUpdateSigma = readBool context "OutcomeUpdateSigma" json
        OutcomeRecordGamma = readBool context "OutcomeRecordGamma" json
        OutcomeEscalate = readBool context "OutcomeEscalate" json
        OutcomeHold = readBool context "OutcomeHold" json
        DerivationEntry = readOptionalDerivationEntry context "DerivationEntry" json
    }

let readCandidateDecision context (json: JsonObject) =
    {
        CandidateId = readString context "CandidateId" json
        CandidateType = readString context "CandidateType" json
        Target = readString context "Target" json
        Decision = readString context "Decision" json |> parseCandidateDecisionValue (context + ".Decision")
        Timestamp = readDateTime context "Timestamp" json
        Rationale = readString context "Rationale" json
    }

let readLedgerEvent context (json: JsonObject) =
    {
        EventId = readString context "EventId" json
        SequenceNumber = readInt context "SequenceNumber" json
        TimestampUtc = readString context "TimestampUtc" json
        Actor = readString context "Actor" json
        EventKind = readString context "EventKind" json
        TargetId = readString context "TargetId" json
        Summary = readString context "Summary" json
        Detail = readString context "Detail" json
    }

let readEvidenceRecord context (json: JsonObject) =
    {
        EvidenceId = readString context "EvidenceId" json
        TimestampUtc = readString context "TimestampUtc" json
        Actor = readString context "Actor" json
        CaptureKind = readString context "CaptureKind" json
        TargetKind = readString context "TargetKind" json
        TargetId = readString context "TargetId" json
        TargetLabel = readString context "TargetLabel" json
        Title = readString context "Title" json
        Notes = readString context "Notes" json
        ContentRef = readString context "ContentRef" json
    }

let serializeProjectSnapshot (snapshot: ProjectSnapshot) =
    (projectSnapshotToJson snapshot).ToJsonString(projectJsonSerializerOptions)

let tryDeserializeProjectSnapshot json =
    try
        if String.IsNullOrWhiteSpace(json) then
            Error "Project JSON is empty."
        else
            let root = JsonNode.Parse(json) |> asObject "ProjectSnapshot"

            {
                SnapshotVersion = readString "ProjectSnapshot" "SnapshotVersion" root
                SavedAtUtc = readString "ProjectSnapshot" "SavedAtUtc" root
                ProjectName = readString "ProjectSnapshot" "ProjectName" root
                PhiIntakes = readObjectList "ProjectSnapshot" "PhiIntakes" readPhiIntake root
                ParsedPhis = readObjectList "ProjectSnapshot" "ParsedPhis" readPhiParse root
                ExcludedPhiIds = readStringList "ProjectSnapshot" "ExcludedPhiIds" root
                CandidateDecisions = readObjectList "ProjectSnapshot" "CandidateDecisions" readCandidateDecision root
                LedgerEvents = readObjectList "ProjectSnapshot" "LedgerEvents" readLedgerEvent root
                EvidenceRecords = readOptionalObjectList "ProjectSnapshot" "EvidenceRecords" readEvidenceRecord root
            }
            |> Ok
    with
    | ProjectJsonReadError message ->
        Error message
    | :? JsonException as ex ->
        Error ("Invalid JSON: " + ex.Message)
    | ex ->
        Error ("Could not import project JSON: " + ex.Message)
