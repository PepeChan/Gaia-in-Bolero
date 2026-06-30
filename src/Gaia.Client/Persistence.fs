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

let writeOptionalString (json: JsonObject) (propertyName: string) (value: string option) =
    match value with
    | Some text when not (String.IsNullOrWhiteSpace(text)) ->
        json[propertyName] <- JsonValue.Create(text)
    | _ ->
        ()

let phiIntakeToJson (phi: PhiIntake) =
    let json = JsonObject()
    json["PhiId"] <- JsonValue.Create(phi.PhiId)
    json["Date"] <- JsonValue.Create(phi.Date)
    writeOptionalString json "InputClass" phi.InputClass
    writeOptionalString json "Actor" phi.Actor
    writeOptionalString json "Mission" phi.Mission
    writeOptionalString json "OperationalContext" phi.OperationalContext
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

let phiContextEntryToJson (entry: PhiContextEntry) =
    let json = JsonObject()
    json["ContextId"] <- JsonValue.Create(entry.ContextId)
    json["PhiId"] <- JsonValue.Create(entry.PhiId)
    json["Kind"] <- JsonValue.Create(entry.Kind)
    json["Value"] <- JsonValue.Create(entry.Value)
    json["Provenance"] <- JsonValue.Create(entry.Provenance)
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

let frToJson (fr: FR) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(fr.Id)
    json["Name"] <- JsonValue.Create(fr.Name)
    json

let dpToJson (dp: DP) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(dp.Id)
    json["Name"] <- JsonValue.Create(dp.Name)
    json

let tfToJson (tf: TF) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(tf.Id)
    json["Name"] <- JsonValue.Create(tf.Name)
    json

let ctqToJson (ctq: CTQ) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(ctq.Id)
    json["Name"] <- JsonValue.Create(ctq.Name)
    json

let partToJson (part: Part) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(part.Id)
    json["Name"] <- JsonValue.Create(part.Name)
    json

let vvItemToJson (vvItem: VVItem) =
    let json = JsonObject()
    json["Id"] <- JsonValue.Create(vvItem.Id)
    json["Name"] <- JsonValue.Create(vvItem.Name)
    json

let linkPairToJson ((sourceId: string), (targetId: string)) =
    let json = JsonObject()
    json["SourceId"] <- JsonValue.Create(sourceId)
    json["TargetId"] <- JsonValue.Create(targetId)
    json

let realizationObjectNoteToJson (note: RealizationObjectNote) =
    let json = JsonObject()
    json["ObjectKind"] <- JsonValue.Create(note.ObjectKind)
    json["ObjectId"] <- JsonValue.Create(note.ObjectId)
    json["Description"] <- JsonValue.Create(note.Description)
    json["SourceNote"] <- JsonValue.Create(note.SourceNote)
    json

let sigmaToJson (sigma: Sigma) =
    let json = JsonObject()
    json["FRs"] <- jsonArrayFrom sigma.FRs frToJson
    json["DPs"] <- jsonArrayFrom sigma.DPs dpToJson
    json["TFs"] <- jsonArrayFrom sigma.TFs tfToJson
    json["CTQs"] <- jsonArrayFrom sigma.CTQs ctqToJson
    json["Parts"] <- jsonArrayFrom sigma.Parts partToJson
    json["FR_to_DP"] <- jsonArrayFrom sigma.FR_to_DP linkPairToJson
    json["DP_to_TF"] <- jsonArrayFrom sigma.DP_to_TF linkPairToJson
    json["TF_to_CTQ"] <- jsonArrayFrom sigma.TF_to_CTQ linkPairToJson
    json["DP_to_Part"] <- jsonArrayFrom sigma.DP_to_Part linkPairToJson
    json["FR_to_CtQ"] <- jsonArrayFrom sigma.FR_to_CtQ linkPairToJson
    json

let realizationStateToJson (state: RealizationState) =
    let json = JsonObject()
    json["Sigma"] <- sigmaToJson state.Sigma
    json["VVItems"] <- jsonArrayFrom state.VVItems vvItemToJson
    json["ObjectNotes"] <- jsonArrayFrom state.ObjectNotes realizationObjectNoteToJson
    json["Host_to_Part"] <- jsonArrayFrom state.Host_to_Part linkPairToJson
    json["Function_to_FR"] <- jsonArrayFrom state.Function_to_FR linkPairToJson
    json["CTQ_to_VV"] <- jsonArrayFrom state.CTQ_to_VV linkPairToJson
    json

let projectSnapshotToJson (snapshot: ProjectSnapshot) =
    let json = JsonObject()
    json["SnapshotVersion"] <- JsonValue.Create(snapshot.SnapshotVersion)
    json["SavedAtUtc"] <- JsonValue.Create(snapshot.SavedAtUtc)
    json["ProjectName"] <- JsonValue.Create(snapshot.ProjectName)
    json["PhiIntakes"] <- jsonArrayFrom snapshot.PhiIntakes phiIntakeToJson
    json["PhiContextEntries"] <- jsonArrayFrom snapshot.PhiContextEntries phiContextEntryToJson
    json["ParsedPhis"] <- jsonArrayFrom snapshot.ParsedPhis phiParseToJson
    json["StaleParsedPhiIds"] <- jsonStringArray snapshot.StaleParsedPhiIds
    json["ExcludedPhiIds"] <- jsonStringArray snapshot.ExcludedPhiIds
    json["CandidateDecisions"] <- jsonArrayFrom snapshot.CandidateDecisions candidateDecisionToJson
    json["LedgerEvents"] <- jsonArrayFrom snapshot.LedgerEvents ledgerEventToJson
    json["EvidenceRecords"] <- jsonArrayFrom snapshot.EvidenceRecords evidenceRecordToJson
    json["RealizationState"] <- realizationStateToJson snapshot.RealizationState
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

let readOptionalString context propertyName (json: JsonObject) =
    match tryReadProperty propertyName json with
    | None ->
        None
    | Some node ->
        if isNull node then
            None
        else
            try
                let value = node.GetValue<string>()

                if String.IsNullOrWhiteSpace(value) then
                    None
                else
                    Some value
            with _ ->
                failProjectJson (context + "." + propertyName + " must be a string or null.")

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

let readOptionalStringList context propertyName (json: JsonObject) =
    match tryReadProperty propertyName json with
    | None ->
        []
    | Some node ->
        let array = node |> asArray (context + "." + propertyName)

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
        InputClass = readOptionalString context "InputClass" json
        Actor = readOptionalString context "Actor" json
        Mission = readOptionalString context "Mission" json
        OperationalContext = readOptionalString context "OperationalContext" json
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

let readPhiContextEntry context (json: JsonObject) =
    {
        ContextId = readString context "ContextId" json
        PhiId = readString context "PhiId" json
        Kind = readString context "Kind" json
        Value = readString context "Value" json
        Provenance = readString context "Provenance" json
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

let readFR context (json: JsonObject) : FR =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readDP context (json: JsonObject) : DP =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readTF context (json: JsonObject) : TF =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readCTQ context (json: JsonObject) : CTQ =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readPart context (json: JsonObject) : Part =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readVVItem context (json: JsonObject) : VVItem =
    {
        Id = readString context "Id" json
        Name = readString context "Name" json
    }

let readLinkPair context (json: JsonObject) =
    readString context "SourceId" json, readString context "TargetId" json

let readRealizationObjectNote context (json: JsonObject) =
    {
        ObjectKind = readString context "ObjectKind" json
        ObjectId = readString context "ObjectId" json
        Description = readString context "Description" json
        SourceNote = readString context "SourceNote" json
    }

let readSigma context (json: JsonObject) =
    {
        FRs = readObjectList context "FRs" readFR json
        DPs = readObjectList context "DPs" readDP json
        TFs = readObjectList context "TFs" readTF json
        CTQs = readObjectList context "CTQs" readCTQ json
        Parts = readObjectList context "Parts" readPart json
        FR_to_DP = readObjectList context "FR_to_DP" readLinkPair json
        DP_to_TF = readObjectList context "DP_to_TF" readLinkPair json
        TF_to_CTQ = readObjectList context "TF_to_CTQ" readLinkPair json
        DP_to_Part = readObjectList context "DP_to_Part" readLinkPair json
        FR_to_CtQ = readOptionalObjectList context "FR_to_CtQ" readLinkPair json
    }

let readRealizationState context (json: JsonObject) =
    let sigma =
        readProperty context "Sigma" json
        |> asObject (context + ".Sigma")
        |> readSigma (context + ".Sigma")

    {
        Sigma = sigma
        VVItems = readOptionalObjectList context "VVItems" readVVItem json
        ObjectNotes = readOptionalObjectList context "ObjectNotes" readRealizationObjectNote json
        Host_to_Part = readOptionalObjectList context "Host_to_Part" readLinkPair json
        Function_to_FR = readOptionalObjectList context "Function_to_FR" readLinkPair json
        CTQ_to_VV = readOptionalObjectList context "CTQ_to_VV" readLinkPair json
    }

let readOptionalRealizationState context (json: JsonObject) =
    match tryReadProperty "RealizationState" json with
    | None -> emptyRealizationState
    | Some node ->
        node
        |> asObject (context + ".RealizationState")
        |> readRealizationState (context + ".RealizationState")

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
                PhiContextEntries = readOptionalObjectList "ProjectSnapshot" "PhiContextEntries" readPhiContextEntry root
                ParsedPhis = readObjectList "ProjectSnapshot" "ParsedPhis" readPhiParse root
                StaleParsedPhiIds = readOptionalStringList "ProjectSnapshot" "StaleParsedPhiIds" root
                ExcludedPhiIds = readStringList "ProjectSnapshot" "ExcludedPhiIds" root
                CandidateDecisions = readObjectList "ProjectSnapshot" "CandidateDecisions" readCandidateDecision root
                LedgerEvents = readObjectList "ProjectSnapshot" "LedgerEvents" readLedgerEvent root
                EvidenceRecords = readOptionalObjectList "ProjectSnapshot" "EvidenceRecords" readEvidenceRecord root
                RealizationState = readOptionalRealizationState "ProjectSnapshot" root
            }
            |> Ok
    with
    | ProjectJsonReadError message ->
        Error message
    | :? JsonException as ex ->
        Error ("Invalid JSON: " + ex.Message)
    | ex ->
        Error ("Could not import project JSON: " + ex.Message)
