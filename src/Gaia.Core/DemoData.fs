namespace Gaia.Core

open System

module DemoData =

    let frs: FR list = [
        { Id = "FR1"; Name = "Detect 2D Motion" }
        { Id = "FR2"; Name = "Detect Click Inputs" }
        { Id = "FR3"; Name = "Scroll Content" }
        { Id = "FR4"; Name = "Wireless Connectivity" }
        { Id = "FR5"; Name = "Wired Charging" }
        { Id = "FR6"; Name = "Indicate Status" }
        { Id = "FR7"; Name = "Map Functions to Software" }
        { Id = "FR8"; Name = "Store & Switch Profiles" }
        { Id = "FR9"; Name = "Provide Feedback" }
    ]

    let dps: DP list = [
        { Id = "DP1"; Name = "High DPI Sensor" }
        { Id = "DP2"; Name = "Silent Switch" }
        { Id = "DP3"; Name = "Scroll Wheel" }
        { Id = "DP4"; Name = "Bluetooth" }
        { Id = "DP5"; Name = "USB Charging" }
        { Id = "DP6"; Name = "LED Indicator" }
        { Id = "DP7"; Name = "Software Layer" }
        { Id = "DP8"; Name = "Profile Memory" }
        { Id = "DP9"; Name = "Haptic Controller" }
    ]

    let parts: Part list = [
        { Id = "P1"; Name = "Upper Shell" }
        { Id = "P2"; Name = "Lower Shell + Weight Housing" }
        { Id = "P3"; Name = "Sensor Module" }
        { Id = "P4"; Name = "Main PCB + MCU" }
        { Id = "P5"; Name = "Quiet Click Switches" }
        { Id = "P6"; Name = "Scroll Wheel Assembly" }
        { Id = "P7"; Name = "Wireless Module" }
        { Id = "P8"; Name = "Rechargeable Battery" }
        { Id = "P9"; Name = "USB-C Charging Port" }
        { Id = "P10"; Name = "LED Indicators" }
    ]

    let tfs: TF list = [
        { Id = "TF1"; Name = "Cursor Position" }
        { Id = "TF2"; Name = "Click Sound" }
        { Id = "TF3"; Name = "Tracking Reliability" }
    ]

    let ctqs: CTQ list = [
        { Id = "CTQ1"; Name = "Precision Feedback" }
        { Id = "CTQ2"; Name = "Multi Device UX" }
        { Id = "CTQ3"; Name = "Ergonomics" }
        { Id = "CTQ4"; Name = "Acoustic Profile" }
        { Id = "CTQ5"; Name = "Tracking Reliability" }
        { Id = "CTQ6"; Name = "Software Stability" }
        { Id = "CTQ7"; Name = "Delight Enhancers" }
    ]

    let fr_to_dp = [
        ("FR1", "DP1")
        ("FR2", "DP2")
        ("FR3", "DP3")
        ("FR4", "DP4")
        ("FR5", "DP5")
        ("FR6", "DP6")
        ("FR7", "DP7")
        ("FR8", "DP8")
        ("FR9", "DP9")
    ]

    let dp_to_tf = [
        ("DP1", "TF1")
        ("DP2", "TF2")
        ("DP1", "TF3")
    ]

    let tf_to_ctq = [
        ("TF1", "CTQ1")
        ("TF2", "CTQ4")
        ("TF3", "CTQ5")
    ]

    let dp_to_part = [
        ("DP1", "P3")
        ("DP2", "P5")
        ("DP3", "P6")
        ("DP4", "P7")
        ("DP5", "P9")
        ("DP6", "P10")
        ("DP7", "P4")
        ("DP8", "P4")
        ("DP9", "P10")
    ]

    let fr_to_ctq = [
        ("FR1", "CTQ1")
        ("FR1", "CTQ5")
        ("FR2", "CTQ4")
        ("FR3", "CTQ1")
        ("FR3", "CTQ3")
        ("FR4", "CTQ2")
        ("FR5", "CTQ7")
        ("FR6", "CTQ3")
        ("FR6", "CTQ7")
        ("FR7", "CTQ6")
        ("FR8", "CTQ2")
        ("FR8", "CTQ6")
        ("FR9", "CTQ7")
    ]

    let sigmaBerenice =
        {
            FRs = frs
            DPs = dps
            TFs = tfs
            CTQs = ctqs
            Parts = parts
            FR_to_DP = fr_to_dp
            DP_to_TF = dp_to_tf
            TF_to_CTQ = tf_to_ctq
            DP_to_Part = dp_to_part
            FR_to_CtQ = fr_to_ctq
        }

    let emptyExposure =
        {
            Function = ""
            Mode = ""
            Interface = ""
            State = ""
            HostCandidate = ""
        }

    let emptyIntake =
        {
            PhiId = "PHI-AUD-001"
            Date = DateTime.Now.ToString("yyyy-MM-dd")
            Source = ""
            Context = "Berenice"
            Confidence = "Medium"
            Status = ""
            RawStatement = ""
            Trigger = ""
            Claim = ""
            About = ""
            Condition = ""
            Assumption = ""
            TypeText = ""
            Impact = ""
            UnresolvedSignal = ""
        }

    let emptyParse =
        {
            PhiId = "PHI-AUD-001"
            Date = DateTime.Now.ToString("yyyy-MM-dd")
            Statement = ""
            InScope = ""
            OutOfScope = ""
            Exposure = emptyExposure
            ExposureNotes = ""
            DeltaAdd = false
            DeltaRemove = false
            DeltaConstrain = false
            DeltaSplit = false
            DeltaRevealMissing = false
            DeltaNotes = ""
            GammaInconsistencyFlagged = false
            GammaEvidenceNeeded = false
            GammaHypothesisLogged = false
            GammaDetails = ""
            Falsifiable = false
            Traceable = false
            PhaseCorrect = false
            ContextBounded = false
            ResultValid = false
            ResultIndeterminate = true
            ResultRejected = false
            FormalNoFormalization = false
            OutcomeUpdateSigma = false
            OutcomeRecordGamma = false
            OutcomeEscalate = false
            OutcomeHold = false
            DerivationEntry = None
        }

    let demoParse =
        {
            emptyParse with
                Statement = "Mouse click too loud"
                Exposure =
                    {
                        Function = "Primary click action"
                        Mode = "Normal use"
                        Interface = "Acoustic emission"
                        State = "Button actuation"
                        HostCandidate = "Button + housing + switch"
                    }
                DeltaConstrain = true
                GammaInconsistencyFlagged = true
                GammaEvidenceNeeded = true
                GammaHypothesisLogged = true
                ResultIndeterminate = true
        }

    let demoParse2 =
        {
            emptyParse with
                Statement = "Mouse click acceptable within acoustic limits"
                Exposure =
                    {
                        Function = "Primary click action"
                        Mode = "Normal use"
                        Interface = "Acoustic emission"
                        State = "Button actuation"
                        HostCandidate = "Button + housing + switch"
                    }
                ResultValid = true
                DeltaAdd = false
                DeltaRemove = false
                DeltaConstrain = false
                DeltaSplit = false
                DeltaRevealMissing = false
        }

    let emptyResolution =
        {
            SelectedEntry = None
            ExecutionPath = []
            DeltaSigmaSummary = ""
            MatchedFRs = []
            MatchedDPs = []
            MatchedTFs = []
            MatchedCTQs = []
            GammaSummary = ""
        }

    let initialSnapshot =
        {
            SnapshotId = "S0"
            ParentSnapshotId = None
            Sigma = sigmaBerenice
            Summary = "Initial Berenice baseline"
            CreatedAtUtc = DateTime.UtcNow
        }