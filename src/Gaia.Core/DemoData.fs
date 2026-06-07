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

    let demoSigma = sigmaBerenice

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
                        Function = "Detect Click Inputs"
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
            DeltaCandidateSummary = ""
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
    
    let demoScenarios : DemoScenario list =
        [
            {
                Id = "berenice-click-too-loud"
                Title = "Berenice — click too loud"
                Description = "A Φ that constrains acoustic behavior and produces Γ flags."
                Intake = emptyIntake
                Parse = demoParse
            }

            {
                Id = "berenice-click-acceptable"
                Title = "Berenice — click acceptable"
                Description = "A Φ that produces no ΔΣ candidate."
                Intake = emptyIntake
                Parse = demoParse2
            }

            {
                Id = "berenice-confirm-quiet-click"
                Title = "Berenice — confirm quiet click constraint"
                Description = "A valid Φ that confirms an existing acoustic constraint and produces an admissible ΔΣ."
                Intake = emptyIntake
                Parse =
                    {
                        emptyParse with
                            PhiId = "PHI-AUD-003"
                            Statement = "Confirm that quiet click behavior is constrained by the acoustic click transfer function."
                            Exposure =
                                {
                                    Function = "Quiet Click Behavior"
                                    Mode = "Click"
                                    Interface = "Button-to-housing mechanical interface"
                                    State = "Click sound pressure level below threshold"
                                    HostCandidate = "Click mechanism"
                                }
                            DeltaConstrain = true
                            ResultValid = true
                            DerivationEntry = Some FromFR
                    }
            }

            {
                Id = "sphynx-stylus-low-light"
                Title = "Sphynx — stylus under low ambient light"
                Description = "Evaluates whether stylus interaction remains admissible under low-light operational conditions."

                Intake = emptyIntake

                Parse =
                    {
                        emptyParse with

                            PhiId = "PHI-SPHYNX-010"

                            Statement =
                                "The stylus shall continue functioning during low ambient light conditions."

                            Exposure =
                                {
                                    Function = "Detect Stylus Inputs"
                                    Mode = "Low Light Operation"
                                    Interface = "Stylus-to-display interaction"
                                    State = "Low ambient illumination"
                                    HostCandidate = "Sphynx Display Assembly"
                                }

                            DeltaConstrain = true

                            GammaEvidenceNeeded = false
                            GammaInconsistencyFlagged = false
                            GammaHypothesisLogged = false

                            ResultValid = true
                            ResultIndeterminate = false
                            ResultRejected = false

                            OutcomeHold = false
                            OutcomeEscalate = false

                            DerivationEntry = Some FromMode
                    }
            }

            {
            Id = "sphynx-unparsed-block"
            Title = "Sphynx — unresolved internet block"
            Description = "Tests probing of an unparsed context block."
            Intake = emptyIntake

            Parse =
                {
                    emptyParse with
                        PhiId = "PHI-SPHYNX-001"
                        Statement = "Is the internet block part of the Sphynx device context?"

                        Exposure =
                            {
                                Function = ""
                                Mode = ""
                                Interface = "Internet connection"
                                State = ""
                                HostCandidate = "External internet service"
                            }

                        DeltaRevealMissing = true

                        GammaEvidenceNeeded = true
                        GammaHypothesisLogged = true

                        ResultIndeterminate = true
                        OutcomeHold = true

                        DerivationEntry = Some FromInterface
                }
            }
        ]
    
    let sphynxUnparsedBlockParse =
        {
            emptyParse with
                PhiId = "PHI-SPHYNX-001"
                Statement = "Is the internet block part of the Sphynx device context?"
                Exposure =
                    {
                        Function = ""
                        Mode = ""
                        Interface = "Internet connection"
                        State = ""
                        HostCandidate = "External internet service"
                    }
                DeltaRevealMissing = true
                GammaEvidenceNeeded = true
                GammaHypothesisLogged = true
                ResultIndeterminate = true
                OutcomeHold = true
                DerivationEntry = Some FromInterface
        }
    let demoPhiIntakes : PhiIntake list =
        [
            {
                PhiId = "PHI-SPHYNX-SEED-001"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users need a large, calm reading surface for long research sessions."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall enable full-page low-fatigue reading and research during extended use."
                Trigger = "Large-format reading and research capability is a core Sphynx use case."
                Claim = "Enable full-page low-fatigue reading and research."
                About = "Sphynx reading and research capability"
                Condition = "During extended use"
                Assumption = "The user performs long reading and research sessions on the device."
                TypeText = "Requirement"
                Impact = "Introduces reading/research function and extended-use mode."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-002"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users need precise handwriting and drawing interaction on the screen."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall provide comfortable high-control handwriting while using the stylus on the screen."
                Trigger = "Stylus-based handwriting is a primary interaction mode."
                Claim = "Provide comfortable high-control handwriting."
                About = "Stylus handwriting interaction"
                Condition = "While using the stylus on the screen"
                Assumption = "The stylus and screen form the main handwriting interface."
                TypeText = "Requirement"
                Impact = "Introduces handwriting mode, stylus-screen interface, and active interaction state."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-003"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users may need productive office-like work even when network connectivity is unavailable."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall support offline and grade-productive workflows when network connectivity is unavailable."
                Trigger = "Office-grade productivity must survive intermittent or missing internet access."
                Claim = "Support offline and productive workflows."
                About = "Offline productivity workflow"
                Condition = "When network connectivity is unavailable"
                Assumption = "The user may continue working without internet access."
                TypeText = "Requirement"
                Impact = "Introduces offline operation state and productivity workflow function."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-004"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users need to compare or review documents side by side."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall support split-screen document review while operating in split-screen mode."
                Trigger = "Document review benefits from simultaneous visible work areas."
                Claim = "Support split-screen document review."
                About = "Split-screen document review"
                Condition = "While operating in split-screen mode"
                Assumption = "The screen and software UI can present multiple document regions."
                TypeText = "Requirement"
                Impact = "Introduces split-screen mode and screen/UI interface implications."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-005"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Stylus quality depends strongly on perceived pen-to-screen distance."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall minimize perceived pen-to-screen distance while the stylus contacts the cover glass."
                Trigger = "Pen-to-screen distance affects writing precision and perceived quality."
                Claim = "Minimize perceived pen-to-screen distance."
                About = "Stylus-to-screen stack thickness"
                Condition = "While the stylus contacts the cover glass"
                Assumption = "The cover glass and display stack influence stylus perception."
                TypeText = "Constraint"
                Impact = "Introduces a constraint on the glass/display/stylus interface."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-006"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "The tablet must remain usable when disconnected from external power."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall provide power to the tablet when the device is not plugged into external power."
                Trigger = "Mobile productivity requires untethered power availability."
                Claim = "Provide power to the tablet."
                About = "Tablet power supply"
                Condition = "When the device is not plugged into external power"
                Assumption = "Battery and power management are required for mobile operation."
                TypeText = "Requirement"
                Impact = "Introduces battery/power-management function and unplugged operating state."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-007"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "The visual interface should support focused work without aggressive visual behavior."
                Confidence = "Medium"
                Status = "Ingested"
                RawStatement = "The Sphynx shall provide calm non-aggressive visual output during focused work sessions."
                Trigger = "Focused work benefits from restrained visual output and low distraction."
                Claim = "Provide calm non-aggressive visual output."
                About = "Visual output behavior"
                Condition = "During focused work sessions"
                Assumption = "The UI and display behavior influence perceived calmness."
                TypeText = "Requirement"
                Impact = "Introduces visual-output behavior and focused-work mode."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-008"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users may hold the Sphynx while reading, writing, or moving between work contexts."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall provide tactile non-slippery handheld interaction while the user holds the device."
                Trigger = "Large-format handheld use requires secure tactile handling."
                Claim = "Provide tactile non-slippery handheld interaction."
                About = "Handheld physical interaction"
                Condition = "While the user holds the device"
                Assumption = "Surface finish, edges, mass, and grip affect handling."
                TypeText = "Requirement"
                Impact = "Introduces handheld-use mode and physical user-device interface."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-009"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "Users need low-friction transitions between reading, writing, app use, and file work."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall minimize workflow friction while the user moves between reading, writing, and file work."
                Trigger = "Productivity depends on smooth transitions across work activities."
                Claim = "Minimize workflow friction."
                About = "Cross-activity workflow"
                Condition = "While the user moves between reading, writing, and file work"
                Assumption = "Software, file handling, input modes, and UI continuity affect workflow friction."
                TypeText = "Requirement"
                Impact = "Introduces workflow transition behavior across multiple modes."
                UnresolvedSignal = ""
            }

            {
                PhiId = "PHI-SPHYNX-SEED-010"
                Date = "2026-05-31"
                Source = "Sphynx architecture / Φ parsing baseline"
                Context = "The product is thin and large-format, so structural integrity is a core architectural concern."
                Confidence = "High"
                Status = "Ingested"
                RawStatement = "The Sphynx shall maintain structural integrity under thin large-format use."
                Trigger = "Thin large-format hardware may flex, deform, or fail under handling loads."
                Claim = "Maintain structural integrity."
                About = "Thin large-format structural design"
                Condition = "Under thin large-format use"
                Assumption = "The full module must resist bending, flexing, and handling loads."
                TypeText = "Requirement"
                Impact = "Introduces structural integrity constraint and full-module host implications."
                UnresolvedSignal = ""
            }
        ]