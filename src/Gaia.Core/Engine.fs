namespace Gaia.Core

module Engine =

    let findFRByName (sigma: Sigma) name =
        sigma.FRs
        |> List.filter (fun fr -> fr.Name = name)
        |> List.map (fun fr -> fr.Id)

    let findDPsForFR (sigma: Sigma) frIds =
        sigma.FR_to_DP
        |> List.choose (fun (fr, dp) -> if List.contains fr frIds then Some dp else None)

    let findTFsForDP (sigma: Sigma) dpIds =
        sigma.DP_to_TF
        |> List.choose (fun (dp, tf) -> if List.contains dp dpIds then Some tf else None)

    let findCTQsForTF (sigma: Sigma) tfIds =
        sigma.TF_to_CTQ
        |> List.choose (fun (tf, ctq) -> if List.contains tf tfIds then Some ctq else None)

    let selectDerivationEntry (p: PhiParse) : DerivationEntry =
        if p.ResultRejected then
            GammaOnly
        elif p.ResultIndeterminate then
            GammaOnly
        elif p.FormalNoFormalization then
            GammaOnly
        elif p.Exposure.State <> "" then
            FromState
        elif p.Exposure.Interface <> "" then
            FromInterface
        elif p.Exposure.Mode <> "" then
            FromMode
        elif p.Exposure.Function <> "" then
            FromFR
        else
            FromParametric

    let executionPath = function
        | GammaOnly ->
            [ "Γ-only"
              "Stop; no Σ update" ]
        | FromFR ->
            [ "FR"
              "Context Diagram"
              "Modes Analysis"
              "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromMode ->
            [ "Modes Analysis"
              "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromInterface ->
            [ "Interface Characterization"
              "States implied by Interfaces"
              "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromState ->
            [ "State Determination"
              "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]
        | FromParametric ->
            [ "Transfer Function Candidates"
              "Proto-part Structure"
              "Axiomatic Design / Choice of DP"
              "Simulation"
              "Performance"
              "V&V" ]