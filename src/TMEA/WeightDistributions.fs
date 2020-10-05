namespace TMEA

module WeightDistributions =

    open FSharp.Stats
    open Plotly.NET
    open FSharpAux
    open TMEA.MonteCarlo
    open TMEA.SurprisalAnalysis


    let getWeightDataForFAS (constraintIndex:int) (fasName:string) (tmeaRes:TMEAResult) =
        let allWeights =
            tmeaRes.Characterizations.[constraintIndex].RawData
            |> Array.filter (fun ann ->
                ann.OntologyTerm = fasName
            )
            |> Array.map (fun ann -> ann.Id , ann.Item)

        FASWeightData.create
            fasName
            allWeights
            (allWeights |> Array.filter (fun (_,weight) -> weight >= 0.))
            (allWeights |> Array.filter (fun (_,weight) -> weight <= 0.))



