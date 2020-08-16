namespace TMEA


module SurprisalAnalysis =
    open FSharp.Stats
    open FSharp.Stats.ML
    open Deedle

    let compute data =
        FSharp.Stats.ML.SurprisalAnalysis.compute data

    ///Computes Surprisal Analysis for the given Data Frame. Expects the input to only contain numerical columns and the data to be column major (e.g. the timepoints/measurements in the columns, the entities in the rows)
    let computeOfDataFrame (f:Frame<'R,'C>) =
        f
        |> Frame.toArray2D
        |> JaggedArray.ofArray2D
        |> matrix
        |> SurprisalAnalysis.compute

    let calculate_FreeEnergyTimeCourse_ForConstraint (constraintIndex:int) (data:float [][]) (saRes:SurprisalAnalysis.SAResult)  =
        let potential = Matrix.getRow saRes.Potentials constraintIndex |> RowVector.toArray
        potential
        |> Array.mapi (fun timePoint potential ->
            let meanSurprisal = 
                saRes.MolecularPhenotypes
                |> fun m -> Matrix.getCol m constraintIndex |> Vector.toArray
                |> Array.mapi (fun transcriptIndex weight -> 
                    data.[timePoint].[transcriptIndex] * weight
                )
                |> Array.sum
            - (potential * meanSurprisal)
        )
