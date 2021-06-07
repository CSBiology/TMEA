namespace TMEA


module SurprisalAnalysis =
    open FSharp.Stats
    open FSharp.Stats.ML
    open Deedle
    open FSharpAux

    let compute data =
        FSharp.Stats.ML.SurprisalAnalysis.compute data

    ///Computes Surprisal Analysis for the given Data Frame. Expects the input to only contain numerical columns and the data to be column major (e.g. the timepoints/measurements in the columns, the entities in the rows)
    let computeOfDataFrame (f:Frame<'R,'C>) =
        f
        |> Frame.toArray2D
        |> JaggedArray.ofArray2D
        |> matrix
        |> SurprisalAnalysis.compute

    type TMEAResult with

        static member calculate_FreeEnergyTimeCourse_ForConstraint (constraintIndex:int) (tmeaRes:TMEAResult)  =
            let potential = Matrix.getRow tmeaRes.ConstraintPotentials constraintIndex |> RowVector.toArray
            potential
            |> Array.mapi (fun timePoint potential ->
                let meanSurprisal = 
                    tmeaRes.Constraints
                    |> fun m -> Matrix.getCol m constraintIndex |> Vector.toArray
                    |> Array.mapi (fun transcriptIndex weight -> 
                        tmeaRes.Data.[timePoint].[transcriptIndex] * weight
                    )
                    |> Array.sum
                - (potential * meanSurprisal)
            )

        static member calculate_TotalFreeEnergyTimeCourse (tmeaRes:TMEAResult)  =
            [|1..tmeaRes.Data.Length-1|]
            |> Array.map (fun cI ->
                TMEAResult.calculate_FreeEnergyTimeCourse_ForConstraint cI tmeaRes
            )
            |> JaggedArray.transpose  
            |> Array.map Array.sum

        static member invertSignsFor (constraintIndices:int []) (tmeaRes:TMEAResult)=
            {
                tmeaRes with
                    ConstraintPotentials =
                        tmeaRes.ConstraintPotentials
                        |> Matrix.mapiRows(fun i cons -> 
                            if Array.contains i constraintIndices then
                                cons 
                                |> RowVector.toArray
                                |> Array.map (fun v -> v * - 1.)
                                |> RowVector.ofArray
                            else 
                                cons
                        )
                        |> matrix

                    Constraints =
                        tmeaRes.Constraints
                        |> Matrix.mapiCols(fun i pattern -> 
                            if Array.contains i constraintIndices then
                                pattern 
                                |> Vector.map (fun v -> v * - 1.)
                            else 
                                pattern
                        )
                        |> matrix
                        |> Matrix.transpose

                    AnalysisParameters =
                        {
                            tmeaRes.AnalysisParameters with
                                InvertedConstraints =
                                    let previous =  tmeaRes.AnalysisParameters.InvertedConstraints |> set
                                    let next = constraintIndices |> set
                                    Set.symmetricDifference previous next
                                    |> Set.toArray
                        }
            }