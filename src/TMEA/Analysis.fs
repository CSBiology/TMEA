namespace TMEA

[<RequireQualifiedAccess>]
module Analysis =

    open TMEA.IO
    open TMEA.SurprisalAnalysis
    open TMEA.MonteCarlo
    open TMEA.Frames
    open TMEA.Plots

    open Deedle
    open FSharpAux
    open FSharp.Stats

    let standardTMEAParameters = TMEAParameters.create "not assigned" 9999 true

    let computeOfDataFrame (parameters: TMEAParameters) (ontologyAnnotations:Map<string,string []>) (dataFrame:Frame<string,string>) = 
        
        let data = 
            dataFrame
            |> Frame.toArray2D
            |> JaggedArray.ofArray2D
            |> JaggedArray.transpose

        let saRes = 
            dataFrame
            |> SurprisalAnalysis.computeOfDataFrame

        let tmeaChar =
            saRes 
            |> MonteCarlo.computeOfSARes 
                parameters.Verbose 
                ontologyAnnotations 
                (dataFrame.RowKeys |> Array.ofSeq) 
                parameters.MissingKey 
                parameters.BootstrapIterations

        TMEAResult.create 
            parameters
            data
            (dataFrame.RowKeys      |> Array.ofSeq)
            (dataFrame.ColumnKeys   |> Array.ofSeq)
            ontologyAnnotations
            saRes.SingularValues
            saRes.MolecularPhenotypes
            saRes.Potentials
            tmeaChar

    let computeFromFile (parameters: TMEAParameters) (ontologyAnnotations:Map<string,string []>) (identifierCol:string) (separators:string) (path:string) = 
        IO.readDataFrame identifierCol separators path
        |> computeOfDataFrame parameters ontologyAnnotations

