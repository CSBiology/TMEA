#r "nuget: FSharpAux"
#r "nuget: FSharpAux.IO"
#r "nuget: FSharp.Stats"
#r "nuget: Deedle"
#r "nuget: FSharp.Stats, 0.4.2"
#r "nuget: BioFSharp, 2.0.0-beta6"
#r "nuget: BioFSharp.Stats, 2.0.0-beta6"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

open FSharp.Stats
open Deedle
open FSharpAux

#load "Domain.fs"
#load "Frames.fs"
#load "IO.fs"
#load "SurprisalAnalysis.fs"
#load "MonteCarlo.fs"
#load "WeightDistributions.fs"
#load "Plots.fs"
#load "Analysis.fs"

open System.IO

open TMEA
open TMEA.IO
open TMEA.SurprisalAnalysis
open TMEA.MonteCarlo
open TMEA.Frames
open TMEA.Plots

open System.Text.RegularExpressions

FSharp.Stats.Algebra.LinearAlgebra.Service()

let testAnnotationFrame: Frame<string,string> = 
    let dataSource = Path.Combine(__SOURCE_DIRECTORY__,"../../tests/data/arabidopsis_araport11.txt")
    Frame.ReadCsv(dataSource,true,separators="\t")
    |> Frame.indexRows "Identifier"

testAnnotationFrame.Print()

let testAnnotationMap = 
    testAnnotationFrame
    |> Frame.getCol "MapManDescription"
    |> Series.observations
    |> Array.ofSeq
    |> Array.map (fun (a,b:string) -> a => (b.Split(';')))
    |> Map.ofArray
    |> Map.map (fun k v ->
        v |> Array.collect (fun v -> v.Split('.') |> Array.scanReduce (fun acc elem -> acc + "." + elem))
    )

let testData =
    let dataSource = Path.Combine(__SOURCE_DIRECTORY__,"../../tests/data/Highlight_LogFPKM.csv")
    TMEA.IO.readDataFrame 
        "TranscriptIdentifier" 
        ","
        dataSource

testData.Print()

let testTmeaRes = 
    testData
    |> Analysis.computeOfDataFrame (TMEAParameters.initDefault()) testAnnotationMap

open Plotly.NET

let characterizationFrame = 
    testTmeaRes
    |> TMEAResult.toTMEACharacterizationFrame()

characterizationFrame.Print()

testTmeaRes
|> TMEAResult.plotConstraintPotentialTimecourses(OmitBaselineState=true)

testTmeaRes
|> TMEAResult.plotConstraintPotentialTimecourses(OmitBaselineState=true,InvertConstraints=[|1;2;3|], ConstraintCutoff=3)

testTmeaRes
|> TMEAResult.plotConstraintPotentialHeatmap(ConstraintCutoff=3,InvertConstraints=[|2|])

testTmeaRes
|> TMEAResult.plotFreeEnergyLandscape()

testTmeaRes
|> TMEAResult.plotConstraintImportance()

testTmeaRes
|> TMEAResult.plotDataRecovery(true,3)

testTmeaRes
|> TMEAResult.plotFASWeightDistribution("signalling.light",[1;2;3],true,0.05)



testTmeaRes
|> TMEAResult.toSignificanceMatrixFrame()
|> fun f -> f.Print()

testTmeaRes.Constraints
|> Matrix.toArray2D
|> Frame.ofArray2D
|> Frame.mapRowKeys(fun (i:int) -> testTmeaRes.EntityNames.[i])
|> Frame.mapColKeys (fun (ck: int) -> $"C_{ck}")
|> fun f -> f.Print()


testTmeaRes.ConstraintPotentials
|> Matrix.toArray2D
|> Frame.ofArray2D
|> Frame.mapRowKeys(fun (i:int) -> $"C_{i}")
|> Frame.mapColKeys (fun (i:int) -> testTmeaRes.Timepoints.[i])
|> fun f -> f.Print()

//readDataFrame 
//    "TranscriptIdentifier" 
//    "\t"
//    @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\Corrected_Data_logFPKM_SFBCore_HighLight.txt"
//|> fun f ->
//    f 
//    |> TMEA.SurprisalAnalysis.computeOfDataFrame
//    |> TMEA.MonteCarlo.computeOfSARes 
//        true
//        tair10_MapMan_Annotations 
//        (f.RowKeys |> Array.ofSeq) 
//        99
//    |> TMEA.Frames.createTMEACharacterizationTable 5 id

