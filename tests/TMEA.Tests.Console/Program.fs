// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Deedle
open Plotly.NET
open System.IO
open FSharpAux

open TMEA
open TMEA.IO
open TMEA.SurprisalAnalysis
open TMEA.MonteCarlo
open TMEA.Frames
open TMEA.Plots


let df =
    Frame.ReadCsv(
        @"C:\Users\schne\OneDrive\Datascience\Net5-scripts\Richter_TMEA\data\mRNA_fpkm_sample_tptCol_HL_Richter.tsv",
        true,
        separators="\t",
        schema="WT_9HL1=float,WT_9HL2=float,WT_9HL3=float,tpt_9HL1=float,tpt_9HL2=float,tpt_9HL3=float,WT_18HL1=float,WT_18HL2=float,WT_18HL3=float,tpt18HL1=float,tpt18HL2=float,tpt18HL3"
    )
    |> Frame.indexRowsString "gene_id"

df.Print(true)

let handleKackNames n =
    match n with
    | "tpt_9H" -> "2_tpt" => 9
    | "tpt18H" -> "2_tpt" => 18
    | "WT_9H"  -> "1_WT"  => 9
    | "WT_18H" -> "1_WT"  => 18

let annotationFrame: Frame<string,string> = 
    let dataSource = Path.Combine(@"C:\Users\schne\OneDrive\Datascience\Net5-scripts\Richter_TMEA\data\arabidopsis_araport11.txt")
    Frame.ReadCsv(dataSource,true,separators="\t")
    |> Frame.indexRows "Identifier"

let annotationMap = 
    annotationFrame
    |> Frame.getCol "MapManDescription"
    |> Series.observations
    |> Array.ofSeq
    |> Array.map (fun (a,b:string) -> a => (b.Split(';')))
    |> Map.ofArray
    |> Map.map (fun k v ->
        v |> Array.collect (fun v -> v.Split('.') |> Array.scanReduce (fun acc elem -> acc + "." + elem))
    )

[<EntryPoint>]
let main argv =

    FSharp.Stats.Algebra.LinearAlgebra.Service() |> ignore

    
    let df_mean_logFPKM =
        df
        |> Frame.mapColKeys (fun ck ->
            let len = ck.Length
            let genotype,timepoint = handleKackNames (ck.Substring(0,len-2))
            let repNr = ck.Substring(len-1)
    
            genotype => (timepoint, repNr)
        )
        |> Frame.sortColsByKey
        |> Frame.transpose
        |> Frame.applyLevel (fun (genotype,(timepoint,repNr)) -> genotype,timepoint) Stats.mean
        |> Frame.transpose
        |> Frame.mapValues (fun (v:float) -> log(v+1.))
        |> Frame.sortColsByKey

    let grouped_noAlign_TmeaRes =
        df_mean_logFPKM
        |> Frame.mapColKeys (fun (a,b) -> $"{a}_{b}H")
        |> Analysis.computeOfDataFrame (TMEAParameters.initDefault()) annotationMap

    grouped_noAlign_TmeaRes
    |> TMEAResult.plotConstraintPotentialTimecourses()

    0 // return an integer exit code