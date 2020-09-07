#r "D:\\nuget_cache\\system.runtime.interopservices.runtimeinformation\\4.3.0\\lib\\netstandard1.1\\System.Runtime.InteropServices.RuntimeInformation.dll" 
#r "D:\\nuget_cache\\system.threading\\4.3.0\\lib\\netstandard1.3\\System.Threading.dll" 
#r "D:\\nuget_cache\\newtonsoft.json\\12.0.3\\lib\\netstandard2.0\\Newtonsoft.Json.dll" 
#r "D:\\nuget_cache\\fsharp.plotly\\2.0.0-alpha\\lib\\netstandard2.0\\FSharp.Plotly.dll" 
#r "D:\\nuget_cache\\fsharpaux\\1.0.0\\lib\\netstandard2.0\\FSharpAux.dll" 
#r "D:\\nuget_cache\\biofsharp\\2.0.0-beta4\\lib\\netstandard2.0\\BioFSharp.dll" 
#r "D:\\nuget_cache\\fsharp.stats\\0.2.1-beta\\lib\\netstandard2.0\\FSharp.Stats.dll" 
#r "D:\\nuget_cache\\fsharpaux.io\\1.0.0\\lib\\netstandard2.0\\FSharpAux.IO.dll" 
#r "D:\\nuget_cache\\biofsharp.stats\\2.0.0-beta4\\lib\\netstandard2.0\\BioFSharp.Stats.dll" 
#r "D:\\nuget_cache\\deedle\\2.2.0\\lib\\net45\\Deedle.dll"

open FSharp.Stats
open Deedle
open FSharpAux

#load "D:\\nuget_cache\\deedle\2.2.0\Deedle.fsx"
#load "Domain.fs"
#load "IO.fs"
#load "SurprisalAnalysis.fs"
#load "MonteCarlo.fs"
#load "Frames.fs"
#load "Plots.fs"
#load "Analysis.fs"

open TMEA
open TMEA.IO
open TMEA.SurprisalAnalysis
open TMEA.MonteCarlo
open TMEA.Frames
open TMEA.Plots


open System.Text.RegularExpressions

FSharp.Stats.Algebra.LinearAlgebra.Service()

let multipleHitRegex = System.Text.RegularExpressions.Regex("^'(?<start>(MULTIPLE HITS: \())(?<atnumbers>(at[a-zA-Z0-9]*),?)*\)")

let singleHitRegex =  System.Text.RegularExpressions.Regex("^'\((?<atnumber>at[a-zA-Z0-9]*)*\){1}")

let multipleMatchTest = singleHitRegex.Match("'MULTIPLE HITS: (at3g05160,at3g05165). at3g05160: sugar transporter, putative | chr3:1453106-1457150 REVERSE at3g05165: sugar transporter, putative | chr3:1458136-1462917 REVERSE'	T")


let tair10_MapManAnnotations_Arabidopsis =
    Frame.ReadCsv(
        @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\data\SFBCore\Ath_AFFY_ATH1_TAIR10_Aug2012.txt",
        true,
        separators = "\t"
    )
    |> fun f ->
        let descriptions = f |> Frame.getCol "DESCRIPTION"
        let atnumbers =
            descriptions
            |> Series.mapValues (fun (description:string) -> 
                let multipleMatches = 
                    multipleHitRegex.Match(description).Groups.Item("atnumbers").Captures
                    |> Seq.cast<Capture>
                    |> Seq.map (fun c -> c.Value)
                    |> List.ofSeq

                let singleMatch = 
                    singleHitRegex.Match(description).Groups.Item("atnumber").Value

                match multipleMatches,singleMatch with
                | [],"" -> ""
                | matches, "" when matches.Length > 0 -> matches |> String.concat ";" |> String.replace "," ""
                | [], sMatch when sMatch <> "" -> sMatch
                | _ -> ""
            )
        f
        |> Frame.addCol "ATNUMBERS" atnumbers
    |> Frame.map (fun rk ck (v:string) ->
        if v.StartsWith("'") then
            if v.EndsWith("'") then
                v.Substring(1,(v.Length-2))
            else
                v.Substring(1)
        else
            v
    )   

let tair10_MapManAnnotations_Arabidopsis_InGut : Frame<(string*string),string> =
    tair10_MapManAnnotations_Arabidopsis
    |> Frame.groupRowsBy "TYPE"
    |> Frame.groupRowsBy "BINCODE"
    |> Frame.sortRowsByKey
    |> Frame.reduceLevel
        (fun (bincode,(typ,index)) -> bincode,typ)
        (fun (v1:string) (v2:string) ->
            if v1 = v2 then 
                v1
            else
                sprintf "%s;%s" v1 v2
        )
    |> Frame.sortRowsByKey

tair10_MapManAnnotations_Arabidopsis_InGut
|> fun f -> f.RowKeys |> Seq.map fst
|> fun (x:seq<string>) -> x |> set
|> Set.iter (printfn "%s")

//tair10_MapManAnnotations_Arabidopsis_InGut
//|> fun f -> 
//    f.SaveCsv(
//        @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\TAIR10_MapManAnnotations_FLAT.txt",
//        ["BINCODE"; "TYPE"],
//        '\t'
//    )

let tair10_MapMan_Annotations : Map<string,string []>=
    let f = 
        tair10_MapManAnnotations_Arabidopsis_InGut
        |> Frame.filterRows (fun (bincode,typ) _ -> typ = "T")
    let atnumbers = 
        f
        |> Frame.getCol "ATNUMBERS"

    let names = 
        f
        |> Frame.getCol "NAME"

    Series.zipInner names atnumbers
    |> Series.observations
    |> Seq.map (fun ((binCode,_),((name:string),(atnumbers:string))) -> 
        atnumbers.Split(';')
        |> Array.map (fun atnumber ->
            atnumber => (name,binCode)
        )
    )
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun (atnumber,(x)) -> 
        atnumber.ToUpperInvariant() => (x |> Seq.map (snd >> snd) |> Array.ofSeq)
    )
    |> Map.ofSeq
    |> Map.filter (fun k v -> k <> "")

let data =
    TMEA.IO.readDataFrame 
        "TranscriptIdentifier" 
        "\t"
        @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\Corrected_Data_logFPKM_SFBCore_HighLight.txt"
    |> Frame.toArray2D
    |> JaggedArray.ofArray2D
    |> JaggedArray.transpose

let SaRes = 
    readDataFrame 
        "TranscriptIdentifier" 
        "\t"
        @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\Corrected_Data_logFPKM_SFBCore_HighLight.txt"
    |> TMEA.SurprisalAnalysis.computeOfDataFrame

let tmeaRes = 
    TMEA.IO.readDataFrame 
        "TranscriptIdentifier" 
        "\t"
        @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\Corrected_Data_logFPKM_SFBCore_HighLight.txt"
    |> Analysis.computeOfDataFrame Analysis.standardTMEAParameters tair10_MapMan_Annotations




open FSharp.Plotly

tmeaRes
|> TMEAResult.plotConstraintTimecourses true

tmeaRes
|> TMEAResult.plotPotentialHeatmap true

tmeaRes
|> TMEAResult.plotFreeEnergyLandscape true

tmeaRes
|> TMEAResult.plotConstraintImportance true

tmeaRes
|> TMEAResult.plotDataRecovery true 3

readDataFrame 
    "TranscriptIdentifier" 
    "\t"
    @"D:\OneDrive\Datascience\projects\EntropyDataAnalysis\results\EverythingSailent\Meta\Corrected_Data_logFPKM_SFBCore_HighLight.txt"
|> fun f ->
    f 
    |> TMEA.SurprisalAnalysis.computeOfDataFrame
    |> TMEA.MonteCarlo.computeOfSARes 
        true
        tair10_MapMan_Annotations 
        (f.RowKeys |> Array.ofSeq) 
        99
    |> TMEA.Frames.createTMEACharacterizationTable 5 id

