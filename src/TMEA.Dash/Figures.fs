module Figures

open Deedle
open Plotly.NET

let getSmallPreview (f:Frame<_,_>) =
    let maxColsIdx = System.Math.Min(f.ColumnCount,6)     
    let header = 
        f.ColumnKeys 
        |> Seq.take maxColsIdx
        |> Seq.append ["RowKey"]
    f
    |> Frame.take 10
    |> Frame.sliceCols header
    |> fun f' -> 
        if f'.ColumnCount < f.ColumnCount then 
            f'
            |> Frame.addCol "..." (f'.RowKeys |> Seq.map (fun ck -> ck,"...") |> Series.ofObservations)
        else 
            f'
    
    

let formatAsTable maxRows maxCols (f:Frame<_,_>) =
    let maxColsIdx = System.Math.Min(f.ColumnCount,maxCols)     
    let maxRowIdx = System.Math.Min(f.RowCount,maxRows) 
    let header = 
        f.ColumnKeys 
        |> Seq.take maxColsIdx
        |> Seq.append ["RowKey"]
    let f' =
        f
        |> Frame.sliceCols header
        |> fun f' -> 
            if f'.ColumnCount < f.ColumnCount then 
                f'
                |> Frame.addCol "..." (f'.RowKeys |> Seq.map (fun ck -> ck,"...") |> Series.ofObservations)
            else 
                f'

    let columnWidth = 
        let headerLength = 
            header 
            |> Seq.map (fun (x:string) -> (x.Length*10) + 10)
        let colLenght    =
            f'
            |> Frame.getCols 
            |> Series.values 
            |> Seq.map (fun s -> 
                s 
                |> Series.values 
                |> Seq.map (string >> String.length >> float) 
                |> Seq.average 
                |> int
                )
        Seq.map2 (fun (x:int) (y:int) -> System.Math.Max(x,y)) headerLength colLenght
    let rows = 
        f'    
        |> Frame.mapRows (fun k s -> s.As<string>() |> Series.values |> Seq.append [k.ToString()])
        |> Series.values
        |> Seq.take maxRowIdx
    Chart.Table(
        header,
        //rows
        rows,
        //sets global header alignment
        AlignHeader = [StyleParam.HorizontalAlign.Center],
        //sets alignment for each column separately 
        //(The last alignment is applied to all potential following columns)
        //AlignCells  = [HorizontalAlign.Left;HorizontalAlign.Center;HorizontalAlign.Right],
        AlignCells  = [StyleParam.HorizontalAlign.Center],
        //sets global header color
        ColorHeader = "#45546a",    
        //sets specific color to each header column
        //ColorHeader=["#45546a";"#deebf7";"#45546a";"#deebf7"],    
        //sets global cell color
        //ColorRows = "#deebf7",
        //sets cell column colors
        ColorCells  = (header |> Seq.mapi (fun i x -> if i%2 = 0 then  "#deebf7" else "lightgrey")),
        //sets cell row colors
        //ColorCells=[["#deebf7";"lightgrey"]],
        //sets font of header
        FontHeader  = Font.init(StyleParam.FontFamily.Courier_New, Size=12., Color="white"),      
        //sets the height of the header
        HeightHeader= 30.,
        //sets lines of header
        LineHeader  = Line.init(2.,"black"),                    
        ColumnWidth = columnWidth      
        //defines order of columns
        //ColumnOrder = [1;2;3;4]                                  
        )
    |> Chart.withSize((columnWidth |> Seq.sum |> float |> (*) 2.),500.)


open TMEA
open TMEA.Plots

let noResultPlot (id:string) =
    Chart.Point([]) 
    |> Chart.withTitle (sprintf "No TMEAResult found for id %s" id)
    |> GenericChart.toFigure

type TMEAResultCache () =
    inherit DynamicObj()

    static member getResult (id:string) (cache:TMEAResultCache) =
        cache.TryGetTypedValue<TMEA.TMEAResult> id

    static member cacheResult  (cache:TMEAResultCache) (res:TMEA.TMEAResult) =
        let guid = System.Guid.NewGuid().ToString()
        res |> DynObj.setValue cache guid
        guid
                           
    static member getPlotFor (id:string) (plotF:TMEAResult -> GenericChart.Figure) (cache:TMEAResultCache) =
        let res = cache |> TMEAResultCache.getResult id
        let handleResult (plotF:TMEAResult -> GenericChart.Figure) (id:string) (res:TMEAResult option) = 
            res
            |> Option.map plotF
            |> fun f ->
                match f with
                | Some f -> f
                | _ -> noResultPlot id
        res
        |> handleResult plotF id 

let getFASWeightDistributionPlot (alphaLevel:float) (constraints:seq<int>) (fasName:string) (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateFASWeightDistributionPlot(fasName,constraints,AlphaLevel=alphaLevel)
        |> GenericChart.toFigure
    )

let getConstraintTimecoursePlot (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateConstraintPotentialTimeCoursePlot()
        |> Chart.withSize(1000.,500.)
        |> GenericChart.toFigure
    )

let getPotentialHeatmapPlot (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateConstraintPotentialHeatmap()
        |> Chart.withSize(1000.,500.)
        |> GenericChart.toFigure
    )

let getFreeEnergyLandscapePlot (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateFreeEnergyLandscapePlot true
        |> Chart.withSize(1000.,500.)
        |> GenericChart.toFigure
    )

let getConstraintImportancePlot (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateConstraintImportancePlot true
        |> GenericChart.toFigure
    )

let getDataRecoveryPlot (constraintCutoff:int) (resultId:string) (cache:TMEAResultCache) =
    cache
    |> TMEAResultCache.getPlotFor resultId (fun res ->
        res 
        |> TMEAResult.generateDataRecoveryPlot(true,constraintCutoff)
        |> GenericChart.toFigure
    )