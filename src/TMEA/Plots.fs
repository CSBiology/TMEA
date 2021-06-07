namespace TMEA

module Plots =
    open FSharp.Stats
    open Plotly.NET
    open FSharpAux
    open TMEA.MonteCarlo
    open TMEA.Frames
    open TMEA.SurprisalAnalysis

    module Presets =

        let colorscale = StyleParam.Colorscale.Custom([
            0.,"SteelBlue";
            0.45,"LavenderBlush";
            1.,"Salmon"
        ])

        let standardConfig =
            Config.init (
                Responsive = true,
                ToImageButtonOptions = ToImageButtonOptions.init(
                    StyleParam.ImageFormat.SVG
                )
            )

        let presetAxis title = Axis.LinearAxis.init(Title=title,Mirror=StyleParam.Mirror.All,Ticks=StyleParam.TickOptions.Inside,Showgrid=false,Showline=true,Zeroline=true)
        
        let applyPresetStyle xName yName chart = chart |> Chart.withX_Axis (presetAxis xName) |> Chart.withY_Axis (presetAxis yName)

        let styleHistChartAxis (cI:int) title c =
            c
            |> applyPresetStyle "FASWeight" (sprintf "<b>C%i</b><br></br>Probability" cI)
            |> Chart.withSize (1000.,1000.)
            |> Chart.withTitle title

        let internal generateFASWeightDistributionPlot (useStylePreset:bool) (alphaLevel:float) (constraintIndex: int) (fasName:string) (tmeaRes:TMEAResult) =

            let posDesc,negDesc =
                tmeaRes.Characterizations.[constraintIndex].PositiveDescriptor
                |> Array.find (fun x -> x.OntologyTerm = fasName)
                ,
                tmeaRes.Characterizations.[constraintIndex].NegativeDescriptor
                |> Array.find (fun x -> x.OntologyTerm = fasName)

            let posPValCorrected , negPValCorrected =
                tmeaRes.Characterizations.[constraintIndex].PositiveDescriptor
                |> Array.map (fun x -> x.OntologyTerm,x.PValue)
                |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id
                |> List.find (fun (id,pVal) -> id = fasName)
                |> snd
                ,
                tmeaRes.Characterizations.[constraintIndex].NegativeDescriptor
                |> Array.map (fun x -> x.OntologyTerm,x.PValue)
                |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id
                |> List.find (fun (id,pVal) -> id = fasName)
                |> snd

            let isSigPos,isSigNeg =
                posPValCorrected < alphaLevel,
                negPValCorrected < alphaLevel

            let binData = WeightDistributions.getWeightDataForFAS constraintIndex fasName tmeaRes

            let allPosWeightDist =
                tmeaRes.Characterizations.[constraintIndex].RawData
                |> Array.map (fun x -> x.Item)
                |> Array.filter (fun x -> x >= 0.)
                |> Distributions.Empirical.create 0.0015 
                |> Distributions.Empirical.normalize
                |> Map.toArray

            let allNegWeightDist = 
                tmeaRes.Characterizations.[constraintIndex].RawData
                |> Array.map (fun x -> x.Item)
                |> Array.filter (fun x -> x <= 0.)
                |> Distributions.Empirical.create 0.0015 
                |> Distributions.Empirical.normalize
                |> Map.toArray

            let posWeightDist  = 
                binData.PosDist
                |> Array.map snd
                |> Distributions.Empirical.create 0.0015 
                |> Distributions.Empirical.normalize
                |> Map.toArray

            let negWeightDist  = 
                binData.NegDist
                |> Array.map snd
                |> Distributions.Empirical.create 0.0015 
                |> Distributions.Empirical.normalize
                |> Map.toArray

            let maxY = 
                match (posWeightDist.Length, negWeightDist.Length) with
                |(p,n) when n > 0 && p > 0  -> max (posWeightDist |> Array.maxBy snd |> snd) (negWeightDist |> Array.maxBy snd |> snd)
                |(0,n) when n > 0           -> (negWeightDist |> Array.maxBy snd |> snd)
                |(p,0) when p > 0           -> (posWeightDist |> Array.maxBy snd |> snd)
                |_                          -> 0.

            let posAnn, negAnn =
                Annotation.init(
                    0.06,
                    maxY*0.6,
                    XRef=(
                        StyleParam.AxisAnchorId.X 2 
                        |> StyleParam.AxisAnchorId.toString ),
                    YRef=(
                        StyleParam.AxisAnchorId.Y constraintIndex 
                        |> StyleParam.AxisAnchorId.toString),
                    ShowArrow=false,
                    BorderColor="ashgray",
                    HorizontalAlign=StyleParam.HorizontalAlign.Left,
                    Text =(
                        let binDataText = (
                            sprintf "<br></br><b>BinSize</b>: %i<br></br><b>WeightSum</b>: %.5f<br></br><b>PVal</b>: %.5f<br></br><b>CorrectedPVal</b>: %.5f<br></br>" posDesc.BinSize posDesc.WeightSum posDesc.PValue posPValCorrected)
                        let header= 
                            if isSigPos then
                                (sprintf "[+]<b>Significant(@%.5f)</b> in C%i" alphaLevel constraintIndex)
                            else
                                (sprintf "[+]<b>Not</b> significant (@%.5f) in C%i" alphaLevel constraintIndex)
                        header + binDataText
                    ),
                    BGColor = if isSigPos then "lightgreen" else "rgba(240,128,128,0.5)"
                ),
                Annotation.init(
                    -0.06,
                    maxY*0.6,
                    XRef=(
                        StyleParam.AxisAnchorId.X 1 
                        |> StyleParam.AxisAnchorId.toString
                    ),
                    YRef=(
                        StyleParam.AxisAnchorId.Y constraintIndex 
                        |> StyleParam.AxisAnchorId.toString
                    ),
                    ShowArrow=false,
                    BorderColor="ashgray",
                    HorizontalAlign=StyleParam.HorizontalAlign.Left,
                    Text= (
                        let binDataText = (sprintf "<br></br><b>BinSize</b>: %i<br></br><b>WeightSum</b>: %.5f<br></br><b>PVal</b>: %.5f<br></br><b>CorrectedPVal</b>: %.5f<br></br>" negDesc.BinSize negDesc.WeightSum negDesc.PValue negPValCorrected)
                        let header= 
                            if isSigNeg then
                                (sprintf "[-]<b>Significant</b> in C%i" constraintIndex)
                            else
                                (sprintf "[-]<b>Not</b> significant in C%i" constraintIndex)
                        header + binDataText
                    ),
                    BGColor = if isSigNeg then "lightgreen" else "rgba(240,128,128,0.5)"
                )
            
            [
                [
                    Chart.Area(allNegWeightDist,Color="ashgray",Name=(sprintf "[-]C%i_AllNeg" constraintIndex),Opacity=0.5)
                    Chart.Column(negWeightDist, Name = (sprintf "C%i_%s" constraintIndex fasName))
                    |> GenericChart.mapTrace(fun t -> t?width <- negWeightDist |> Array.map (fun x -> 0.0015 ); t)
                    |> Chart.withMarker(Marker.init(Color="SteelBlue", Line=Line.init(1.5,"black")))
                ]
                |> Chart.Combine
                |> Chart.withY_AxisStyle ("",MinMax=(0.,maxY))
                |> Chart.withX_AxisStyle ("", MinMax=(-0.1,0.))
                |> fun c -> 
                    if useStylePreset then
                        c
                        |> styleHistChartAxis constraintIndex ""
                    else
                        c

                [
                    Chart.Area(allPosWeightDist,Color="ashgray",Name=(sprintf "[+]C%i_AllPos" constraintIndex),Opacity=0.5)
                    Chart.Column(posWeightDist, Name =(sprintf "C%i_%s" constraintIndex fasName))
                    |> GenericChart.mapTrace(fun t -> t?width <- posWeightDist |> Array.map (fun x -> 0.0015 ); t)
                    |> Chart.withMarker(Marker.init(Color="salmon",Line=Line.init(1.5,"black")))
                ]
                |> Chart.Combine
                |> Chart.withY_AxisStyle ("",MinMax=(0.,maxY))
                |> Chart.withX_AxisStyle ("", MinMax=(0.,0.1))
                |> fun c -> 
                    if useStylePreset then
                        c
                        |> styleHistChartAxis constraintIndex ""
                    else
                        c

            ],
            [
                posAnn
                negAnn
            ]

        let internal generate_WeightDistPlot_GridChart (baseTitle:string) (constraints:seq<int>) (useStylePreset:bool) (alphaLevel:float) (fasName:string) (tmeaRes:TMEAResult) =
        
            let charts, anns = 
                constraints
                |> Seq.map (fun cI -> generateFASWeightDistributionPlot useStylePreset alphaLevel cI fasName tmeaRes)
                |> Seq.unzip
                |> fun (c,a) -> c , a|> List.concat 
        
            Chart.Grid (
                charts,
                true
            )
            |> Chart.withAnnotations anns
            |> fun c -> 
                if useStylePreset then
                    c
                    |> Chart.withTitle (sprintf "[%s] Bin Weight Distributions for <b>%s</b> in Constraints %A" baseTitle fasName constraints)
                    |> Chart.withLayoutGridStyle (XSide = StyleParam.LayoutGridXSide.Bottom)
                    |> Chart.withSize (1500.,(float (Seq.length charts) * 333.333))
                    |> Chart.withConfig standardConfig
                else
                    c


    type TMEAResult with
        
        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result
        static member generateConstraintTimeCoursePlot (tmeaRes:TMEAResult, ?UseStylePreset:bool, ?InvertConstraints:int []) =
            let useStylePreset = defaultArg UseStylePreset true
            let switchCons = defaultArg InvertConstraints [||]
            
            tmeaRes
            |> TMEAResult.invertSignsFor switchCons
            |> fun x -> x.ConstraintPotentials
            |> Matrix.toJaggedArray
            |> Array.mapi (fun i x -> 
                Chart.Line((x |> Array.indexed), Name = (sprintf "C_%i" i)))
            |> Chart.Combine
            |> fun c -> 
                if useStylePreset then
                    c 
                    |> Presets.applyPresetStyle "TP" "λ(t)"
                    |> Chart.withSize (1500.,1000.)
                    |> Chart.withTitle "Constraint Potential TimeCourse"
                    |> Chart.withConfig Presets.standardConfig
                else
                    c
                    |> Chart.withX_AxisStyle "TP"
                    |> Chart.withY_AxisStyle "λ(t)"
                    |> Chart.withTitle "Constraint Potential TimeCourse"

        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result and renders it in the browser
        static member plotConstraintTimecourses (tmeaRes:TMEAResult, ?UseStylePreset:bool, ?InvertConstraints:int [])=
             TMEAResult.generateConstraintTimeCoursePlot(tmeaRes,?UseStylePreset=UseStylePreset,?InvertConstraints=InvertConstraints)
             |> Chart.Show

        ///generates a Chart object containing the free energy landscape of the given Surprisal Analysis result
        static member generateFreeEnergyLandscapePlot (useStylePreset:bool) (tmeaRes:TMEAResult) =
            let freeEnergies =
                [|1..tmeaRes.Data.Length-1|]
                |> Array.map (fun cI ->
                    TMEAResult.calculate_FreeEnergyTimeCourse_ForConstraint cI tmeaRes
                )

            freeEnergies
            |> fun f ->
                let total = f |> JaggedArray.transpose  |> Array.map Array.sum
                f 
                |> Array.mapi (fun i x -> Chart.Spline((x |> Array.indexed), Name = (sprintf "C_%i" (i+1)), Smoothing=0.5))
                |> fun singles -> [
                    yield! singles
                    Chart.Spline((total |> Array.indexed), Name = "total", Smoothing=0.5, Color = "black")
                ]
            |> Chart.Combine
            |> fun c -> 
                if useStylePreset then
                    c
                    |> Presets.applyPresetStyle "TP" "Free Energy / k<sub>b</sub>T"
                    |> Chart.withSize (1500.,1000.)
                    |> Chart.withTitle "Free Energy Landscape"
                    |> Chart.withConfig Presets.standardConfig
                else
                    c
                    |> Chart.withX_AxisStyle "TimePoint"
                    |> Chart.withY_AxisStyle "Free Energy / k<sub>b</sub>T"
                    |> Chart.withTitle "Free Energy Landscape"

        ///generates a Chart object containing the free energy landscape of the given Surprisal Analysis result and renders it in the browser
        static member plotFreeEnergyLandscape (useStylePreset:bool) (tmeaRes:TMEAResult) =
            TMEAResult.generateFreeEnergyLandscapePlot useStylePreset tmeaRes
            |> Chart.Show
        
        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result via heatmap. omits the baseline state.
        static member generatePotentialHeatmap (tmeaRes:TMEAResult, ?UseStylePreset:bool, ?InvertConstraints:int []) =
            let useStylePreset = defaultArg UseStylePreset true
            let switchCons = defaultArg InvertConstraints [||]

            tmeaRes
            |> TMEAResult.invertSignsFor switchCons
            |> fun x -> x.ConstraintPotentials
            |> Matrix.toArray2D
            |> JaggedArray.ofArray2D
            |> Array.tail
            |> Array.rev
            |> fun potentials ->
                Chart.Heatmap(
                    potentials,
                    ColNames=[0 .. potentials.Length-1],
                    RowNames = ([1..potentials.Length] |> List.map (sprintf "Constraint_%i") |> List.rev),
                    Ygap=10,
                    Colorscale= Presets.colorscale,zSmooth=StyleParam.SmoothAlg.Best
                    )
            |> Chart.withColorBar (Colorbar.init(Title="Constraint Potential"))
            |> Chart.withTitle("PotentialTimeCourse")
            |> fun c -> 
                if useStylePreset then
                    c
                    |> Presets.applyPresetStyle "Time point" "Constraint index" 
                    |> Chart.withSize (1500.,1000.)
                    |> Chart.withMargin (Margin.init(Left=100))
                    |> Chart.withConfig Presets.standardConfig
                else
                    c
                       
        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result via heatmap and renders it in the browser. omits the baseline state.
        static member plotPotentialHeatmap (tmeaRes:TMEAResult, ?UseStylePreset:bool, ?InvertConstraints:int []) =
            TMEAResult.generatePotentialHeatmap(tmeaRes,?UseStylePreset=UseStylePreset,?InvertConstraints=InvertConstraints)
            |> Chart.Show

        ///generates a Chart object containing charts to help with selection of an importance threshold for constraints of the given surprisal analysis result.
        static member generateConstraintImportancePlot (useStylePreset:bool) (tmeaRes:TMEAResult) =
            [
                tmeaRes.SingularValues 
                |> Vector.toArray 
                |> Array.indexed
                |> Array.tail
                |> Array.mapi (fun i (cI,x) -> 
                    Chart.Column([(i+1),x])
                    |> Chart.withTraceName (sprintf "σ(C_%i)" (i+1))
                )
                |> Chart.Combine
                |> fun c -> 
                    if useStylePreset then
                        c
                        |> Presets.applyPresetStyle "ConstraintIndex α" "Singular Value σ(α)"
                    else
                        c
            
                tmeaRes.SingularValues 
                |> Vector.toArray 
                |> Array.tail
                |> fun a -> 
                    a 
                    |> Array.mapi (fun i x -> 
                        [(i+1) , (
                            if (i=0) then 
                                0. 
                            else
                                1. - (x/a.[i-1])
                        )]
                        |> fun c -> Chart.Column(c)
                        |> Chart.withTraceName (sprintf "σ loss (C_%i)" (i+1))
                
                    )
                |> Chart.Combine
                |> fun c -> 
                    if useStylePreset then
                        c
                        |> Presets.applyPresetStyle "ConstraintIndex α" "σ loss (1 - (σ(α) / σ(α-1))"
                    else
                        c
            ]
            |> List.rev
            |> fun x -> Chart.SingleStack (x,true)
            |> Chart.withTitle "Constraint importance"
            |> fun c -> 
                if useStylePreset then
                    c
                    |> Chart.withSize(750.,1000.)
                    |> Chart.withConfig Presets.standardConfig
                else
                    c
        
        ///generates a Chart object containing charts to help with selection of an importance threshold for constraints of the given surprisal analysis result and renders it in the browser.
        static member plotConstraintImportance (useStylePreset:bool) (tmeaRes:TMEAResult) =
            TMEAResult.generateConstraintImportancePlot useStylePreset tmeaRes
            |> Chart.Show

        ///generates a Chart object containing a plot that shows the gradual reconstruction of the original data when using only n constraints from the given Surprisal Analysis result.
        static member generateDataRecoveryPlot (useStylePreset:bool) (constraintCutoff:int) (tmeaRes:TMEAResult) =
            
            let patterns =
                tmeaRes.Constraints
                |> Matrix.toJaggedArray
                |> JaggedArray.transpose

            let potentials = 
                tmeaRes.ConstraintPotentials
                |> Matrix.toJaggedArray

            [0 .. constraintCutoff]
            |> List.map (fun recIndex ->
                patterns.[0..10]
                |> Array.mapi (fun i pattern ->
                    pattern
                    |> Array.map (fun g ->
                        potentials.[i] |> Array.map (fun p -> p * g)
                    )
                )
                |> fun x ->
                    let outerLen = x.[0].Length
                    let innerLen = x.[0].[0].Length
                    Array.init outerLen (fun outerI ->
                        Array.init innerLen (fun innerI ->
                            x.[0..recIndex]
                            |> Array.map (fun r ->
                                r.[outerI].[innerI]
                            )
                            |> Array.sum
                        )
                    )
                |> JaggedArray.transpose
                |> Array.concat
                |> fun x ->  Array.zip x (tmeaRes.Data |> Array.concat)
                |> fun xy -> 
                    let xy' = xy |> Array.filter (fun (x,y) -> not (nan.Equals(x) || nan.Equals(y) || infinity.Equals(-x) || infinity.Equals(-y) || infinity.Equals(x) || infinity.Equals(y)))
                    let x,y = xy' |> Array.unzip
                    let coeffs = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Polynomial.coefficient 1 (x|> vector) (y|> vector)
                    let fit = FSharp.Stats.Fitting.LinearRegression.OrdinaryLeastSquares.Linear.Univariable.fit coeffs
                    let sos = FSharp.Stats.Fitting.GoodnessOfFit.calculateSumOfSquares fit x y
                    let rSquared = FSharp.Stats.Fitting.GoodnessOfFit.calculateDetermination sos
            
                    xy,rSquared
            
                |> fun (xy,rS) -> 
                    Chart.Point(xy,UseWebGL=true,Name= (sprintf "C0-%i : R<sup>2</sup> = %.5f" recIndex rS)) 
                    |> Chart.withTitle (sprintf "R² = %.5f" rS)
                    |> Chart.withMarkerStyle (
                        Size = 3,
                        Opacity = 0.7,
                        Symbol = StyleParam.Symbol.Star
            
                    )
                |> fun c -> 
                    if useStylePreset then
                        c |> Presets.applyPresetStyle "&#8721;<sup>&#945;</sup>(&#955;<sub>&#945;</sub>G<sub>i&#945;</sub>)" "ln(X<sub>i</sub>)"
                    else
                        c
            )
            |> Chart.Combine
            |> Chart.withTitle (sprintf "Data recovery using Constraints 0-%i" constraintCutoff)
            |> fun c ->
                if useStylePreset then
                    c
                    |> Chart.withSize (1000.,1000.)
                    |> Chart.withConfig Presets.standardConfig
                else
                    c

        ///generates a Chart object containing a plot that shows the gradual reconstruction of the original data when using only n constraints from the given Surprisal Analysis result and renders it in the browser
        static member plotDataRecovery (useStylePreset:bool) (constraintCutoff:int) (tmeaRes:TMEAResult) =
            TMEAResult.generateDataRecoveryPlot useStylePreset constraintCutoff tmeaRes
            |> Chart.Show

        ///
        static member generateFASWeightDistributionPlot (useStylePreset:bool) (alphaLevel:float) (constraints: seq<int>) (fasName:string) (tmeaRes:TMEAResult) =
            Presets.generate_WeightDistPlot_GridChart
                "test"
                constraints
                useStylePreset 
                alphaLevel 
                fasName 
                tmeaRes        
                
        ///
        static member plotFASWeightDistribution (useStylePreset:bool) (alphaLevel:float) (constraints: seq<int>) (fasName:string) (tmeaRes:TMEAResult) =
            TMEAResult.generateFASWeightDistributionPlot
               useStylePreset 
               alphaLevel 
               constraints
               fasName 
               tmeaRes        
            |> Chart.Show

