namespace TMEA


[<RequireQualifiedAccess>]
module Plots =
    open FSharp.Stats
    open FSharp.Plotly
    open FSharpAux

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

        

    open FSharp.Stats.ML
    open TMEA.SurprisalAnalysis

    module SurprisalAnalysis =
        
        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result
        let generateConstraintTimeCoursePlot (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
            saRes.Potentials
            |> Matrix.toJaggedArray
            |> Array.mapi (fun i x -> Chart.Line((x |> Array.indexed), Name = (sprintf "C_%i" i)))
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
        let plotConstraintTimecourses (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
             generateConstraintTimeCoursePlot useStylePreset saRes
             |> Chart.Show

        ///generates a Chart object containing the free energy landscape of the given Surprisal Analysis result
        let generateFreeEnergyLandscapePlot (useStylePreset:bool) (data:float [] []) (saRes:SurprisalAnalysis.SAResult) =
            let freeEnergies =
                [|1..data.Length-1|]
                |> Array.map (fun cI ->
                    calculate_FreeEnergyTimeCourse_ForConstraint cI data saRes
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
        let plotFreeEnergyLandscape (useStylePreset:bool) (data:float [] []) (saRes:SurprisalAnalysis.SAResult) =
            generateFreeEnergyLandscapePlot useStylePreset data saRes
            |> Chart.Show
        
        ///generates a Chart object containing the constraint time courses of the given Surprisal Analysis result via heatmap. omits the baseline state.
        let generatePotentialHeatmap (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
            saRes.Potentials
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
        let plotPotentialHeatmap (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
            generatePotentialHeatmap useStylePreset saRes
            |> Chart.Show

        ///generates a Chart object containing charts to help with selection of an importance threshold for constraints of the given surprisal analysis result.
        let generateConstraintImportancePlot (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
            [
                saRes.SingularValues 
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
            
                saRes.SingularValues 
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
        let plotConstraintImportance (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
            generateConstraintImportancePlot useStylePreset saRes
            |> Chart.Show

        ///generates a Chart object containing a plot that shows the gradual reconstruction of the original data when using only n constraints from the given Surprisal Analysis result.
        let generateDataRecoveryPlot (useStylePreset:bool) (constraintCutoff:int) (data:float[][]) (saRes:SurprisalAnalysis.SAResult) =
            
            let patterns =
                saRes.MolecularPhenotypes
                |> Matrix.toJaggedArray
                |> JaggedArray.transpose

            let potentials = 
                saRes.Potentials
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
                |> fun x ->  Array.zip x (data |> Array.concat)
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
        let plotDataRecovery (useStylePreset:bool) (constraintCutoff:int) (data:float[][]) (saRes:SurprisalAnalysis.SAResult) =
            generateDataRecoveryPlot useStylePreset constraintCutoff data saRes
            |> Chart.Show