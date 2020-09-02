namespace TMEA


[<RequireQualifiedAccess>]
module Plots =
    open FSharp.Stats
    open FSharp.Plotly
    open FSharpAux

    module Presets =

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
        
        ///generates a Plot object containing the constraint time courses of the given Surprisal Analysis result
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


        ///generates a Plot object containing the constraint time courses of the given Surprisal Analysis result and renders it in the browser
        let plotConstraintTimecourses (useStylePreset:bool) (saRes:SurprisalAnalysis.SAResult) =
             generateConstraintTimeCoursePlot useStylePreset saRes
             |> Chart.Show

        ///generates a Plot object containing the free energy landscape of the given Surprisal Analysis result
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
                    |> Chart.withX_AxisStyle "TP"
                    |> Chart.withY_AxisStyle "Free Energy / k<sub>b</sub>T"
                    |> Chart.withTitle "Free Energy Landscape"

        ///generates a Plot object containing the free energy landscape of the given Surprisal Analysis result and renders it in the browser
        let plotFreeEnergyLandscape (useStylePreset:bool) (data:float [] []) (saRes:SurprisalAnalysis.SAResult) =
            generateFreeEnergyLandscapePlot useStylePreset data saRes
            |> Chart.Show
        
