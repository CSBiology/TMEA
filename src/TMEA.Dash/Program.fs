module Dash.NET.POC.App

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.ModelBinding

open Dash.NET
open Plotly.NET


module Helpers = 

    open Deedle
    open System.Text

    ///returns a choropleth plot that has the input country highlighted.
    let createWorldHighlightFigure countryName =
        Chart.ChoroplethMap(locations=[countryName],z=[100],Locationmode = StyleParam.LocationFormat.CountryNames)
        |> Chart.withMapStyle(
            ShowLakes=true,
            ShowOcean=true,
            OceanColor="lightblue",
            ShowRivers=true
        )
        |> Chart.withSize (1000.,1000.)
        |> Chart.withLayout (Layout.init(Paper_bgcolor="rgba(0,0,0,0)",Plot_bgcolor="rgba(0,0,0,0)"))
        |> GenericChart.toFigure

    let decodeBase64 (encodedString:string) =
        Convert.FromBase64String
            (
                encodedString
                    .Replace("data:application/octet-stream;base64,","")
                    .Replace("data:text/plain;base64,","")
            )
        |> Encoding.UTF8.GetString

    let stringAsFrame (separators:string) (s:string) =
        let byteArray = Encoding.UTF8.GetBytes(s)
        use stream = new MemoryStream(byteArray)
        Frame.ReadCsv(stream,true,separators=separators)

    let stringAsBytes (s:string) =
        Encoding.UTF8.GetBytes(s)


    let formatFrame f =
        (f :> Deedle.Internal.IFsiFormattable).Format()

//----------------------------------------------------------------------------------------------------
//============================================= Callbacks ============================================
//----------------------------------------------------------------------------------------------------


let separatorOfOption s =
    match s with
    | "\t" -> "\t"
    | s -> s

let createResultChangedDispatch output =
    Callback(
        [CallbackInput.create("tmea-result-store","data")],
        CallbackOutput.create(output),
        (fun (data:string) ->
            false
        )
    )

let createDataFrameUploadCallback (uploadId:string) (seperatorId:string) (previewId:string) =
    Callback(
        [
            CallbackInput.create(uploadId,"contents")
        ],
        CallbackOutput.create(previewId,"children"),
        (fun (encodedString:string) (separatorOption:string) ->
            encodedString
            |> Helpers.decodeBase64
            |> Helpers.stringAsFrame (separatorOfOption separatorOption)
            |> Figures.getSmallPreview 
            |> Helpers.formatFrame
        ),
        State=[
            CallbackState.create(seperatorId,"value")   
        ]
    )

let framePreviewCallback        = createDataFrameUploadCallback "frame-upload"          "frame-seperator-dropdown"          "frame-preview"
let ontologyMapPreviewCallback  = createDataFrameUploadCallback "ontology-map-upload"   "ontology-map-seperator-dropdown"   "ontology-map-preview"

//this would be in the same callback as above if multi would work
let createPupulateHeaderSelectionCallback (uploadId:string) (seperatorId:string) (dropdownID:string) =
    Callback(
        [
            CallbackInput.create(uploadId,"contents")
        ],
        CallbackOutput.create(dropdownID,"options"),
        (fun (encodedString:string) (separatorOption:string) ->
            encodedString
            |> Helpers.decodeBase64
            |> Helpers.stringAsFrame (separatorOfOption separatorOption)
            |> fun f -> f.ColumnKeys
            |> Array.ofSeq
            |> Array.map (fun x -> ComponentPropTypes.DropdownOption.create x x false x)
        ),
        State = [
            CallbackState.create(seperatorId,"value")
        ]
    )

let frameHeaderIdSelectCollBack                 = createPupulateHeaderSelectionCallback "frame-upload"          "frame-seperator-dropdown"          "frame-id-col"
let ontologyMapHeaderIdSelectCollBack           = createPupulateHeaderSelectionCallback "ontology-map-upload"   "ontology-map-seperator-dropdown"   "ontology-map-id-col"
let ontologyMapHeaderAnnotationSelectCollBack   = createPupulateHeaderSelectionCallback "ontology-map-upload"   "ontology-map-seperator-dropdown"   "ontology-map-annotation-col"


let serverSideResultCache = Figures.TMEAResultCache()

let startComputationCallback =
    Callback(
        [
            CallbackInput.create("main-start-btn","n_clicks")
        ],
        CallbackOutput.create("tmea-result-store","data"),
        (fun (clicks:IConvertible) (frameData:string) (frameSeparator:string) (frameIdCol:string) (omData:string) (omSeparator:string) (omIdCol:string) (omAnnCol:string) ->
            let dataFrame = 
                frameData
                |> Helpers.decodeBase64
                |> Helpers.stringAsBytes 
                |> fun bytes ->
                    TMEA.IO.readDataFrameFromStream frameIdCol frameSeparator bytes

            let ontologyMap = 
                omData
                |> Helpers.decodeBase64
                |> Helpers.stringAsBytes 
                |> fun bytes ->
                    use stream = new MemoryStream(bytes)
                    TMEA.IO.readOntologyMapFromStream stream omSeparator omIdCol omAnnCol

            FSharp.Stats.Algebra.LinearAlgebra.Service() |> ignore
            let tmeaParams = TMEA.TMEAParameters.create "not assigned" 99 true
            TMEA.Analysis.computeOfDataFrame tmeaParams ontologyMap dataFrame
            |> Figures.TMEAResultCache.cacheResult serverSideResultCache
        ),
        State = [

            CallbackState.create("frame-upload","contents")
            CallbackState.create("frame-seperator-dropdown","value")
            CallbackState.create("frame-id-col","value")

            CallbackState.create("ontology-map-upload","contents")
            CallbackState.create("ontology-map-seperator-dropdown","value")
            CallbackState.create("ontology-map-id-col","value")
            CallbackState.create("ontology-map-annotation-col","value")
        ]
    )

let constraintImportanceCallback =
    Callback (
        [CallbackInput.create("tmea-result-store","data")],
        CallbackOutput.create("constraint-importance-figure","figure"),
        (fun (resId:string) -> serverSideResultCache |> Figures.getConstraintImportancePlot resId)
    )

let dataRecoveryCallback =
    Callback (
        [
            CallbackInput.create("tmea-result-store","data")
            CallbackInput.create("data-recovery-cutoff","value")
        ],
        CallbackOutput.create("data-recovery-figure","figure"),
        (fun (resId:string) (cutoff:int64) -> 
            serverSideResultCache |> Figures.getDataRecoveryPlot (int cutoff) resId)
    )

let constraintTimeCourseCallback =
    Callback(
        [CallbackInput.create("tmea-result-store","data")],
        CallbackOutput.create("constraint-time-course","figure"),
        (fun (resId:string) -> serverSideResultCache |> Figures.getConstraintTimecoursePlot resId)
    )

let constraintHeatmapCallback =
    Callback(
        [CallbackInput.create("tmea-result-store","data")],
        CallbackOutput.create("constraint-heatmap","figure"),
        (fun (resId:string) -> serverSideResultCache |> Figures.getPotentialHeatmapPlot resId)
    )

let energyLandscapeCallback =
    Callback(
        [CallbackInput.create("tmea-result-store","data")],
        CallbackOutput.create("energy-landscape","figure"),
        (fun (resId:string) -> serverSideResultCache |> Figures.getFreeEnergyLandscapePlot resId)
    )
    
let fasWeightDistributionCallback =
    Callback(
        [CallbackInput.create("fas-weight-distribution-trigger","n_clicks")],
        CallbackOutput.create("fas-weight-distribution","figure"),
        (fun (clicks:IConvertible) (fasName:string) (resId:string) (alphaLevel:string) (constraints:string) -> 
            let constraints' = constraints.Split(',') |> Array.map (fun x -> x.Trim() |> int)
            let alpha = float alphaLevel
            serverSideResultCache 
            |> Figures.getFASWeightDistributionPlot alpha constraints' fasName resId

        ),
        State = [
            CallbackState.create("fas-weight-distribution-fasname","value")
            CallbackState.create("tmea-result-store","data")
            CallbackState.create("fas-weight-distribution-alphalevel","value")
            CallbackState.create("fas-weight-distribution-constraints","value")
        ]
    )


//----------------------------------------------------------------------------------------------------
//============================================= The App ==============================================
//----------------------------------------------------------------------------------------------------

//The 'DashApp' type is your central DashApp that contains all settings, configs, the layout, styles, 
//scripts, etc. that makes up your Dash.NET app. 

let myDashApp =
    DashApp.initDefault() // create a Dash.NET app with default settings
    |> DashApp.withLayout Layout.mainView // register the layout defined above.
    |> DashApp.appendCSSLinks [ 
        "main.css" // serve your custom css
        "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css" // register bulma as an external css dependency
    ]
    |> DashApp.addCallback framePreviewCallback
    |> DashApp.addCallback ontologyMapPreviewCallback
    //col header selectors
    |> DashApp.addCallback frameHeaderIdSelectCollBack
    |> DashApp.addCallback ontologyMapHeaderIdSelectCollBack
    |> DashApp.addCallback ontologyMapHeaderAnnotationSelectCollBack
    //state change due to result being finished
    |> DashApp.addCallback (createResultChangedDispatch("resultValidation","disabled"))
    |> DashApp.addCallback (createResultChangedDispatch("tmeaResults","disabled"))
    //Result generation
    |> DashApp.addCallback startComputationCallback
    //Plot callbacks
    |> DashApp.addCallback constraintImportanceCallback
    |> DashApp.addCallback dataRecoveryCallback
    |> DashApp.addCallback constraintTimeCourseCallback
    |> DashApp.addCallback constraintHeatmapCallback
    |> DashApp.addCallback energyLandscapeCallback
    |> DashApp.addCallback fasWeightDistributionCallback

// The things below are Giraffe/ASP:NetCore specific and will likely be abstracted in the future.

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.EnvironmentName with
    | "Development" -> app.UseDeveloperExceptionPage()
    | _ -> app.UseGiraffeErrorHandler(errorHandler))
        .UseHttpsRedirection()
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(DashApp.toHttpHandler myDashApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddFilter(fun l -> l.Equals LogLevel.Debug)
           .AddConsole()
           .AddDebug() |> ignore

[<EntryPoint>]
let main args =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(Action<IApplicationBuilder> configureApp)
                    .ConfigureServices(configureServices)
                    .ConfigureLogging(configureLogging)
                    |> ignore)
        .Build()
        .Run()
    0