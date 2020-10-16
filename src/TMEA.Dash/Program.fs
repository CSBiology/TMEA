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

    let formatFrame f =
        (f :> Deedle.Internal.IFsiFormattable).Format()

//----------------------------------------------------------------------------------------------------
//============================================== LAYOUT ==============================================
//----------------------------------------------------------------------------------------------------


//The layout describes the components that Dash will render for you. 
open Dash.NET.HTML // this namespace contains the standard html copmponents, such as div, h1, etc.
open Dash.NET.DCC  // this namespace contains the dash core components, the heart of your Dash.NET app.

open HTMLPropTypes
open ComponentPropTypes

let myChart = 
    Graph.graph "my-graph" [
        Graph.Figure (Helpers.createWorldHighlightFigure "Germany")
    ] []

let formInput inputId inputType label helpText  =
    Div.div [ClassName "field"] [
        Label.label [ClassName "label"] [str label]
        Div.div [ClassName "control"] [
            Input.input inputId [Input.Type inputType; Input.ClassName "input"] []
        ]
        P.p [ClassName "help"] [str helpText]
    ]

let formSelect (selectId:string) label helpText (options:seq<string>) =
    Div.div [ClassName "field"] [
        Label.label [ClassName "label"] [str label]
        Div.div [ClassName "control"] [
            Select.select [ClassName "select"; Custom ("Id",box selectId)] (
                options
                |> Seq.map (fun opt ->
                    Option.option [Custom ("value",box opt)] [str opt]
                )
            )
        ]
        P.p [ClassName "help"] [str helpText]
    ]

let uploadStyle = DashComponentStyle()
uploadStyle?("width")<-"100%"
uploadStyle?("height")<-"60px"
uploadStyle?("lineHeight")<-"60px"
uploadStyle?("borderWidth")<-"1px'"
uploadStyle?("borderStyle")<-"dashed"
uploadStyle?("borderRadius")<-"5px"
uploadStyle?("textAlign")<-"center"

let formComponent labelText helpText (children:seq<DashComponent>) =
    Div.div [ClassName "field"] [
        Label.label [ClassName "label"] [str labelText]
        Div.div [ClassName "control"] children
        P.p [ClassName "help"] [str helpText]
    ]

let dataInput = 
    Div.div [] [
        H1.h1 [ClassName "title has-text-centered"] [str "TMEA - Thermodynamically Motivated Enrichment Analysis"]
        Div.div [ClassName "content"] [ 
            P.p [ClassName "has-text-centered"] [str "This is a simple example Dash.NET app that contains an input component, A world map graph, and a callback that highlights the country you type on that graph."]
        
        ]
        Div.div [ClassName "section"; Custom ("Id",box "experimental-data-section")] [
            H2.h2 [ClassName "title"] [str "Experimental data"]
            Br.br [] []
            Div.div [ClassName "columns"] [
                Div.div [ClassName "column is-4"] [
                    formComponent "Separator" "Select the separator that separates the data in your file" [
                        Dropdown.dropdown "frame-seperator-dropdown" [
                            Dropdown.Multi false
                            Dropdown.Placeholder "Select the separator that separates the data in your file"
                            Dropdown.Options [
                                DropdownOption.create "Comma (e.g. for .csv files)" "," false "Comma (e.g. for .csv files)"
                                DropdownOption.create "Tab (e.g. for .txt or .tsv files)" "\t" false "Tab (e.g. for .txt or .tsv files)"
                            ]
                        ] []
                    ]
                    formComponent "Data" "Upload the input experimental data for your TMEA workflow." [
                        Upload.upload "frame-upload" [
                            Upload.Style uploadStyle
                            Upload.Multiple false
                        ] [
                            A.a [] [str "Drag and Drop or select file"]
                        ]
                    ]
                ]
                Div.div [ClassName "column is-8"] [
                    Div.div [ClassName "field"] [
                        Label.label [ClassName "label"] [str "Dataframe preview:"]
                        Pre.pre [Custom("Id", box "frame-preview")] [
                            str "Data preview will be rendered here"
                        ]
                    ]
                ]
            ] 
        ]
        Div.div [ClassName "section"; Custom ("Id",box "ontology-map-section")] [
            H2.h2 [ClassName "title"] [str "Ontology map"]
            Br.br [] []
            Div.div [ClassName "columns"] [
                Div.div [ClassName "column is-4"] [
                    formComponent "Separator" "Select the separator that separates the data in your file" [
                        Dropdown.dropdown "ontology-map-seperator-dropdown" [
                            Dropdown.Multi false
                            Dropdown.Placeholder "Select the separator that separates the data in your file"
                            Dropdown.Options [
                                DropdownOption.create "Comma (e.g. for .csv files)" "," false "Comma (e.g. for .csv files)"
                                DropdownOption.create "Tab (e.g. for .txt or .tsv files)" "\t" false "Tab (e.g. for .txt or .tsv files)"
                            ]
                        ] []
                    ]
                    formComponent "Ontology map" "Upload the ontology map file that contains the functional annotations for your dataset" [
                        Upload.upload "ontology-map-upload" [
                            Upload.Style uploadStyle
                            Upload.Multiple false
                        ] [
                            A.a [] [str "Drag and Drop or select file"]
                        ]
                    ]
                ]
                Div.div [ClassName "column is-8"] [
                    Div.div [ClassName "field"] [
                        Label.label [ClassName "label"] [str "Ontology map preview:"]
                        Pre.pre [Custom("Id", box "ontology-map-preview")] [
                            str "Ontology map preview will be rendered here"
                        ]
                    ]
                ]
            ] 
        ]
    ] 

let mainView =
    Tabs.tabs "main-tabs" [] [
        Tab.tab "dataInput" [Tab.Label "Data input"] [
            dataInput
        ]
        Tab.tab "resultValitation" [Tab.Label "Result validation"] [
            Dropdown.dropdown "country-select" [
                Dropdown.Options [
                    DropdownOption.create "Canada" "Canada" false "Canada"
                    DropdownOption.create "Austria" "Austria" false "Austria"
                ]
            ] []
            myChart
        ]
        Tab.tab "tmeaResults" [Tab.Label "TMEA results"] [

        ]
    ]

//----------------------------------------------------------------------------------------------------
//============================================= Callbacks ============================================
//----------------------------------------------------------------------------------------------------

let separatorOfOption s =
    match s with
    | "\t" -> "\t"
    | s -> s

let countryCallback = 
    Callback.create
        [|
            CallbackInput.create("country-select","value")
        |]
        (CallbackOutput.create("my-graph","figure"))
        (fun (country:string) -> Helpers.createWorldHighlightFigure country)

let createDataFrameUploadCallback (uploadId:string) (seperatorId:string) (previewId:string)=
    Callback.create
        [|
            CallbackInput.create(uploadId,"contents")
            CallbackInput.create(seperatorId,"value")
        |]
        (CallbackOutput.create(previewId,"children"))
        (fun (encodedString:string) (separatorOption:string) ->
            encodedString
            |> Helpers.decodeBase64
            |> Helpers.stringAsFrame (separatorOfOption separatorOption)
            |> Figures.getSmallPreview 
            |> Helpers.formatFrame
        )

let framePreviewCallback        = createDataFrameUploadCallback "frame-upload" "frame-seperator-dropdown" "frame-preview"
let ontologyMapPreviewCallback  = createDataFrameUploadCallback "ontology-map-upload" "ontology-map-seperator-dropdown" "ontology-map-preview"


//----------------------------------------------------------------------------------------------------
//============================================= The App ==============================================
//----------------------------------------------------------------------------------------------------

//The 'DashApp' type is your central DashApp that contains all settings, configs, the layout, styles, 
//scripts, etc. that makes up your Dash.NET app. 

let myDashApp =
    DashApp.initDefault() // create a Dash.NET app with default settings
    |> DashApp.withLayout mainView // register the layout defined above.
    |> DashApp.addCSSLinks [ 
        "main.css" // serve your custom css
        "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.9.1/css/bulma.min.css" // register bulma as an external css dependency
    ]
    |> DashApp.withCallbackHandler("frame-preview.children",framePreviewCallback)
    |> DashApp.withCallbackHandler("ontology-map-preview.children",ontologyMapPreviewCallback)
    |> DashApp.withCallbackHandler("my-graph.figure",countryCallback)

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
        .UseGiraffe(DashApp.toWebApp myDashApp)

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