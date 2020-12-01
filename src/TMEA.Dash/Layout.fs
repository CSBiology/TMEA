module Layout

open Dash.NET
open Dash.NET.HTML // this namespace contains the standard html copmponents, such as div, h1, etc.
open Dash.NET.DCC  // this namespace contains the dash core components, the heart of your Dash.NET app.

open HTMLPropTypes
open ComponentPropTypes

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

let selectHeader id searchable labelText helpText placeholder options =
    formComponent labelText helpText [
        Dropdown.dropdown id [
            Dropdown.Multi false
            Dropdown.Placeholder placeholder
            Dropdown.Options options
            Dropdown.Searchable searchable
        ] []
    ]

let frameInput = 
    [
        Div.div [ClassName "section"; Custom ("Id",box "experimental-data-section")] [
            H2.h2 [ClassName "title"] [str "Experimental data"]
            Br.br [] []
            Div.div [ClassName "columns"] [
                Div.div [ClassName "column is-4"] [
                    selectHeader "frame-seperator-dropdown" false "Separator" "Select the separator that separates the data in your file" "Select the separator that separates the data in your file" [
                        DropdownOption.create "Comma (e.g. for .csv files)" "," false "Comma (e.g. for .csv files)"
                        DropdownOption.create "Tab (e.g. for .txt or .tsv files)" "\t" false "Tab (e.g. for .txt or .tsv files)"
                    ]
                    formComponent "Data" "Upload the input experimental data for your TMEA workflow." [
                        Upload.upload "frame-upload" [
                            Upload.Style uploadStyle
                            Upload.Multiple false
                        ] [
                            A.a [] [str "Drag and Drop or select file"]
                        ]
                    ]
                    selectHeader "frame-id-col" true "Identifier Column" "Select the column header that defines the ids of the entities in your dataset" "Search for column header ..." []
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
    ]

let ontologyMapInput =
    [
        Div.div [ClassName "section"; Custom ("Id",box "ontology-map-section")] [
            H2.h2 [ClassName "title"] [str "Ontology map"]
            Br.br [] []
            Div.div [ClassName "columns"] [
                Div.div [ClassName "column is-4"] [
                    selectHeader "ontology-map-seperator-dropdown" false "Separator" "Select the separator that separates the data in your file" "Select the separator that separates the data in your file" [
                        DropdownOption.create "Comma (e.g. for .csv files)" "," false "Comma (e.g. for .csv files)"
                        DropdownOption.create "Tab (e.g. for .txt or .tsv files)" "\t" false "Tab (e.g. for .txt or .tsv files)"
                    ]
                    formComponent "Ontology map" "Upload the ontology map file that contains the functional annotations for your dataset" [
                        Upload.upload "ontology-map-upload" [
                            Upload.Style uploadStyle
                            Upload.Multiple false
                        ] [
                            A.a [] [str "Drag and Drop or select file"]
                        ]
                    ]
                    selectHeader "ontology-map-id-col"  true "Identifier Column" "Select the column header that defines the ids of the entities in your dataset (must have the same name as in the frame above)" "Search for column header ..." []
                    selectHeader "ontology-map-annotation-col" true "Functional annotation column" "Select the column header that defines the functional annotations of the entities in your dataset" "Search for column header ..." []
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

let resultValidation  =
    Div.div [ClassName "section"; Custom ("Id",box "result-validation-section")] [
        H2.h2 [ClassName "title"] [str "Quality control and Result Validation"]
        Br.br [] []
        Div.div [ClassName "columns"] [
            Div.div [ClassName "column is-6"] [
                H2.h2 [ClassName "title"] [str "Constraint importance"]
                H3.h3 [ClassName "subtitle"] [str "Use this plot to determine which constraints are important and which ar enot. More information here"]
                Graph.graph "constraint-importance-figure" [] []
            ]
            Div.div [ClassName "column is-6"] [
                H2.h2 [ClassName "title"] [str "Data recovery"]
                H3.h3 [ClassName "subtitle"] [str "You can use this plot to determine to what extend the original data can be recovered when using only a set amount of constraints. Recovery percentage is calculated as coefficient of determination (R²) of the linear regresion of the original data with the reversal of the surprisal analysis constraint when only using the amounts of constraints you selected."]
                formComponent "Constraint Cutoff" "Use this slider to determine which constraints should be used to calculate data recovery" [
                    Input.input "data-recovery-cutoff" [Input.ClassName "input"; Input.Type InputType.Number; Input.Value 0] []
                ]
                Graph.graph "data-recovery-figure" [] []
            ]
        ]
    ]

let tmeaResultGraphSection id title subtitle paramSelectors =
    let plotWidth, paramWidth = 
        if Seq.length paramSelectors = 0 then
            12,0
        else 
            8,4
    Div.div [ClassName "section"; Id (sprintf "%s-section" id)] [
        H2.h2 [ClassName "title"] [str title]
        H3.h3 [ClassName "subtitle"] [str subtitle]
        Div.div [ClassName "columns"] [
            Div.div [ClassName (sprintf "column is-%i" paramWidth)] paramSelectors
            Div.div [ClassName (sprintf "column is-%i" plotWidth )] [
                Graph.graph id [] []
            ]
        ]
    ]


let tmeaResults  =
    [
        tmeaResultGraphSection "constraint-time-course" 
            "Constraint time course" 
            "Use this plot to investigate how the importance of the constraints determined for your dataset evolve over time. The most interesting property of these plots are the sign changes of the plotted state variables, as they indicate state transitions."
            []
        tmeaResultGraphSection "constraint-heatmap" 
            "Constraint heatmap" 
            "An alternative way for visualization of constraint time courses. correlation between different state variables may be more obvious on this plot."
            []
        tmeaResultGraphSection "energy-landscape" 
            "Energy landscape" 
            "Use this plot to determine stable states of your system during the perturbation. Local energy minima generally indicate stable states. "
            []
        tmeaResultGraphSection "fas-weight-distribution" 
            "FAS weight distribution" 
            "you can use this plot to investigate the weight distribution of a FAS of choice in your dataset"
            [
                formComponent "FAS name" "the name of the FAS to investigate" [
                    Input.input "fas-weight-distribution-fasname" [Input.ClassName "input"; Input.Type InputType.Text] []
                ]
                formComponent "Alpha level" "p-value threshold for a FAS to be considered significant" [
                    Input.input "fas-weight-distribution-alphalevel" [Input.ClassName "input"; Input.Type InputType.Text; Input.Value "0.05"] []
                ]
                formComponent "Constraints" "Comma separated list of constraints to consider" [
                    Input.input "fas-weight-distribution-constraints" [Input.ClassName "input"; Input.Type InputType.Text; Input.Value "1,2,3"] []
                ]
                formComponent "" "" [
                    Button.button [ClassName "button is-primary"; Id "fas-weight-distribution-trigger"] [str "Go"]
                ]
            ]
    ]

let mainView =
    Div.div [] [
        Store.store "tmea-result-store" [] []
        Section.section [ClassName "hero is-small is-primary is-bold"] [
            Div.div [ClassName "hero-body"] [
                H1.h1 [ClassName "title has-text-centered"] [str "TMEA - Thermodynamically Motivated Enrichment Analysis"]
                Div.div [ClassName "content"] [ 
                    P.p [ClassName "has-text-centered"] [str "This is a simple example Dash.NET app that contains an input component, A world map graph, and a callback that highlights the country you type on that graph."]
                
                ]
            ]
        ]
        Tabs.tabs "main-tabs" [] [
            Tab.tab "dataInput" [Tab.Label "Data input"] [
                Div.div [ClassName "section"] [
                    formComponent "Start the analysis" "start the analysis with this button after selecting the input data below" [
                        Button.button [ClassName "button is-primary"; Id "main-start-btn"; (*Custom("disabled",box true)*)] [str "start analysis"]
                    ]
                    Div.div [Id "btn-test"] []
                ]
                yield! frameInput
                yield! ontologyMapInput
            ]
            Tab.tab "resultValidation" [Tab.Label "Result validation"; Tab.Disabled true] [
                resultValidation
            ]
            Tab.tab "tmeaResults" [Tab.Label "TMEA results"; Tab.Disabled true] [
                yield! tmeaResults
            ]
        ]
    ]
