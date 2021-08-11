(**
---
title: Reading input
category: Usage
categoryindex: 1
index: 1
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Newtonsoft.JSON, 12.0.3"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: Plotly.NET, 2.0.0-preview.6"
#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: BioFSharp, 2.0.0-beta6"
#r "nuget: BioFSharp.Stats, 2.0.0-beta6"
#r "nuget: Deedle, 2.4.0"
#r "../bin/TMEA/netstandard2.0/TMEA.dll"

let path = System.Environment.GetEnvironmentVariable("PATH")
let lib = System.IO.Path.Combine(__SOURCE_DIRECTORY__,@"..\lib")
System.Environment.SetEnvironmentVariable("PATH",$"{path}{lib};") |> ignore

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, {{fsdocs-package-version}}"
#r "nuget: Plotly.NET.Interactive, {{fsdocs-package-version}}"
#endif // IPYNB

(**
[![Binder]({{root}}img/badge-binder.svg)](https://mybinder.org/v2/gh/plotly/Plotly.NET/gh-pages?filepath={{fsdocs-source-basename}}.ipynb)&emsp;
[![Script]({{root}}img/badge-script.svg)]({{root}}{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook]({{root}}img/badge-notebook.svg)]({{root}}{{fsdocs-source-basename}}.ipynb)

# Reading input

## Data matrix

you can either use [Deedle]() to read a dataframe and pass it to analysis functions, or use the read functions provided by `TMEA.IO`.

Input provided to TMEA read functions have to have the following format:

```
<IdentifierColumn>  <NumericColumn1>    . . .     <NumericColumnM>
<Identifier1>
.
.
.
<IdentifierN>
```

custom frames preprocessed by deedle need to have string indexed columns with your identifiers and only numeric columns.

You can use `IO.readDataFrame` to read from disk, additionally providing the identifier column and the separator:
*)
open TMEA
open TMEA.IO
open Deedle

(***do-not-eval***)
// read input data matrix from disk
let dataPath = "some/local/path"
let dataFromDisk = IO.readDataFrame "<IdentifierColumn>" "\t" dataPath


(**
Or use an online source and read the frame from it using `IO.readDataFrameFromString`
*)

#r "nuget: FSharp.Data, 4.1.1"
open FSharp.Data
open TMEA
open TMEA.IO
open Deedle

// read input data matrix from an online source
let dataFromWeb = Http.RequestString "https://raw.githubusercontent.com/CSBiology/TMEA/main/tests/data/Highlight_LogFPKM.csv"

let df = IO.readDataFrameFromString "TranscriptIdentifier" "," dataFromWeb

df.Print()

(***include-output***)

(**
## Functional annotations

The input has to contain 2 columns that map from the same identifiers used in the data matrix to the functional annotations of interest.

Consider this annotation data:
*)

let annotationData = Http.RequestString "https://raw.githubusercontent.com/CSBiology/TMEA/main/tests/data/arabidopsis_araport11.txt"

(**
It has the following columns:
*)

let firstRow = annotationData.Split("\n").[0]

(***include-value:firstRow***)

(**
The columns needed to construct an ontology map for TMEA are `Identifier` and for example `MapManDescription`.

These are passed to the `readOntologyMapFromString` function together with the separator. 
*)

// read ontology map from a string (e.g. an online source)
let annotationMap2FromWeb = IO.readOntologyMapFromString annotationData "\t" "Identifier" "MapManDescription"

(***include-value:annotationMap2FromWeb***)

(**
Alternatively, one can read such data from disk as well.
*)

(***do-not-eval***)
// read ontology map from disk
let annotationPath = "some/local/path"
let annotationMapFromDisk = IO.readOntologyMap annotationPath "\t" "<IdentifierColumn>" "<AnnotationColumn>"