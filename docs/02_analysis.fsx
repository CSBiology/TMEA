(**
---
title: Performing TMEA
category: Usage
categoryindex: 1
index: 2
---
*)

(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Newtonsoft.Json, 13.0.1"
#r "nuget: BioFSharp, 2.0.0-preview.3"
#r "nuget: BioFSharp.Stats, 2.0.0-preview.3"
#r "nuget: Deedle, 2.5.0"
#r "nuget: FSharp.Stats, 0.4.7"
#r "nuget: FSharpAux, 1.1.0"
#r "nuget: FSharpAux.IO, 1.1.0"
#r "nuget: Plotly.NET, 3.0.0"
#I "../src/TMEA/bin/Release/netstandard2.0"
#r "TMEA.dll"

let path = System.Environment.GetEnvironmentVariable("PATH")
let lib = System.IO.Path.Combine(__SOURCE_DIRECTORY__,@"..\lib") |> System.IO.Path.GetFullPath
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

# Performing TMEA

to perform TMEA from a dataframe, use the `Analysis.computeOfDataFrame` function. 

You need a map containing your 

You can control the following parameters used during the analysis by passing `TMEAParameters`. 
|---|---|
| `MissingKey` | The key to use for items that have no functional annotations |
| `BootstrapIterations` | The key to use for items that have no functional annotations |
| `Verbose` | Wether to print progress info to the console |

*)

// read data input, see input docs for more info
#r "nuget: FSharp.Data, 4.1.1"
open FSharp.Data
open TMEA
open TMEA.Frames
open Deedle
open TMEA.IO

let df = 
    Http.RequestString "https://raw.githubusercontent.com/CSBiology/TMEA/main/tests/data/Highlight_LogFPKM.csv"
    |> IO.readDataFrameFromString "TranscriptIdentifier" "," 

let annotations = 
    Http.RequestString "https://raw.githubusercontent.com/CSBiology/TMEA/main/tests/data/arabidopsis_araport11.txt"
    |> IO.readOntologyMapFromString "\t" "Identifier" "MapManDescription"

// initialize parameters for analysis
let parameters = TMEAParameters.initDefault()

let customParameters = TMEAParameters.initDefaultWith(MissingKey="MISSING_ANNOTATION",BootstrapIterations=99,Verbose=false)

(**
TMEA uses highly optimized native binaries for linear algebra. They are included in the nuget package, but you have to include them in your PATh variable.

the standard location on windows is `~/.nuget/packages/tmea/<package-version>/Netlib_LAPACK`
*)

// initialize linear algebra service
FSharp.Stats.Algebra.LinearAlgebra.Service()

// perform TMEA
let tmeaResult = 
    df
    |> Analysis.computeOfDataFrame customParameters annotations

(**
Multiple types of results can be obtained from the TMEA result:

### Constraint potentials
*)

open TMEA.Frames

let cps = 
    tmeaResult
    |> TMEAResult.toConstraintPotentialsFrame

cps.Print()

(***include-output***)

(**
### TMEA characterization frames

This frame contains the central result of the TMEA characterization
*)

let characterizationFrame = 
    tmeaResult
    |> TMEAResult.toTMEACharacterizationFrame(
        AlphaLevel = 0.05
    )

characterizationFrame.Print()

(***include-output***)

(**
### Enrichment significance frame

The contents of this frame indicate for every functionally annotated set in each constraint wether it can be accepted as significantly contributing to the system response based on the given alpha level.
*)

let sigFrame = 
    tmeaResult
    |> TMEAResult.toSignificanceMatrixFrame(
        CorrectionMethod = MultipleTestingCorrection.BenjaminiHochberg,
        AlphaLevel = 0.05
    )

sigFrame.Print()

(***include-output***)