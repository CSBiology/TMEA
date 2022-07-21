(*** hide ***)

(*** condition: prepare ***)
#r "nuget: Newtonsoft.JSON, 12.0.3"
#r "nuget: FSharpAux, 1.0.0"
#r "nuget: FSharpAux.IO, 1.0.0"
#r "nuget: Plotly.NET, 2.0.0-preview.6"
#r "nuget: FSharp.Stats, 0.4.1"
#r "nuget: BioFSharp, 2.0.0-beta6"
#r "nuget: BioFSharp.Stats, 2.0.0-beta6"
#r "nuget: Deedle"
#I "../src/TMEA/bin/Release/netstandard2.0"
#r "TMEA.dll"

let path = System.Environment.GetEnvironmentVariable("PATH")
let lib = System.IO.Path.Combine(__SOURCE_DIRECTORY__,@"..\lib")
System.Environment.SetEnvironmentVariable("PATH",$"{path}{lib};") |> ignore

(*** condition: ipynb ***)
#if IPYNB
#r "nuget: Plotly.NET, {{fsdocs-package-version}}"
#r "nuget: Plotly.NET.Interactive, {{fsdocs-package-version}}"
#endif // IPYNB
