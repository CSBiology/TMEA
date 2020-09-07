![](https://img.shields.io/nuget/v/TMEA)


# Introduction

The objective of gene set enrichment analysis (GSEA) in modern biological studies is to identify functional 
profiles in huge sets of biomolecules generated by high throughput measurements of genes, transcripts, metabolites, 
and proteins. GSEA is based on a two-stage process using classical statistical analysis to score the input data and 
subsequent testing for overrepresentation of the enrichment score within a given functional coherent set. 
However, enrichment scores computed by different methods are merely statistically motivated and often elusive to 
direct biological interpretation. 

Here, we propose a novel approach, called Thermodynamically Motivated Enrichment Analysis (TMEA), to account for the 
energy investment in biological relevant processes. Therefore, TMEA is based on surprisal analysis, that offers a 
thermodynamic-free energy-based representation of the biological steady state and of the biological change. 
The contribution of each biomolecule underlying the changes in free energy is used in a Monte Carlo resampling 
procedure resulting in a functional characterization directly coupled to the thermodynamic characterization of 
biological responses to system perturbations.

This package is in an experimental stage, there may be bugs, and some advanced functionality (e.g. producing plots from the result types) are missing (at least at the moment). 

![](./docs/img/GraphicalAbstract.png)

# Installation

This repository is home of the library TMEA, which we created from the scripts we used in our currently in-review paper.

the package is available on nuget:

https://www.nuget.org/packages/TMEA

alternatively, clone this repo and run `fake.cmd` or `fake.sh` (requires dotnet sdk >= 3.1.302)


# Usage

1. Include the lapack folder to your PATH variable, either for the fsi session or on your systems level. The folder is located in the nuget package under `./Netlib_LAPACK`

2. Reference this library and its dependencies.

3. We strongly recommend to register fsi printers for Deedle, the dataframe library we use in this project. There is a `Deedle.fsx` file located in the Deedle nuget package which will take care of that if you `#load` it.

4. A simple pipeline to perform TMEA on time series data looks like this:

    ```F#
    open TMEA
    open TMEA.SurprisalAnalysis
    open TMEA.MonteCarlo
    open TMEA.Frames
    open TMEA.Plots

    let annotationMap : Map<string,string[]> = ... // We assume you have ontology annotations for your dataset

    IO.readDataFrame 
        "TranscriptIdentifier" // The column of the data table that contains your entity identifiers
        "\t" // separator for the input file
        "path/to/your/raw/data.txt"
    |> Analysis.computeOfDataFrame 
        Analysis.standardTMEAParameters //using custom parameters you can change verbosity, bootstrap iterations, and the annotation used for unannotated entities
        annotationMap
    ```

# Plots

All plot functions have a `generate*` analog, which generates the Chart object without rendering it (in case you want to fine tune styles etc.).
Currently, the following plots are provided by the package:

All charting functions are extensiopn methods of the `TMEAResult` type. Given the example script above, you can visualize the results as:

##### Functionally Annotated Set (FAS) weight distributions

 - `plotFASWeightDistribution` is an exploratory plot that visualizes the overall weight distributions of the given TMEA Characterizations, and adds detailed weight distributions of the FAS of interest on top of that. additionally, annotations on the respective subplots show useful information about the FAS characterization.

    ```F#
    tmeaRes
    |> TMEAResult.plotFASWeightDistribution 
        true //use style presets
        0.05 //significance threshold for (corrected!) p values
        [1;2;3] //constraints to plot
        "signalling.light" //name of the FAS
    ```

    ![](./docs/img/FASWeightDist.png)

##### Potential Time Course:

 - `plotConstraintTimecourses` plots the constraint potential time courses of the given TMEA result:

    ```F#
    SaRes
    |> TMEAResult.plotConstraintTimecourses true //true -> will use style presets
    ```

    ![](./docs/img/cpTimeCourse.png)

 - `plotPotentialHeatmap` is a more visually pleasing version of above plot (it omits the baseline state per default):

    ```F#
    SaRes
    |> TMEAResult.plotPotentialHeatmap true
    ```

    ![](./docs/img/cpHeatmap.png)

##### Free Energy Landscape:

 - `plotFreeEnergyLandscape` plots the free energy landscape of the TMEA result:

    ```
    SaRes
    |> TMEAResult.plotFreeEnergyLandscape true
    ```

    ![](./docs/img/EnergyLandscape.png)

##### Constraint importance:

 - `plotConstraintImportance`: given the TMEA result, plots the singular values of all constraints (except the baseline state) and the 'importance loss' between them. 

    ```
    SaRes
    |> TMEAResult.plotConstraintImportance true
    ```

    ![](./docs/img/ConstraintImportance.png)

##### Data recovery:

 - `plotDataRecovery`: given the TMEA result, plots the gradual reconstruction of the original data when using only n (in the example below, n = 3) constraints from the given TMEA result:

    ```
    SaRes
    |> TMEAResult.plotDataRecovery true 3 
    ```

    ![](./docs/img/DataRecovery.png)

# License acknowlegments

This library contains [Netlib LAPACK](http://www.netlib.org/lapack/) binaries compiled from source, thanks to all the authors of it:
    
    Anderson, E. and Bai, Z. and Bischof, C. and Blackford, S. and Demmel, J. and Dongarra, J. and
    Du Croz, J. and Greenbaum, A. and Hammarling, S. and McKenney, A. and Sorensen, D.

