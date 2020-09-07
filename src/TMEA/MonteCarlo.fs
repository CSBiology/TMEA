namespace TMEA

module MonteCarlo =
    open FSharp.Stats
    open BioFSharp.Stats
    open BioFSharp.Stats.OntologyEnrichment


    //create distribution of iter weight sums for a bin of size binSize 
    let private bootstrapBin (binSize: int) (weightArray:float[]) (iter: int) =
        let steps = iter / 10
        let startTime = System.DateTime.Now

        let rec sumRandomEntriesBy k sum =
            if k < binSize then
                sumRandomEntriesBy (k+1) (sum + (weightArray.[Random.rndgen.NextInt(weightArray.Length)]))
            else 
                sum

        let rec loop currentIter resultList =
            if currentIter < iter then
                let tmp = sumRandomEntriesBy 0 0.
                loop (currentIter+1) (tmp::resultList)
            else
                resultList |> Array.ofList

        loop 1 []

    let private getEmpiricalPvalue (testDistributions: Map<int,Map<float,int>>) (weightSum:float) (binSize:int) =
        match Map.tryFind binSize testDistributions with
        |Some dist ->   
            let testDist = dist
            float (testDist |> Map.fold (fun acc key value -> if abs key > abs weightSum then acc + value else acc) 0) / (float (testDist |> Map.fold (fun acc key value -> acc + value) 0))
        |_ -> 10000000.

    let private assignPValues (testDistributions:Map<int,Map<float,int>>) (testTargets:(string*int*float)[])=
        testTargets 
        |> Array.map (fun (name,binSize,weightSum) -> TMEASetDescriptor.create name (getEmpiricalPvalue testDistributions weightSum binSize) binSize weightSum)
       
    ///utility function to prepare a dataset column for TMEA characterization. The ontology map can be created by using the BioFSharp.BioDB module. 
    ///
    ///identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///missingKey: placeholder annotation for entities without annotation
    ///rawData: feature array of interest, must be same length as annotations.
    let prepareDataColumn (ontologyMap:Map<string,string [] >) (missingKey:string) (identifiers: string []) (rawData:float []) =

        if rawData.Length <> identifiers.Length then
            failwithf "data column and identifiers dont have the same length (%i vs %i)" rawData.Length identifiers.Length
        else
            let annotatedIds =
                identifiers
                |> Array.map (fun id -> 
                    match Map.tryFind id ontologyMap with
                    |Some ann -> ann 
                    |_ -> [|missingKey|]
                )
            rawData
            |> Array.mapi (fun i x ->  
                annotatedIds.[i] 
                |> Array.map (fun ann -> 
                    createOntologyItem identifiers.[i] ann 0 x
                )
            )
            |> Array.concat

    ///utility function to prepare a dataset (in column major form) for TMEA characterization. The ontology map can be created by using the BioFSharp.BioDB module.
    ///identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///missingKey: placeholder annotation for entities without annotation
    ///rawData: feature matrix of interest, columns must have same length as identifiers
    let prepareDataset (ontologyMap:Map<string,string [] >) (missingKey:string) (identifiers: string []) (rawDataset:float [] []) =
        rawDataset
        |> Array.map (prepareDataColumn ontologyMap missingKey identifiers)

    ///Compute TMEA (Thermodynamically motivated Set Enrichment Analysis t) for the given annotated dataset. This empirical test was
    ///initially designed for the biological application of Surprisal Analysis to test the weight distribution of a given bin of annotations is significantly different than a random distribution 
    ///of the same size given the whole dataset, but it should be applicable to similar types of datasets.
    ///
    ///Input: 
    ///
    ///- verbose: if true, bootstrap iterations and runtime for bootstrapping is printed
    ///
    ///- bootstrapIterations: the amount of distributions to sample from the whole dataset to create test distributions for each binsize present in the data
    ///
    ///- data: annotated dataset (containing ontology items with the associated feature)
    ///
    ///a TMEA test returns 3 descriptors for the input data:
    ///Negative descriptor: test distributions and tests are performed on the negative values of the dataset only
    ///Absolute descriptor: test distributions and tests are performed on the positive values of the dataset only
    let compute (verbose:bool) (bootstrapIterations:int) (data: OntologyItem<float> array) =

        if verbose then printfn "starting TMEA characterization"

        let groups = data |> Array.groupBy (fun x -> x.OntologyTerm)
           
        let positiveTestTargets = 
            groups
            |> Array.map (fun (termName,tmp) ->   
                let tmp = tmp |> Array.filter (fun ann -> ann.Item > 0.)
                termName,tmp.Length,tmp |> Array.sumBy (fun x -> x.Item))
            |> Array.filter (fun (termName,binSize,weightSum) -> binSize>0)
           
        let negativeTestTargets = 
            groups
            |> Array.map (fun (termName,tmp) ->   
                let tmp = tmp |> Array.filter (fun ann -> ann.Item < 0.)
                termName,tmp.Length,tmp |> Array.sumBy (fun x -> x.Item))
            |> Array.filter (fun (termName,binSize,weightSum) -> binSize>0)

        let positiveBinsizes = positiveTestTargets |> Array.map (fun (_,binSize,_) -> binSize) |> Array.distinct
        let negativeBinsizes = negativeTestTargets |> Array.map (fun (_,binSize,_) -> binSize) |> Array.distinct

        let weightArr =     data        |> Array.map (fun ann -> ann.Item)
        let posWeightArr =  weightArr   |> Array.filter(fun x -> x>0.)
        let negWeightArr =  weightArr   |> Array.filter(fun x -> x<0.)


        // create bootstrapped test distributions for all test targets

        if verbose then printfn "bootstrapping positive test distributions for %i functionally annotated sets" positiveBinsizes.Length
        let positiveTestDistributions =

            let startTime = System.DateTime.Now

            positiveBinsizes
            |> Array.mapi 
                (fun i binSize ->

                    if verbose && (i % (positiveBinsizes.Length / 10) = 0 ) then
                        let elapsed = System.DateTime.Now.Subtract(startTime)
                        printfn "[%i/%i] bins @ %imin %is" i positiveBinsizes.Length elapsed.Minutes elapsed.Seconds

                    let tmp = bootstrapBin binSize posWeightArr bootstrapIterations
                    (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
                )
            |> Map.ofArray
               
        if verbose then printfn "bootstrapping negative test distributions for %i functionally annotated sets" negativeBinsizes.Length
        let negativeTestDistributions = 

            let startTime = System.DateTime.Now

            negativeBinsizes
            |> Array.mapi 
                (fun i binSize ->

                    if verbose && (i % (negativeBinsizes.Length / 10) = 0 ) then
                        let elapsed = System.DateTime.Now.Subtract(startTime)
                        printfn "[%i/%i] bins @ %imin %is" i negativeBinsizes.Length elapsed.Minutes elapsed.Seconds

                    let tmp = bootstrapBin binSize negWeightArr bootstrapIterations
                    (binSize,Distributions.Frequency.create (Distributions.Bandwidth.nrd0 tmp) tmp)
                )
            |> Map.ofArray

        if verbose then printfn "assigning empirical pValues for all bins..."

        //assign Pvalues for all test targets
        let posResults = assignPValues positiveTestDistributions positiveTestTargets 
        let negResults = assignPValues negativeTestDistributions negativeTestTargets 

        TMEACharacterization.create data posResults negResults bootstrapIterations


    ///Compute TMEA (Thermodynamically motivated Set Enrichment Analysis ) for the given Surprisal Analysis result. This empirical test was
    ///designed for the biological application of Surprisal Analysis to test the weight distribution of a given bin of annotations is significantly different than a random distribution 
    ///of the same size given the whole dataset.
    ///
    ///Input: 
    ///
    ///- verbose: if true, bootstrap iterations and runtime for bootstrapping is printed
    ///
    ///- ontologyMap: maps identifiers of the data to ontology annotations (can be created using the BioFSharp.BioDB module)
    ///
    ///- identifiers: a string array containing the annotations of the data at the same index, used as lookup in the ontology map. 
    ///
    ///- missingKey: placeholder annotation for entities without annotation
    ///
    ///- bootstrapIterations: the amount of distributions to sample from the whole dataset to create test distributions for each binsize present in the data
    ///
    ///- saRes: the Surprisal Analysis Result to test
    ///
    ///a TMEA test returns 2 descriptors for each constraint of the Surprisal Nalysis result:
    ///Negative descriptor: test distributions and tests are performed on the negative values of the dataset only
    ///Absolute descriptor: test distributions and tests are performed on the positive values of the dataset only

    let computeOfSARes (verbose:bool) (ontologyMap:Map<string,string [] >) (identifiers: string []) (missingKey:string) (bootstrapIterations:int) (saRes:FSharp.Stats.ML.SurprisalAnalysis.SAResult) =
        saRes.MolecularPhenotypes
        |> Matrix.toJaggedArray
        // Matrices are sadly row major =(
        |> JaggedArray.transpose
        |> prepareDataset ontologyMap missingKey identifiers
        |> Array.mapi 
            (fun i p ->
                if verbose then printfn "TMEA of constraint %i" i
                compute verbose bootstrapIterations p
            ) 
       
    ///Async version of computeOfSARes to use for parallelization (computeOfSAResAsync ( .. ) |> Async.Parallel |> Async.RunSynchronously)
    let computeOfSAResAsync (verbose:bool) (ontologyMap:Map<string,string [] >) (identifiers: string []) (missingKey:string) (bootstrapIterations:int) (saRes:FSharp.Stats.ML.SurprisalAnalysis.SAResult) =
        saRes.MolecularPhenotypes
        |> Matrix.toJaggedArray
        // Matrices are sadly row major =(
        |> JaggedArray.transpose
        |> prepareDataset ontologyMap missingKey identifiers
        |> Array.mapi 
            (fun i p ->
                async {
                    return compute verbose bootstrapIterations p
                }
            ) 
       

