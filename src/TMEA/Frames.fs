namespace TMEA

module Frames =

    open Deedle
    open FSharp.Stats

    let internal optDefFloat (f:float opt)=
        if f.HasValue then f.Value else -1.

    let internal optDefInt (f:int opt)=
        if f.HasValue then f.Value else 0

    let internal createTMEACharacterizationTable minBinSize (alphaLevel:float) (termNameTransformation: string -> string) (tmeaCharacterizations:TMEACharacterization []) : Frame<(string*(string*int)),string> =
        tmeaCharacterizations
        |> Array.mapi (fun i desc ->
            let negFrame: Frame<string,string> =
                Frame.ofRecords desc.NegativeDescriptor
                |> Frame.indexRows "OntologyTerm"
                |> Frame.mapColKeys (fun ck -> sprintf "Neg_%s" ck)

            let posFrame: Frame<string,string>  =
                Frame.ofRecords desc.PositiveDescriptor
                |> Frame.indexRows "OntologyTerm"
                |> Frame.mapColKeys (fun ck -> sprintf "Pos_%s" ck)

            [negFrame;posFrame]
            |> List.reduce Frame.merge
            |> Frame.mapRowKeys (fun rk -> rk,i)
        )
        
        |> Array.reduce (Frame.merge)
        |> fun f ->
            let posBinSize, negBinSize =
                f |> Frame.getCol "Pos_BinSize",
                f |> Frame.getCol "Neg_BinSize" 
            
            let absBinSizes = 
                Series.zip posBinSize negBinSize
                |> Series.mapAll (fun _ (x:(int opt * int opt) option) ->
                    let p1,p2 =
                        match x with 
                        |Some (o1,o2) ->
                            optDefInt o1,optDefInt o2
                        |None -> 0,0

                    p1 + p2
                    |> Some
                )
            f |> Frame.addCol "Total_BinSize" absBinSizes

        |> Frame.filterRows (fun rk os -> 
            match os.TryGetAs<int>("Total_BinSize") |> OptionalValue.asOption with
            | None -> printfn "%A" rk; true
            | Some b -> b  >= minBinSize
        )
        |> fun f ->
            let posPVals,negPVals =
                f |> Frame.getCol "Pos_PValue" |> Series.observations |> Seq.groupBy (fun ((bin,constrIndex),(pValue:float)) -> constrIndex),
                f |> Frame.getCol "Neg_PValue" |> Series.observations |> Seq.groupBy (fun ((bin,constrIndex),(pValue:float)) -> constrIndex)
            let correctedPosPVals, correctedNegPVals =
                posPVals 
                |> Seq.map (fun (cI,pVals) -> pVals |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id)
                |> Seq.concat
                |> series
                ,
                negPVals 
                |> Seq.map (fun (cI,pVals) -> pVals |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id)
                |> Seq.concat
                |> series
        
            f
            |> Frame.addCol "Pos_PValue_BH_Corrected" correctedPosPVals
            |> Frame.addCol "Neg_PValue_BH_Corrected" correctedNegPVals
            |> Frame.mapRowKeys (fun (name,cI) -> (termNameTransformation name) => (name,cI))
            |> fun f ->
                let allPvalZip =
                    Series.zip (f |> Frame.getCol "Neg_PValue_BH_Corrected") (f |> Frame.getCol "Pos_PValue_BH_Corrected")
                    |> Series.mapAll (fun _ (x:(float opt * float opt) option) ->
                        let p1,p2 =
                            match x with 
                            |Some (o1,o2) ->
                                optDefFloat o1,optDefFloat o2
                            |None -> -1.,-1.

                        ((p1 < alphaLevel && p1 >= 0.) 
                        || (p2 < alphaLevel && p2 >= 0.))
                        |> Some
                    )
                f |> Frame.addCol "isAnySig_BH_Corrected" allPvalZip
            |> fun f ->
                let allPvalZip =
                    Series.zip (f |> Frame.getCol "Neg_PValue") (f |> Frame.getCol "Pos_PValue")
                    |> Series.mapAll (fun _ (x:(float opt * float opt) option) ->
                        let p1,p2 =
                            match x with 
                            |Some (o1,o2) ->
                                optDefFloat o1, optDefFloat o2
                            |None -> -1.,-1.

                        ((p1 < alphaLevel && p1 >= 0.) 
                        || (p2 < alphaLevel && p2 >= 0.))
                        |> Some
                    )
                f |> Frame.addCol "isAnySig" allPvalZip

    type TMEAResult with
    
        /// <summary>
        /// Returns a function that creates a frame containing the TMEA Characterizations identified in the given TMEAResult. This is the central result of TMEA analysis.
        ///
        /// It contains rows with the following columns:
        ///
        /// Term(FAS) - Indicates the Name of the Functionally Annotated Set (FAS)
        ///
        /// Transformed Term - A custom term name transformation that can be set by the user. Can be used e.g. for further grouping of the FAS
        ///
        /// ConstraintIndex - Indicates the index of the constraint the rest of the result in the row is concerned with
        ///
        /// Neg_PValue - The raw empirical PValue returned by the TMEA Monte-Carlo simulation indicating if the FAS is enriched when only analyzing negative weights in the constraint
        ///
        /// Neg_BinSize - The amount of entities with negative weight in the FAS
        ///
        /// Neg_WeightSum - The sum of negative weights in the FAS
        ///
        /// Pos_PValue - The raw empirical PValue returned by the TMEA Monte-Carlo simulation indicating if the FAS is enriched when only analyzing positive weights in the constraint
        ///
        /// Pos_BinSize - The amount of entities with positive weight in the FAS
        ///
        /// Pos_WeightSum - The sum of positive weights in the FAS
        ///
        /// Total_BinSize - The amount of entities contained in the FAS
        ///
        /// Pos_PValue_BH_Corrected - The afforementioned positive p-value, constraint-wise adjusted with the Benjamini-Hochberg correction for multiple testing
        ///
        /// Neg_PValue_BH_Corrected - The afforementioned negative p-value, constraint-wise adjusted with the Benjamini-Hochberg correction for multiple testing
        ///
        /// isAnySig_BH_Corrected - Indicates if the FAS can be considered as significantly enriched when looking at either negative or positive weights depending on the user defined alpha level
        ///
        /// isAnySig - Indicates if the FAS can be considered as significantly enriched when looking at either negative or positive weights depending on the user defined alpha level after applying constraint-wise adjustment with the Benjamini-Hochberg correction for multiple testing
        ///
        /// </summary>
        /// <param name="MinBinSize">A threshold for the amount of entities contained in every FAS. FAS below this entity count will not be contained in the result frame. Default=0</param>
        /// <param name="AlphaLevel">The alpha level to either accept or reject the FAS as significantly enriched. Default=0.05</param>
        /// <param name="TermNameTransformation">A function to transform the FAS Names</param>
        static member toTMEACharacterizationFrame(?MinBinSize:int, ?AlphaLevel:float, ?TermNameTransformation:string->string) = 
            
            let minBinSize = defaultArg MinBinSize 0
            let alphaLevel = defaultArg AlphaLevel 0.05
            let termNameTransformation = defaultArg TermNameTransformation id

            fun (tmeaRes: TMEAResult) ->
                tmeaRes.Characterizations |> createTMEACharacterizationTable minBinSize alphaLevel termNameTransformation

        /// <summary>
        /// Returns a function that creates a frame containing A matrix that contains 1(indicating significant enrichment) or 0(indicating no enrichment) for all FAS(rows) in all constraints(columns)
        /// </summary>
        /// <param name="UseBenjaminiHochberg">Wether to use p-values adjusted by constraint-wise Benjamini-Hochberg correction for multiple testing. Default=false</param>
        /// <param name="AlphaLevel">The alpha level to either accept or reject the FAS as significantly enriched. Default=0.05</param>
        /// <param name="TermNameTransformation">A function to transform the FAS Names</param>
        static member toSignificanceMatrixFrame (?UseBenjaminiHochberg:bool, ?AlphaLevel:float, ?TermNameTransformation:string->string) =
            
            let useBenjaminiHochberg = defaultArg UseBenjaminiHochberg false
            let threshold = defaultArg AlphaLevel 0.05
            let termNameTransformation = defaultArg TermNameTransformation id

            fun (tmeaRes:TMEAResult) ->
                tmeaRes.Characterizations
                |> Array.mapi (fun cI c ->
                    let pos = 
                        c.PositiveDescriptor
                        |> Array.map (fun d ->
                            d.OntologyTerm => d.PValue
                        )
            
                    let neg = 
                        c.PositiveDescriptor
                        |> Array.map (fun d ->
                            d.OntologyTerm => d.PValue
                        )
            
                    let keys, pos' =
                        if useBenjaminiHochberg then
                            neg
                            |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id
                            |> Array.ofList
                            |> Array.unzip
                        else
                            neg
                            |> Array.unzip
            
                    let keys, neg' =
                        if useBenjaminiHochberg then
                            neg
                            |> FSharp.Stats.Testing.MultipleTesting.benjaminiHochbergFDRBy id
                            |> Array.ofList
                            |> Array.unzip
                        else
                            neg
                            |> Array.unzip
            
            
                    $"C_{cI}" => (
                        Array.map2 (fun p n -> if p < n then p else n) pos' neg'
                        |> Array.map (fun lowestPValue -> if lowestPValue < threshold then 1 else 0)
                        |> Array.zip keys
                        |> series
                    )
                )
                |> frame
                |> Frame.fillMissingWith 0
                |> Frame.mapRowKeys (fun (name) -> (termNameTransformation name) => name)

        /// <summary>
        /// Creates a frame containing the constraints(patterns) identified in the TMEAResult
        /// </summary>
        /// <param name="tmeaRes">The tmeaResult to extract the constraint frame from.</param>
        static member toConstraintsFrame (tmeaRes:TMEAResult) =
            
            tmeaRes.Constraints
            |> Matrix.toArray2D
            |> Frame.ofArray2D
            |> Frame.mapRowKeys(fun (i:int) -> tmeaRes.EntityNames.[i])
            |> Frame.mapColKeys (fun (ck: int) -> $"C_{ck}")

        /// <summary>
        /// Creates a frame containing the constraint potentials identified in the TMEAResult
        /// </summary>
        /// <param name="tmeaRes">The tmeaResult to extract the constraint potentials frame from.</param>
        static member toConstraintPotentialsFrame (tmeaRes:TMEAResult) =

            tmeaRes.ConstraintPotentials
            |> Matrix.toArray2D
            |> Frame.ofArray2D
            |> Frame.mapRowKeys(fun (i:int) -> $"C_{i}")
            |> Frame.mapColKeys (fun (i:int) -> tmeaRes.Timepoints.[i])
