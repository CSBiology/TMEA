namespace TMEA

module Frames =

    open Deedle

    let private optDefFloat (f:float opt)=
        if f.HasValue then f.Value else -1.

    let private optDefInt (f:int opt)=
        if f.HasValue then f.Value else 0

    let createTMEACharacterizationTable minBinSize (termNameTransformation: string -> string) (tmeaCharacterizations:TMEACharacterization []) : Frame<(string*(string*int)),string> =
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

                        ((p1 < 0.05 && p1 >= 0.) 
                        || (p2 < 0.05 && p2 >= 0.))
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

                        ((p1 < 0.05 && p1 >= 0.) 
                        || (p2 < 0.05 && p2 >= 0.))
                        |> Some
                    )
                f |> Frame.addCol "isAnySig" allPvalZip

    type TMEAResult with
    
        static member toTMEACharacterizationFrame(tmeaRes:TMEAResult) = 
            tmeaRes.Characterizations |> createTMEACharacterizationTable 0 id
        
        static member toTMEACharacterizationFrame(minBinSize:int,tmeaRes:TMEAResult) = 
            tmeaRes.Characterizations |> createTMEACharacterizationTable minBinSize id
        
        static member toTMEACharacterizationFrame (minBinSize:int,termNameTransformation:string->string,tmeaRes:TMEAResult) = 
            tmeaRes.Characterizations |> createTMEACharacterizationTable minBinSize termNameTransformation

        static member toSignificanceMatrixFrame : unit = raise (System.NotImplementedException())

        static member toConstraintsFrame : unit = raise (System.NotImplementedException())

        static member toConstraintPotentialsFrame : unit = raise (System.NotImplementedException())
        
