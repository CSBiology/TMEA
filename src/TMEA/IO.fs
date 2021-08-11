namespace TMEA

module IO =
    open Deedle
    open System.IO
    open Frames
    
    let readComparisonFrame (path:string) : Frame<string,string> =
        Frame.ReadCsv(
            path,
            true,
            separators = "\t"
        )
        |> Frame.indexRows "BinName"

    let readTMEACharacterizationFrame(path:string) : Frame<string*(string*int),string> =
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,string,int,float,int,float,float,int,float,float,int,float,float,float,float,bool,bool"
        )
        |> Frame.indexRowsUsing (fun os ->
            os.GetAs<string>("NAME") => (os.GetAs<string>("BINCODE"),os.GetAs<int>("ConstraintIndex"))
        )

    let readConstraintPotentialsFrame (path:string) : Frame<string,string*int> =
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,float,float,float,float,float,float,float,float,float,float,float"
        )
        |> Frame.indexRows "ConstraintIndex"
        |> Frame.mapColKeys (fun ck ->
            let splt = ck.Split('_')
            splt.[0] => (splt.[1].Replace("min","") |> int)
        )    

    let readConstraintsFrame (path:string) : Frame<string,string> =
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,float,float,float,float,float,float,float,float,float,float,float"
        )
        |> Frame.indexRows "TranscriptIdentifier"

    let readSignificanceMatrixFrame (path:string): Frame<string*string,string>=
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,string,string,string,string,string,string,string,string,string,string"
        )
        |> Frame.indexRowsUsing (fun os -> os.GetAs<string>("NAME") => os.GetAs<string>("BINCODE"))

    //Read a data frame for TMEA analysis. The data is expected to be column major and contain only numeric data except the identifier column.
    let readDataFrame (identifierCol:string) (separators:string) (path:string) : Frame<string,string> =
        let f' = 
            Frame.ReadCsv(
                path,
                true,
                separators=separators
            )

        let columnTypes = 
            f'.ColumnKeys 
            |> Seq.map (fun s -> if s = identifierCol then sprintf "%s=string" s else sprintf "%s=float" s)
            |> String.concat ","

        Frame.ReadCsv(
            path,
            true,
            separators=separators,
            schema=columnTypes
        )
        |> Frame.indexRows identifierCol

    let readDataFrameFromString (identifierCol:string) (separators:string) (dataString:string) : Frame<string,string> =
        let f' = 
            Frame.ReadCsvString(
                dataString,
                true,
                separators=separators
            )

        let columnTypes = 
            f'.ColumnKeys 
            |> Seq.map (fun s -> if s = identifierCol then sprintf "%s=string" s else sprintf "%s=float" s)
            |> String.concat ","

        Frame.ReadCsvString(
            dataString,
            true,
            separators=separators,
            schema=columnTypes
        )
        |> Frame.indexRows identifierCol

    //Read a data frame for TMEA analysis. The data is expected to be column major and contain only numeric data except the identifier column.
    let readDataFrameFromStream (identifierCol:string) (separators:string) (data:byte []) : Frame<string,string> =
        use stream1 = new MemoryStream(data)
        use stream2 = new MemoryStream(data)
        let f' = 
            Frame.ReadCsv(
                stream1,
                true,
                separators=separators
            )
        let columnTypes = 
            f'.ColumnKeys 
            |> Seq.map (fun s -> if s = identifierCol then sprintf "%s=string" s else sprintf "%s=float" s)
            |> String.concat ","

        Frame.ReadCsv(
            stream2,
            true,
            separators="\t",
            schema=columnTypes
        )
        |> Frame.indexRows identifierCol

    let readOntologyMap (path:string) (separator:string) (idColName:string) (annColName:string) : Map<string,string []>=
        Frame.ReadCsv(path,hasHeaders=true,separators=separator)
        |> fun f ->
            let idCol : Series<int,string> = Frame.getCol idColName f
            let annCol : Series<int,string>= Frame.getCol annColName f
            Series.zipInner idCol annCol
            |> Series.values
            |> Seq.groupBy fst
            |> Seq.map (fun (id,anns) -> id , anns |> Seq.map snd |> Array.ofSeq)
            |> Map.ofSeq

    let readOntologyMapFromStream (stream:Stream) (separator:string) (idColName:string) (annColName:string) : Map<string,string []>=
        Frame.ReadCsv(stream,hasHeaders=true,separators=separator)
        |> fun f ->
            let idCol : Series<int,string> = Frame.getCol idColName f
            let annCol : Series<int,string>= Frame.getCol annColName f
            Series.zipInner idCol annCol
            |> Series.values
            |> Seq.groupBy fst
            |> Seq.map (fun (id,anns) -> id , anns |> Seq.map snd |> Array.ofSeq)
            |> Map.ofSeq

    type TMEAResult with
        
        /// <summary>
        /// Returns a function that saves a frame containing the TMEA Characterizations identified in the given TMEAResult at the given path. This is the central result of TMEA analysis.
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
        /// <param name="path">The path (containing the filename with extension) of the output file</param>
        /// <param name="MinBinSize">A threshold for the amount of entities contained in every FAS. FAS below this entity count will not be contained in the result frame. Default=0</param>
        /// <param name="AlphaLevel">The alpha level to either accept or reject the FAS as significantly enriched. Default=0.05</param>
        /// <param name="TermNameTransformation">A function to transform the FAS Names</param>
        static member saveTMEACharacterizationFrame(path:string, ?MinBinSize:int, ?AlphaLevel:float, ?TermNameTransformation:string->string) = 
             
             fun (tmeaRes: TMEAResult) ->
                tmeaRes
                |> TMEAResult.toTMEACharacterizationFrame(?MinBinSize=MinBinSize, ?AlphaLevel=AlphaLevel, ?TermNameTransformation=TermNameTransformation)
                |> fun f -> f.SaveCsv(path,separator='\t',keyNames=["Term(FAS)"; "Transformed Term"; "ConstraintIndex"])

        /// <summary>
        /// Returns a function that saves a frame containing A matrix that contains 1(indicating significant enrichment) or 0(indicating no enrichment) for all FAS(rows) in all constraints(columns) at the given path.
        /// </summary>
        /// <param name="path">The path (containing the filename with extension) of the output file</param>
        /// <param name="UseBenjaminiHochberg">Wether to use p-values adjusted by constraint-wise Benjamini-Hochberg correction for multiple testing. Default=false</param>
        /// <param name="AlphaLevel">The alpha level to either accept or reject the FAS as significantly enriched. Default=0.05</param>
        /// <param name="TermNameTransformation">A function to transform the FAS Names</param>
        static member saveSignificanceMatrixFrame(path:string, ?UseBenjaminiHochberg:bool, ?AlphaLevel:float, ?TermNameTransformation:string->string) =
            
            fun (tmeaRes: TMEAResult) ->
                tmeaRes
                |> TMEAResult.toSignificanceMatrixFrame(?UseBenjaminiHochberg=UseBenjaminiHochberg, ?AlphaLevel=AlphaLevel, ?TermNameTransformation=TermNameTransformation)
                |> fun f -> f.SaveCsv(path,separator='\t',keyNames=["Term(FAS)"; "Transformed Term"])

        /// <summary>
        /// Saves a frame containing the constraints(patterns) identified in the TMEAResult at the given path.
        /// </summary>
        /// <param name="path">The path (containing the filename with extension) of the output file</param>
        /// <param name="tmeaRes">The tmeaResult to extract the constraint frame from.</param>
        static member saveConstraintsFrame (path:string) (tmeaRes:TMEAResult) = 
            
            fun (tmeaRes: TMEAResult) ->
                tmeaRes
                |> TMEAResult.toConstraintsFrame
                |> fun f -> f.SaveCsv(path,separator='\t',keyNames=["EntityName"])

        /// <summary>
        /// Saves a frame containing the constraint potentials identified in the TMEAResult at the given path.
        /// </summary>
        /// <param name="tmeaRes">The tmeaResult to extract the constraint potentials frame from.</param>
        /// <param name="path">The path (containing the filename with extension) of the output file</param>
        static member saveConstraintPotentialsFrame (path:string) (tmeaRes:TMEAResult) = 

            fun (tmeaRes: TMEAResult) ->
                tmeaRes
                |> TMEAResult.toConstraintPotentialsFrame
                |> fun f -> f.SaveCsv(path,separator='\t',keyNames=["EntityName"])