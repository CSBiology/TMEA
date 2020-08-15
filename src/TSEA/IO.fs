namespace TSEA

module IO =
    open Deedle
    
    let readComparisonFrame (path:string) : Frame<string,string> =
        Frame.ReadCsv(
            path,
            true,
            separators = "\t"
        )
        |> Frame.indexRows "BinName"

    let readTSEAFrame(path:string) : Frame<string*(string*int),string> =
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,string,int,float,int,float,float,int,float,float,int,float,float,float,float,bool,bool"
        )
        |> Frame.indexRowsUsing (fun os ->
            os.GetAs<string>("NAME") => (os.GetAs<string>("BINCODE"),os.GetAs<int>("ConstraintIndex"))
        )

    let readPotentialsFrame (path:string) : Frame<string,string*int> =
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

    let readMolecularPhenotypeFrame (path:string) : Frame<string,string> =
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,float,float,float,float,float,float,float,float,float,float,float"
        )
        |> Frame.indexRows "TranscriptIdentifier"

    //Read a data frame for TSEA analysis. The data is expected to be column major and contain only numeric data except the identifier column.
    let readDataFrame (identifierCol:string) (path:string) : Frame<string,string> =
        let f' = 
            Frame.ReadCsv(
                path,
                true,
                separators="\t"
            )

        let columnTypes = 
            f'.ColumnKeys 
            |> Seq.map (fun s -> sprintf "%s=float"s)
            |> String.concat ","

        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema=columnTypes
        )
        |> Frame.indexRows identifierCol

    let readSigMatrix (path:string): Frame<string*string,string>=
        Frame.ReadCsv(
            path,
            true,
            separators="\t",
            schema="string,string,string,string,string,string,string,string,string,string,string"
        )
        |> Frame.indexRowsUsing (fun os -> os.GetAs<string>("NAME") => os.GetAs<string>("BINCODE"))
