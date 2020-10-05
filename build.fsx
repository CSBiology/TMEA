#r "paket:
nuget BlackFox.Fake.BuildTask
nuget Fake.Core.Target
nuget Fake.Core.Process
nuget Fake.Core.ReleaseNotes
nuget Fake.IO.FileSystem
nuget Fake.DotNet.Cli
nuget Fake.DotNet.MSBuild
nuget Fake.DotNet.AssemblyInfoFile
nuget Fake.DotNet.Paket
nuget Fake.DotNet.FSFormatting
nuget Fake.DotNet.Fsi
nuget Fake.DotNet.NuGet
nuget Fake.Api.Github
nuget Fake.DotNet.Testing.Expecto //"

#load ".fake/build.fsx/intellisense.fsx"
open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing
open Fake.IO.Globbing.Operators
open Fake.DotNet.Testing
open Fake.Tools
open Fake.Api
open Fake.Tools.Git

[<AutoOpen>]
module MessagePrompts =

    let prompt (msg:string) =
        System.Console.Write(msg)
        System.Console.ReadLine().Trim()
        |> function | "" -> None | s -> Some s
        |> Option.map (fun s -> s.Replace ("\"","\\\""))

    let rec promptYesNo msg =
        match prompt (sprintf "%s [Yn]: " msg) with
        | Some "Y" | Some "y" -> true
        | Some "N" | Some "n" -> false
        | _ -> System.Console.WriteLine("Sorry, invalid answer"); promptYesNo msg


Target.initEnvironment ()

let release = ReleaseNotes.load "RELEASE_NOTES.md"

//Nuget package info
let authors = "Kevin Schneider, Benedikt Venn, Timo Muehlhaus"
let title = "TMEA"
let owners = "Kevin Schneider, Benedikt Venn, Timo Muehlhaus"
let description = "TMEA (Thermodynamically Motivated Enrichment Analysis) is a thermodynamically motivated approach to gene set enrichment analysis"
let licenseUrl = "https://github.com/CSBiology/TMEA/blob/master/LICENSE"
let projectUrl = "https://github.com/CSBiology/TMEA"
let iconUrl = ""
let tags = "F# FSharp bioinformatics biostatistics datascience biology GSEA geneset-enrichment-analysis"
let releaseNotes = (release.Notes |> String.concat "\r\n")
let repositoryUrl ="https://github.com/CSBiology/TMEA"

let stableVersion = SemVer.parse release.NugetVersion

let pkgDir = "pkg"

let clean = BuildTask.create "Clean" [] {
    !! "src/**/bin"
    ++ "src/**/obj"
    ++ "pkg"
    |> Shell.cleanDirs 
}

let build = BuildTask.create "Build" [clean.IfNeeded] {
    !! "src/**/*.*proj"
    ++ "dev/*.*.proj"
    |> Seq.iter (Fake.DotNet.DotNet.build id)
}

let pack = BuildTask.create "Pack" [clean; build.IfNeeded] {
    if promptYesNo (sprintf "creating stable package with version %i.%i.%i OK?" stableVersion.Major stableVersion.Minor stableVersion.Patch ) then
        !! "src/**/*.*proj"
        |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->
            let msBuildParams =
                {p.MSBuildParams with 
                    Properties = ([
                        "Version",(sprintf "%i.%i.%i" stableVersion.Major stableVersion.Minor stableVersion.Patch )
                        "Authors",      authors
                        "Title",        title
                        "Owners",       owners
                        "Description",  description
                        "PackageLicenseUrl",   licenseUrl
                        "PackageProjectUrl",   projectUrl
                        "IconUrl",      iconUrl
                        "PackageTags",         tags
                        "PackageReleaseNotes", releaseNotes
                        "RepositoryUrl",repositoryUrl
                        "RepositoryType","git"
                    ] @ p.MSBuildParams.Properties)
                }
            {
                p with 
                    MSBuildParams = msBuildParams
                    OutputPath = Some pkgDir
            }
        ))
    else failwith "aborted"
}

let packPrerelease = BuildTask.create "PackPrerelease" [clean; build.IfNeeded] {
    !! "src/**/*.*proj"
    |> Seq.iter (Fake.DotNet.DotNet.pack (fun p ->

        printfn "Please enter pre-release package suffix"
        let suffix = System.Console.ReadLine()
        let prereleaseTag = (sprintf "%s-%s" release.NugetVersion suffix)

        if promptYesNo (sprintf "package tag will be %s OK?" prereleaseTag )
            then 
                let msBuildParams =
                    {p.MSBuildParams with 
                        Properties = ([
                            "Version",prereleaseTag
                            "Authors",      authors
                            "Title",        title
                            "Owners",       owners
                            "Description",  description
                            "PackageLicenseUrl",   licenseUrl
                            "PackageProjectUrl",   projectUrl
                            "IconUrl",      iconUrl
                            "PackageTags",         tags
                            "PackageReleaseNotes", releaseNotes
                            "RepositoryUrl",repositoryUrl
                            "RepositoryType","git"
                        ] @ p.MSBuildParams.Properties)
                    }
                {
                    p with 
                        VersionSuffix = Some suffix
                        OutputPath = Some pkgDir
                        MSBuildParams = msBuildParams
                }
            else
                failwith "aborted"
    ))
}

let _all = BuildTask.createEmpty "All" [clean; build; pack]

BuildTask.runOrDefaultWithArguments _all