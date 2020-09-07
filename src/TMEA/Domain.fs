namespace TMEA

open FSharp.Stats
open FSharp.Stats.ML
open BioFSharp
open BioFSharp.Stats
open BioFSharp.Stats.OntologyEnrichment

type TMEASetDescriptor = {
    OntologyTerm : string
    PValue : float
    BinSize: int
    WeightSum: float
} with
    static member create ontTerm pVal binSize wS =
        {   
            OntologyTerm    = ontTerm
            PValue          = pVal
            BinSize         = binSize
            WeightSum       = wS
        }

type TMEACharacterization= {
    RawData:                OntologyItem<float> []
    NegativeDescriptor:     TMEASetDescriptor []
    PositiveDescriptor:     TMEASetDescriptor []
    BootstrapIterations:    int
} with
    static member create raw pos neg iter = 
        {
            RawData             = raw
            PositiveDescriptor  = pos
            NegativeDescriptor  = neg
            BootstrapIterations = iter
        }

type TMEAParameters = {
    MissingKey          : string
    BootstrapIterations : int
    Verbose             : bool
} with
    static member create mK bI v =
        {
            MissingKey          = mK
            BootstrapIterations = bI
            Verbose             = v
        }

type TMEAResult = {
    AnalysisParameters  : TMEAParameters
    Data                : float [] []
    EntityNames         : string []
    Timepoints          : string []
    AnnotationMap       : Map<string,string[]>
    SingularValues      : Vector<float>
    Constraints         : Matrix<float>
    ConstraintPotentials: Matrix<float>
    Characterizations   : TMEACharacterization []
} with 
    static member create parameters data ens tps am svs cs cps tmeacs =
        {
            AnalysisParameters      = parameters
            Data                    = data
            EntityNames             = ens
            Timepoints              = tps
            AnnotationMap           = am
            SingularValues          = svs
            Constraints             = cs
            ConstraintPotentials    = cps
            Characterizations       = tmeacs
        }

type FASWeightData = {
    FASName     : string
    FullDist    : (string*float) []
    PosDist     : (string*float) []
    NegDist     : (string*float) []
} with
    static member create n fd pd nd =
        {
            FASName     = n
            FullDist    = fd
            PosDist     = pd
            NegDist     = nd
        }