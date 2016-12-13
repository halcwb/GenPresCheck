
#time

#load "../FormularyParser/formularyParser.fsx"


open System


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tuple =

    let tuple x1 x2 = (x1 , x2)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validator =


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueUnit =
        type ValueUnit = { Value : float ; Unit : string }

        let create v u = { Value = v ; Unit = u }


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TargetFilter =

        type ValueUnit = ValueUnit.ValueUnit

        type TargetFilter = 
            {
                Gender : Gender
                AgeType : AgeType 
                ChronAgeRange : ChronAgeRange
                GestAgeRange : GestAgeRange
                PostConcAgeRange : PostConcAgeRange
                BirthWeightRange : BirthWeightRange
                WeightRange : WeightRange
                BsaRange : BsaRange
            }
        and Gender = Male | Female | AnyGender
        and AgeType = Neonate | Premature | Aterm | Child | Adult | AllAge
        and ChronAgeRange = ChronAgeRange of ValueUnit Option * ValueUnit Option
        and GestAgeRange = GestAgeRange of ValueUnit Option * ValueUnit Option
        and PostConcAgeRange = PostConcAgeRange of ValueUnit Option * ValueUnit Option
        and BirthWeightRange = BirthWeightRange of ValueUnit Option * ValueUnit Option
        and WeightRange = WeightRange of ValueUnit Option * ValueUnit Option
        and BsaRange = BsaRange of ValueUnit Option * ValueUnit Option
        
        let create gnd atp cha gta pca bwt awt bsa = 
            {
                Gender = gnd
                AgeType = atp
                ChronAgeRange = cha
                GestAgeRange = gta
                PostConcAgeRange = pca
                BirthWeightRange = bwt
                WeightRange = awt
                BsaRange = bsa
            }

        let emtpyBsaRange = BsaRange (None, None)


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DiagnoseFilter =

        type ValueUnit = ValueUnit.ValueUnit

        type DiagnoseFilter = DiagnoseFilter of Indication list * ContraIndication list
        and Indication = Indication of string
        and ContraIndication = ContraIndication of string

        let create ins cns = (ins , cns) |> DiagnoseFilter

        let createIndication s = s |> Indication

        let createContraindication s = s |> ContraIndication

        let createFromString ins cns = 
            (cns |> List.map createContraindication)
            |> create (ins |> List.map createIndication) 


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module RouteFilter =

        type RouteFilter = RouteFilter of Route list
        and Route = Route of string

        let create rts = rts |> RouteFilter

        let createRoute s = s |> Route


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseRange =

        type ValueUnit = ValueUnit.ValueUnit

        type DoseRange = DoseRange of ValueUnit Option * ValueUnit Option

        let create vu = vu |> DoseRange


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module TimeRange =

        type ValueUnit = ValueUnit.ValueUnit

        type TimeValue = TimeRange of ValueUnit Option * ValueUnit  Option

        let create vu = vu |> TimeRange


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module FrequencyRange =

        type ValueUnit = ValueUnit.ValueUnit

        type FrequencyRange = FrequencyRange of ValueUnit Option * ValueUnit Option

        let create vu = vu |> FrequencyRange

        let empty = FrequencyRange (None, None)

        let once = 
            let vu = ValueUnit.create 1. "" |> Some
            (vu, vu) |> FrequencyRange

        let onceDaily =
            let vu = ValueUnit.create 1. "day" |> Some
            (vu, vu) |> FrequencyRange
            


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseRule =

        type FrequencyRange = FrequencyRange.FrequencyRange
        type DoseRange = DoseRange.DoseRange
        type TimeRange = TimeRange.TimeValue

        type DoseRule =
            | Dose of FrequencyRange * DoseRange * TimeRange Option
            | Bolus of DoseRange * TimeRange Option
            | Continuous of DoseRange

        let createDose fv dv tv = (fv , dv , tv) |> Dose

        let createBolus dv tv = (dv , tv) |> Bolus

        let createContinuous dv = dv |> Continuous


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PrescriptionFilter =

        type PrescriptionFilter = 
            | PRN of PrescriptionType 
            | NonPRN of PrescriptionType
        and PrescriptionType = Continuous | Discontinuous | Timed 

        let continousPRN = Continuous |> PRN

        let discontinuousPRN = Discontinuous |> PRN

        let timedPRN = Timed |> PRN

        let continousNonPRN = Continuous |> NonPRN

        let discontinuousNonPRN = Discontinuous |> NonPRN

        let timedNonPRN = Timed |> NonPRN


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ProductFilter =

        type ProductFilter = ProductFilter of GenericProduct list * TradeProduct list
        and GenericProduct = GenericProduct of Product
        and TradeProduct = TradeProduct of Product
        and Product = { Id : int; Name : string }

        let create gps tps = (gps , tps) |> ProductFilter

        let createProduct id nm = { Id = id ; Name = nm }

        let empty = create [] []


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Rule =

        module FP = FormularyParser

        type TargetFilter = TargetFilter.TargetFilter
        type DiagnoseFilter = DiagnoseFilter.DiagnoseFilter
        type PrescriptionFilter = PrescriptionFilter.PrescriptionFilter
        type DrugFilter = ProductFilter.ProductFilter
        type RouteFilter = RouteFilter.RouteFilter
        type DoseRule = DoseRule.DoseRule

        type Rule =
            {
                Generic : string
                Targets : TargetFilter
                Diagnoses : DiagnoseFilter
                Prescription : PrescriptionFilter
                Drugs : DrugFilter
                Routes : RouteFilter
                Advice : DoseRule list
                Warning : DoseRule list
                Error : DoseRule list
                Text : string
            }

        let create gnc trg dgn prs drg rts adv wrn err txt =
            {
                Generic = gnc
                Targets = trg
                Diagnoses = dgn
                Prescription = prs
                Drugs = drg
                Routes = rts
                Advice = adv
                Warning = wrn
                Error = err
                Text = txt
            }

        let targetTypeGender = function
            | FP.Drug.Target.TargetType.Girl -> TargetFilter.Gender.Female
            | FP.Drug.Target.TargetType.Boy  -> TargetFilter.Gender.Male
            | _ -> TargetFilter.Gender.AnyGender

        let targetTypeAge = function
            | FP.Drug.Target.TargetType.Aterm     -> TargetFilter.Aterm
            | FP.Drug.Target.TargetType.Neonate   -> TargetFilter.Neonate
            | FP.Drug.Target.TargetType.Premature -> TargetFilter.Premature
            | _ -> TargetFilter.AllAge

        let qtyUnTo c min max =
            let min' =
                min 
                |> Option.bind (fun qu ->
                    let q, u = qu |> FP.Drug.Target.getQuantityUnit
                    ValueUnit.create q u|> Some) 
            let max' =
                max 
                |> Option.bind (fun qu ->
                    let q, u = qu |> FP.Drug.Target.getQuantityUnit
                    ValueUnit.create q u|> Some)
            (min', max') |> c

        let qtyUnToChronAge  = qtyUnTo TargetFilter.ChronAgeRange
        let qtyUnToGestAge   = qtyUnTo TargetFilter.GestAgeRange
        let qtyUnToPostMAge  = qtyUnTo TargetFilter.PostConcAgeRange
        let qtyUnToActWght   = qtyUnTo TargetFilter.WeightRange
        let qtyUnToBirthWght = qtyUnTo TargetFilter.BirthWeightRange

        let targetChronAgeRange = function 
            | FP.Drug.Target.TargetAge.Age(min, max) -> qtyUnToChronAge min max
            | _ -> qtyUnToChronAge None None

        let targetGestAgeRange = function 
            | FP.Drug.Target.TargetAge.Pregnancy(min, max) -> qtyUnToGestAge min max
            | _ -> qtyUnToGestAge None None

        let targetPostMenstrAgeRange = function 
            | FP.Drug.Target.TargetAge.PostConc(min, max) -> qtyUnToPostMAge min max
            | _ -> qtyUnToPostMAge None None

        let targetActWeight = function 
            | FP.Drug.Target.TargetWeight.Weight(min, max) -> qtyUnToActWght min max
            | _ -> qtyUnToActWght None None

        let targetBirthWeight = function 
            | FP.Drug.Target.TargetWeight.BirthWeight(min, max) -> qtyUnToBirthWght min max
            | _ -> qtyUnToBirthWght None None

        let freqToFreq (freq : FP.Drug.Frequency.Quantity) =
                let min = 
                    if freq.Min = 0 then None 
                    else ValueUnit.create (float freq.Min) freq.Unit |> Some
                let max = 
                    if freq.Max = 0 then None 
                    else ValueUnit.create (float freq.Min) freq.Unit |> Some
                (min, max) |> FrequencyRange.create

        let freqPrescrType = function
            | FP.Drug.Frequency.PRN freq ->
                PrescriptionFilter.discontinuousPRN ,
                freq |> freqToFreq
            | FP.Drug.Frequency.Frequency freq ->
                PrescriptionFilter.discontinuousNonPRN ,
                freq |> freqToFreq
            | FP.Drug.Frequency.AnteNoctum ->
                PrescriptionFilter.discontinuousNonPRN ,
                FrequencyRange.onceDaily
            | FP.Drug.Frequency.Once
            | FP.Drug.Frequency.Bolus ->
                PrescriptionFilter.discontinuousNonPRN ,
                FrequencyRange.empty

        let fromPediatricFormulary (drugs : FP.Drug.Drug []) =
            [
                for drug in drugs do
                    let gnc = drug.Generic
                    for dose in drug.Doses do
                        let dgn = 
                            let inds = [
                                dose.Indication
                                |> DiagnoseFilter.createIndication ]
                            DiagnoseFilter.create inds []
                        for route in dose.Routes do
                            let rts =
                                [ route.Name ] 
                                |> List.map RouteFilter.createRoute 
                                |> RouteFilter.create
                            for schedule in route.Schedules do
                                let tt, ta, tw =
                                    schedule.Target
                                    |> FP.Drug.Target.getTarget
                                let trg = 
                                    let gnd = tt |> targetTypeGender
                                    let atp = tt |> targetTypeAge
                                    let cha = ta |> targetChronAgeRange
                                    let gta = ta |> targetGestAgeRange
                                    let pma = ta |> targetPostMenstrAgeRange
                                    let bwt = tw |> targetBirthWeight
                                    let awt = tw |> targetActWeight
                                    let bsa = TargetFilter.emtpyBsaRange
                                    TargetFilter.create gnd atp cha gta pma bwt awt bsa
                                let prs, frq =
                                    match schedule.Frequency with 
                                    | Some freq -> freq |> freqPrescrType
                                    | None -> 
                                        PrescriptionFilter.discontinuousNonPRN ,
                                        FrequencyRange.empty

                                let un = schedule.Unit
                                match schedule.Value with
                                | Some (minmax) ->
                                    let min = minmax.Min |> Option.bind (fun m ->
                                        ValueUnit.create m un |> Some)
                                    let max = minmax.Max |> Option.bind (fun m ->
                                        ValueUnit.create m un |> Some)
                                    let dr = DoseRange.create (min, max)
                                    let err = DoseRule.createDose frq dr None 
                                    let txt = 
                                        sprintf "%s: %s, %s, %s, %s, %s %s" 
                                                gnc
                                                dose.Indication 
                                                route.Name 
                                                schedule.TargetText 
                                                schedule.FrequencyText 
                                                schedule.ValueText
                                                un
                                    yield create gnc trg dgn prs ProductFilter.empty rts [] [] [err] txt
                                | None -> () ]
                   



Validator.Rule.fromPediatricFormulary (FormularyParser.WebSiteParser.getFormulary ())     
FormularyParser.WebSiteParser.getFormulary ()
|> Array.filter (fun d ->
    d.Doses
    |> List.exists (fun d ->
        d.Routes
        |> List.exists (fun r ->
            r.Schedules
            |> List.exists (fun s ->
                if s.FrequencyText.ToLower().Contains ("bolus") then true
                else false
            )
        )
    )
) 
|> Array.toList
|> List.collect (fun d ->
    d.Doses
    |> List.collect (fun d ->
        d.Routes
        |> List.collect (fun r ->
            r.Schedules
            |> List.map (fun s ->
                s.TargetText + ": " + s.FrequencyText + ", " + s.ValueText + " " + s.Unit
            )
        )
    )
)
|> List.iter (printfn "%A")

  