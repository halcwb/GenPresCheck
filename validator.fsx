
#time

#load "../FormularyParser/formularyParser.fsx"
      "../ExcelMedicationParser/excelParser.fsx"

open System


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =

    open System

    open System.Text.RegularExpressions

    let regex s = new Regex(s)

    let apply f (s: string) = f s

    let nullOrEmpty = apply String.IsNullOrEmpty

    let notNullOrEmpty = nullOrEmpty >> not

    let splitAt (s1: string) (s2: string) =
        s2.Split([|s1|], StringSplitOptions.None)

    let arrayConcat (cs : char[]) = String.Concat(cs)

    let replace (s1: String) s2 s = (s |> apply id).Replace(s1, s2)

    let trim s = (s |> apply id).Trim()

    let substring n l s = (s |> apply id).Substring(n, l)

    let toLower s = (s |> apply id).ToLower()

    let eqsCapsInsens s1 s2 =
        s1 |> trim |> toLower = (s2 |> trim |> toLower)

    let startsWithCapsInsens s2 s1 =
        let s1, s2 = s1 |> trim |> toLower, s2 |> trim |> toLower
        let l = 
            if s2 |> String.length > (s1 |> String.length) then 
                s1 |> String.length
            else s2 |> String.length
        s1 
        |> substring 0 l
        |> ((=) s2)

    /// Count the number of times character
    /// c appears in string t
    let countChar c t =
        if String.IsNullOrEmpty(c) then "Cannot count empty string in text: '" + t + "'" |> failwith
        (c |> regex).Matches(t).Count

    /// Count the number of times that a 
    /// string t starts with character c
    let countFirstChar c t =
        let _, count = 
            if String.IsNullOrEmpty(t) then (false, 0)
            else
                t |> Seq.fold(fun (flag, dec) c' -> if c' = c && flag then (true, dec + 1) else (false, dec)) (true, 0) 
        count



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tuple =

    let tuple x1 x2 = (x1 , x2)


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Validator =


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ValueUnit =
        type ValueUnit = { Value : float ; Unit : string }

        let create v u = { Value = v ; Unit = u }

        let normalize (vu: ValueUnit) =
            match vu.Unit with
            | _ when vu.Unit = "year"  -> vu.Value * 365.
            | _ when vu.Unit = "month" -> vu.Value * 30. 
            | _ when vu.Unit = "week"  -> vu.Value * 7.
            | _ when vu.Unit = "day"   -> vu.Value
            | _ when vu.Unit = "dag"   -> vu.Value
            | _ when vu.Unit = "kg"    -> vu.Value * 1000.
            | _ when vu.Unit = "gram"  -> vu.Value
            | _ when vu.Unit = "gr"  -> vu.Value
            | _ -> 
                sprintf "Cannot normalize %A" vu
                |> failwith 


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


        let isInRange (min : ValueUnit Option) (max : ValueUnit Option) (vu : ValueUnit Option) =
            match min, max, vu with
            | None, None, _ -> true
            | Some min', Some max', Some vu' -> 
                min' |> ValueUnit.normalize <= (vu' |> ValueUnit.normalize) &&
                max' |> ValueUnit.normalize >= (vu' |> ValueUnit.normalize)
            | Some min', None, Some vu' ->
                min' |> ValueUnit.normalize <= (vu' |> ValueUnit.normalize)
            | None, Some max', Some vu' ->
                max' |> ValueUnit.normalize >= (vu' |> ValueUnit.normalize)   
            | _ -> false 

        let matchAgeTypeWith targ rule =
            match (targ, rule) with
            | _, AllAge            
            | Premature, Premature 
            | Neonate, Neonate     
            | Aterm, Aterm
            | Aterm, Neonate       
            | Child, Child         
            | Adult, Adult         
            | AllAge, AllAge -> true
            | _ -> false

        let filter gnd agt cag gag pca bwt wgt (t : TargetFilter) = 
            let cagr = 
                let (ChronAgeRange(min, max)) = t.ChronAgeRange in min, max
            let gagr = 
                let (GestAgeRange(min, max)) = t.GestAgeRange in min, max
            let pcar = 
                let (PostConcAgeRange(min, max)) = t.PostConcAgeRange in min, max
            let bwtr = 
                let (BirthWeightRange(min, max)) = t.BirthWeightRange in min, max
            let wgtr = 
                let (WeightRange(min, max)) = t.WeightRange in min, max

            t.Gender = gnd &&
            t.AgeType |> matchAgeTypeWith agt &&
            (cag |> isInRange (cagr |> fst) (cagr |> snd)) &&
            (gag |> isInRange (gagr |> fst) (gagr |> snd)) &&
            (pca |> isInRange (pcar |> fst) (pcar |> snd)) &&
            (bwt |> isInRange (bwtr |> fst) (bwtr |> snd)) &&
            (wgt |> isInRange (wgtr |> fst) (wgtr |> snd)) 
            

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

        let eqsString s (Route r) = s |> String.eqsCapsInsens r

        let replace = 
            [
                "or", "Oraal"
                "iv", "Intraveneus"
                "im", "Intramusculair"
                "rect", "Rectaal"
                "oog", "Oculair"
                "inh", "Inhalatie"
                "sc", "Subcutaan"
            ]

        let normalize rts =
            rts
            |> List.map (fun s ->
                match replace |> List.tryFind (fst >> (String.eqsCapsInsens s)) with
                | Some s' -> s' |> snd
                | None -> s
            )

        let filter rts (RouteFilter(rf)) =
            rf
            |> List.exists (fun r ->
                rts
                |> normalize
                |> List.exists (fun r' -> r |> eqsString r')
            )
            


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module DoseRange =

        type ValueUnit = ValueUnit.ValueUnit

        type DoseRange = DoseRange of ValueUnit Option * ValueUnit Option

        let create vu = vu |> DoseRange

        let isInRange (vu: ValueUnit Option) (DoseRange(min, max)) =
            match min, max, vu with 
            | None, None, _ 
            | _, _, None -> false
            | Some min', Some max', Some vu' ->
                min'.Value <= vu'.Value &&
                max'.Value >= vu'.Value
            | Some min', None, Some vu' ->
                min'.Value <= vu'.Value
            | None, Some max', Some vu' ->
                max'.Value >= vu'.Value
            


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

        let isValid vu = function
        | Dose (_, dr, _) -> dr |> DoseRange.isInRange vu
        | _ -> false



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

        let filter gen gnd agt cag gag pca bwt wgt rts (r: Rule) =
            (gen |> String.startsWithCapsInsens r.Generic ||
             r.Generic |> String.startsWithCapsInsens gen) &&

            r.Targets |> TargetFilter.filter gnd agt cag gag pca bwt wgt &&
            r.Routes  |> RouteFilter.filter rts
            
        let isValid dos (r: Rule) =
            r.Error
            |> List.fold (fun a dr ->
                dr |> DoseRule.isValid dos && a
            ) true

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
                   

module Check =

    open System

    let objToFloat (o: obj) =
        match o with
        | null -> None
        | _ -> o :?> float |> Some

    let objToString (o: obj) =
        match o with
        | null -> ""
        | _ -> o :?> string

    let parseGeneric s =
        let s = 
            s 
            |> objToString
            |> String.replace "ï" "i"
        match s |> String.splitAt " " |> Array.toList with
        | h::_ -> h
        | _ -> s

    let form = FormularyParser.WebSiteParser.getFormulary ()

    let toRow valid rule (r : ExcelParser.Prescription.Prescription.Row) =
        [
            r.LastName
            r.FirstName
            r.Generic
            r.Route
            (match r.Frequency |> objToFloat with | Some v -> v.ToString() | None -> "")
            r.FreqUnit
            (match r.Dose |> objToFloat with | Some v -> v.ToString() | None -> "")
            r.DoseUnit
            (match r.DoseTotal |> objToFloat with | Some v -> v.ToString() | None -> "")
            r.TotalUnit
            valid.ToString()
            rule
        ]
    
    let check () =
        [
            for p in ExcelParser.Prescription.get () do
                let gen = p.Generic |> parseGeneric
                let gnd = Validator.TargetFilter.AnyGender
                let agt =
                    match p.GestAgeWeeks |> objToFloat with
                    | Some a -> 
                            if a < 37. then 
                                Validator.TargetFilter.AgeType.Premature
                            else Validator.TargetFilter.AgeType.Neonate
                    | None -> Validator.TargetFilter.AgeType.AllAge
                let cag =
                    let ds = 
                        (p.BirthDate - p.Start).Days |> float
                    Validator.ValueUnit.create ds "day"
                    |> Some
                let gag =
                    match (p.GestAgeWeeks |> objToFloat, p.GestAgeDays |> objToFloat) with
                    | Some ws, Some ds -> Validator.ValueUnit.create (ws * 7. + ds) "day" |> Some
                    | _ -> None
                let pca =
                    match (p.GestAgeWeeks |> objToFloat, p.GestAgeDays |> objToFloat) with
                    | Some ws, Some ds -> 
                        match cag with
                        | Some a -> 
                            let v = a.Value + ws * 7. + ds
                            Validator.ValueUnit.create v "day" |> Some
                        | None -> None
                    | _ -> None
                let bwt =
                    match p.BirthWeight |> objToFloat with
                    | Some w -> 
                        if p.BirthWghtUnit |> objToString = "kg" then
                            Validator.ValueUnit.create (w * 1000.) "gram"
                        else Validator.ValueUnit.create w "gram"
                        |> Some
                    | _ -> None
                let wgt =
                    match p.Weight |> objToFloat with
                    | Some w -> 
                        if p.WeightUnit |> objToString = "kg" then
                            Validator.ValueUnit.create (w * 1000.) "gram"
                        else Validator.ValueUnit.create w "gram"
                        |> Some
                    | _ -> None
                let rts =
                    p.Route |> objToString |> String.splitAt "/" |> Array.toList
                let dos =
                    match p.DoseTotal |> objToFloat with
                    | Some v ->
                        Validator.ValueUnit.create v p.TotalUnit |> Some
                    | None -> None
                    
                match Validator.Rule.fromPediatricFormulary form
                      |> List.filter (Validator.Rule.filter gen gnd agt cag gag pca bwt wgt rts) with
                | h::_ -> yield p |> toRow (h |> Validator.Rule.isValid dos) h.Text
                | _ ->
                    yield p |> toRow false ""
        ]



Check.check () |> ExcelParser.ExcelWriter.seqToExcel "check" "medication"
|> List.filter (fun (gen, vld, txt) -> txt |> Option.isSome) // |> List.length
|> List.distinct
|> List.iter (printfn "%A")

Check.form
|> Array.filter (fun d -> d.Generic |> String.startsWithCapsInsens "gentamicine")
|> Validator.Rule.fromPediatricFormulary
|> List.map (fun r -> r.Text)
|> List.iter (printfn "%s")

Check.form
|> Array.filter (fun d -> d.Generic |> String.startsWithCapsInsens "parac")
|> Array.filter (fun dr ->
    dr.Doses
    |> List.exists (fun ds ->
        ds.Routes
        |> List.exists (fun rt ->
            rt.Schedules
            |> List.length > 1
        )
    )
) |> Array.length

//
//Validator.Rule.fromPediatricFormulary (FormularyParser.WebSiteParser.getFormulary ())     
//FormularyParser.WebSiteParser.getFormulary ()
//|> Array.filter (fun d ->
//    d.Doses
//    |> List.exists (fun d ->
//        d.Routes
//        |> List.exists (fun r ->
//            r.Schedules
//            |> List.exists (fun s ->
//                if s.FrequencyText.ToLower().Contains ("bolus") then true
//                else false
//            )
//        )
//    )
//) 
//|> Array.toList
//|> List.collect (fun d ->
//    d.Doses
//    |> List.collect (fun d ->
//        d.Routes
//        |> List.collect (fun r ->
//            r.Schedules
//            |> List.map (fun s ->
//                s.TargetText + ": " + s.FrequencyText + ", " + s.ValueText + " " + s.Unit
//            )
//        )
//    )
//)
//|> List.iter (printfn "%A")

  