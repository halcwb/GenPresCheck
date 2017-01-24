
#time

#load "../FormularyParser/formularyParser.fsx"
      "../ExcelMedicationParser/excelParser.fsx"
      "../Zindex.TypeProvider/zindex.fsx"


open System


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DateTime =
    open System
    

    /// Returns age in years, months, weeks and days
    /// Given a DateTime 'date1' and a DateTime 'date2'
    /// where 'date1' should be >= 'date2' to get a positive
    /// age, otherwise age will be negative
    let now = DateTime.Now
    let daysInYear = (now.AddYears(1) - now).Days
    let daysInMonth = daysInYear / 12
    let daysInWeek = 7

    let age (date1:DateTime) (date2: DateTime) = 

        let inv, date1, date2 = 
            if date1 > date2 then false, date1, date2 else true, date2, date1

        let y, date2 = date1.Year - date2.Year, date2.AddYears(date1.Year - date2.Year)
        let y, date2 = 
            if (date1 - date2).Days < 0 then y - 1, date2.AddYears(-1) else y, date2

        let m, date2 = 
            if date1.Year = date2.Year then 
                date1.Month - date2.Month, date2.AddMonths(date1.Month - date2.Month) 
            else 
                (12 - date2.Month) + date1.Month, date2.AddMonths((12 - date2.Month) + date1.Month)
        let m, date2 = 
            if (date1 - date2).Days < 0 then m - 1, date2.AddMonths(-1) else m, date2

        let d = (date1 - date2).Days
        let d, w = d % 7, d / 7
        
        if inv then -y, -m, -w, -d
        else y, m, w, d

    let ageNow = age DateTime.Now

    let ageToString  years months weeks days age =
        let pluralize n s = 
            match n with 
            | 1 -> n.ToString() + " " + (s |> fst)
            | _ -> n.ToString() + " " + (s |> snd)
        let yr, mo, wk, d = age
        let s =
            match yr, mo with 
            | _ when yr > 10 -> pluralize yr years
            | _ when yr > 0  -> pluralize yr years + " " + pluralize mo months
            | _ when mo > 0  -> pluralize mo months + " " + pluralize wk weeks
            | _              -> pluralize wk weeks + " " + pluralize d days
        s.Trim() 

    let ageToStringDutch = ageToString ("jaar", "jaar") 
                                       ("maand", "maanden") 
                                       ("week", "weken") 
                                       ("dag", "dagen")

    let getAgeFromDate = ageNow >> ageToStringDutch

    let dateDiffToAgeString dt1 dt2 = age dt1 dt2 |> ageToStringDutch



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
module Double =

    open System

    let tryParse s =
        let (b, n) = Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) //Double.TryParse(s)
        if b then Some n else None

    let stringToFloat32 s =
        match s |> tryParse with
        | Some v -> float32 v
        | None -> 0.f

    let stringToFloat s =
        match s |> tryParse with
        | Some v -> float v
        | None -> 0.

    /// Calculates the number of decimals that 
    /// should be shown according to a precision 
    /// number n that specifies the number of non
    /// zero digits in the decimals
    let getPrecision n f =
        let f = (f |> string).Split([|'.'|])
        let n = n - if f.[0] = "0" then 0 else f.[0].Length
        let n = if n < 0 then 0 else n
        if f.Length = 1 then
            n
        else
            let c = (f.[1] |> String.countFirstChar '0')
            c + n

    /// Fix the precision of a float f to
    /// match a minimum of non zero digits n
    let fixPrecision n (f: float) =
        Math.Round(f, f |> getPrecision n)

    let toString n f =
        f
        |> fixPrecision n
        |> string


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
            | _ when vu.Unit = "gr"    -> vu.Value
            | _ -> 
                sprintf "Cannot normalize %A" vu
                |> failwith 

        let toString (vu: ValueUnit)  = 
            sprintf "%s %s" (vu.Value |> Double.toString 2) vu.Unit

        let increase x ({Value = v; Unit = u}) = { Value = v * x; Unit = u } 

        let rangeToString nm vu1 vu2 =
            let r =
                match vu1, vu2 with
                | Some min, Some max ->
                    sprintf "%s - %s" 
                        (min |> toString) 
                        (max |> toString)
                | Some min, None -> 
                    sprintf "> %s" 
                        (min |> toString)
                | None, Some max ->
                    sprintf "< %s"
                        (max |> toString)
                | None, None -> ""
            if r |> String.IsNullOrWhiteSpace then ""
            else sprintf "%s: %s " nm r


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

        let toString (tg: TargetFilter) =
            let rangeToString = ValueUnit.rangeToString
            let gn = 
                match tg.Gender with
                | Male -> "male "
                | Female -> "female "
                | AnyGender -> ""
            let at =
                match tg.AgeType with
                | Neonate -> "neonate "
                | Premature -> "premature "
                | Aterm -> "aterm "
                | Child -> "child "
                | Adult -> "adult "
                | AllAge -> ""
            let ca = 
                let (ChronAgeRange(vu1, vu2)) = tg.ChronAgeRange
                rangeToString "age" vu1 vu2
            let pa = 
                let (PostConcAgeRange(vu1, vu2)) = tg.PostConcAgeRange
                rangeToString "post conc age" vu1 vu2
            let ga = 
                let (GestAgeRange(vu1, vu2)) = tg.GestAgeRange
                rangeToString "gest age" vu1 vu2
            let bw = 
                let (BirthWeightRange(vu1, vu2)) = tg.BirthWeightRange
                rangeToString "birthweight" vu1 vu2
            let aw = 
                let (WeightRange(vu1, vu2)) = tg.WeightRange
                rangeToString "weight" vu1 vu2
            sprintf "%s %s %s %s %s %s %s" gn at ca pa ga bw aw
            |> String.trim
            

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

        let indicationToString (Indication(s)) = s

        let contraIndToString (ContraIndication(s)) = s

        let toString (DiagnoseFilter(ids, cds)) =
            let ind =
                ids
                |> List.map indicationToString
                |> String.concat ", "
            let cnd = 
                cds
                |> List.map contraIndToString
                |> String.concat ", "
            sprintf "Indications: %s, Contra-indications %s" ind cnd


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

        let routeToString (Route(s)) = s

        let toString (RouteFilter(rl)) =
            rl
            |> List.map routeToString
            |> String.concat ", "
            |> sprintf "%s"

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

        let toString (DoseRange(vu1, vu2)) =
            ValueUnit.rangeToString "Dose range" vu1 vu2

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

        let toString (TimeRange(vu1, vu2)) =
            ValueUnit.rangeToString " in " vu1 vu2


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


        let toString (FrequencyRange(vu1, vu2)) =
            ValueUnit.rangeToString "Frequency range" vu1 vu2
            


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

        let createAdvWarnErr fv (DoseRange.DoseRange(min, max)) tv =
            match min, max with
            // When there only is an advice, calculate warning and error
            | Some _, Some _ when min = max ->
                let adv =  [ createDose fv (DoseRange.create (min, max)) tv ]
                let war = [ createDose fv 
                                (DoseRange.create 
                                (min |> Option.bind ((ValueUnit.increase 0.9) >> Some), 
                                    max |> Option.bind ((ValueUnit.increase 1.1) >> Some))) tv ] 
                let err = [ createDose fv 
                                (DoseRange.create 
                                (min |> Option.bind ((ValueUnit.increase 0.5) >> Some), 
                                    max |> Option.bind ((ValueUnit.increase 1.5) >> Some))) tv ] 
                adv, war, err
            // When there is a range, only use error
            | _ -> [], [], [ createDose fv (DoseRange.create (min, max)) tv ]

        let createBolus dv tv = (dv , tv) |> Bolus

        let createContinuous dv = dv |> Continuous

        let isValid vu = function
        | Dose (_, dr, _) -> dr |> DoseRange.isInRange vu
        | _ -> false

        let toString = function
            | Dose(fr, dr, tr) -> 
                sprintf "%s %s %s" 
                    (fr |> FrequencyRange.toString)
                    (dr |> DoseRange.toString)
                    (match tr with Some tr' -> tr' |> TimeRange.toString | None -> "")
            | Bolus(dr, tr) ->
                sprintf "%s %s"
                    (dr |> DoseRange.toString)
                    (match tr with Some tr' -> tr' |> TimeRange.toString | None -> "")
            | Continuous dr ->
                sprintf "%s"
                    (dr |> DoseRange.toString)



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

        let toString = function
            | PRN pt ->
                match pt with
                | Continuous -> "prn continuous" 
                | Discontinuous -> "prn discontinous"
                | Timed -> "prn timed"
            | NonPRN pt ->
                match pt with
                | Continuous -> "continuous" 
                | Discontinuous -> "discontinous"
                | Timed -> "timed"


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ProductFilter =

        type ProductFilter = ProductFilter of GenericProduct list * TradeProduct list
        and GenericProduct = GenericProduct of Product
        and TradeProduct = TradeProduct of Product
        and Product = { Id : int; Name : string }

        let create gps tps = (gps , tps) |> ProductFilter

        let createProduct id nm = { Id = id ; Name = nm }

        let createGeneric id nm = createProduct id nm |> GenericProduct

        let createTrade id nm = createProduct id nm |> TradeProduct

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
                Source : string
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

        let create src gnc trg dgn prs drg rts adv wrn err txt =
            {
                Source = src
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

        let toString (r: Rule) =
            sprintf "Generic: %s, Target: %s, Diagnoses: %s, Prescription: %s, Routes: %s, Advice: %s" 
                r.Generic 
                (r.Targets |> TargetFilter.toString)
                (r.Diagnoses |> DiagnoseFilter.toString)
                (r.Prescription |> PrescriptionFilter.toString)
                (r.Routes |> RouteFilter.toString)
                (r.Error |> List.fold (fun a r -> a + " " + (r |> DoseRule.toString) ) "")

        let filter gen gnd agt cag gag pca bwt wgt rts (r: Rule) =
            gen       |> String.startsWithCapsInsens r.Generic &&
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
                    else ValueUnit.create (float freq.Max) freq.Unit |> Some
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
            let src = "Pediatric"
            [
                for drug in drugs do
                    printfn "Getting Pediatric rules for: %s" drug.Generic
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
                                    let adv, warn, err = DoseRule.createAdvWarnErr frq dr None
                                    let txt = 
                                        sprintf "%s: %s, %s, %s, %s, %s %s" 
                                                gnc
                                                dose.Indication 
                                                route.Name 
                                                schedule.TargetText 
                                                schedule.FrequencyText 
                                                schedule.ValueText
                                                un
                                    yield create src gnc trg dgn prs ProductFilter.empty rts adv warn err txt
                                | None -> () ]

        let fromGStandard (gpps: Zindex.GenPresProduct.GenPresProduct[])
                          (gsrs: Zindex.DoseRule.DoseRule[]) =
            let src = "GStandard"
            [
                for gpp in gpps do
                    printfn "Getting GStand rules for: %s" gpp.Name
                    for gp in gpp.GenericProducts do
                        yield!
                            gsrs
                            |> Array.filter (fun gsr ->
                                gsr.GenericProduct
                                |> Array.exists (fun gp' -> 
                                    gp'.Id = gp.Id &&
                                    (gsr.CareGroup = "alle" || 
                                     gsr.CareGroup = "intensieve")
                                ) &&
                                gpp.Route
                                |> Array.exists (String.eqsCapsInsens gsr.Route)
                            )
                            |> Array.map (fun gsr -> 
                                let gnc = gpp.Name
                                let trg =
                                    let gnd = 
                                        match gsr.Gender with
                                        | _ when gsr.Gender = "F" -> TargetFilter.Female
                                        | _ when gsr.Gender = "M" -> TargetFilter.Male
                                        | _ -> TargetFilter.AnyGender
                                    let car =
                                        let min = gsr.Age.Min |> Option.bind (fun a -> ValueUnit.create a "month" |> Some)
                                        let max = gsr.Abs.Max |> Option.bind (fun a -> ValueUnit.create a "month" |> Some)
                                        TargetFilter.ChronAgeRange (min, max)
                                    let gar = TargetFilter.GestAgeRange     (None, None)
                                    let pcr = TargetFilter.PostConcAgeRange (None, None)
                                    let bwr = TargetFilter.BirthWeightRange (None, None)
                                    let awr =
                                        let min = gsr.Weight.Min |> Option.bind (fun w -> ValueUnit.create w "kg" |> Some)
                                        let max = gsr.Weight.Max |> Option.bind (fun w -> ValueUnit.create w "kg" |> Some)
                                        TargetFilter.WeightRange (min, max)
                                    let bmr = 
                                        let min = gsr.BSA.Min |> Option.bind (fun m -> ValueUnit.create m "m2" |> Some)
                                        let max = gsr.BSA.Max |> Option.bind (fun m -> ValueUnit.create m "m2" |> Some)
                                        TargetFilter.BsaRange (min, max)
                                    TargetFilter.create gnd TargetFilter.AllAge car gar pcr bwr awr bmr
                                let dgn =
                                    let inds = [DiagnoseFilter.createIndication gsr.Indication]
                                    DiagnoseFilter.create inds []
                                let prs = PrescriptionFilter.discontinuousNonPRN
                                let drg =
                                    let gps = [ ProductFilter.createGeneric gp.Id gp.Name ]
                                    let tps = 
                                        gp.PrescriptionProducts
                                        |> Array.collect (fun pp -> pp.TradeProducts)
                                        |> Array.toList
                                        |> List.map (fun tp -> ProductFilter.createTrade tp.Id tp.Name)
                                    ProductFilter.create gps tps
                                let rtf = [ gsr.Route |> RouteFilter.createRoute ] |> RouteFilter.create
                                let adv = []
                                let wrn = []
                                let err = []

                                create src gnc trg dgn prs drg rtf [] [] [] "GStand Rule"
                            )
            ]


//Validator.Rule.fromGStandard (Zindex.Database.getProducts()) (Zindex.DoseRule.get())
//|> Seq.length                   
//
//Zindex.DoseRule.get() 
//|> Array.map (fun gsr -> gsr.DoseType)
//|> Array.distinct

module Check =

    open System

    let objToFloat (o: obj) =
        match o with
        | null -> None
        | _ -> o :?> float |> Some

    let floatToString x = 
        match x |> objToFloat with
        | Some x -> string x
        | None -> ""

    let objToString (o: obj) =
        match o with
        | null -> ""
        | _ -> o |> string

    let objToDate (o: obj) =
        match o with
        | null -> None
        | _ -> o :?> DateTime |> Some

    let parseGeneric s =
        let s = 
            s 
            |> objToString
            |> String.replace "ï" "i"
        match s |> String.splitAt " " |> Array.toList with
        | h::_ -> h
        | _ -> s

    let form = FormularyParser.WebSiteParser.getFormulary ()

    let toRow valid pedtext gsttext (r : ExcelParser.Prescription.Prescription.Row) =
        let format = "MM-dd-yy"

        [
            r.Department
            r.HospitalNumber |> objToString
            r.LastName
            r.FirstName
            r.BirthDate.ToString(format)
            (r.BirthDate |> DateTime.dateDiffToAgeString r.Start)
            r.GestAgeWeeks |> objToString
            r.GestAgeDays  |> objToString
            r.BirthWeight  |> objToString
            r.BirthWghtUnit |> objToString
            r.Weight |> objToString
            r.WeightUnit |> objToString
            r.Start.ToString(format)
            r.Prescriber
            (match r.Stop |> objToDate with | None -> "" | Some dt -> dt.ToString(format))
            r.Prescriber2
            r.Generic
            r.Route
            r.Frequency |> objToString
            r.FreqUnit
            r.Dose |> objToString
            r.DoseUnit
            r.DoseTotal |> objToString
            r.TotalUnit
            r.Text |> objToString
            valid.ToString()
            pedtext
            gsttext
        ]
    
    let check path =
        let pedrs = 
            Validator.Rule.fromPediatricFormulary form
        let gstrs =
            let gpps = Zindex.Database.getProducts ()
            let gsrs = Zindex.DoseRule.get()
            Validator.Rule.fromGStandard gpps gsrs
        [
            for p in ExcelParser.Prescription.get path do
                printfn "Processing: %A" p
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
                        (p.Start - p.BirthDate).Days |> float
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
                    
                match pedrs
                      |> List.tryFind (Validator.Rule.filter gen gnd agt cag gag pca bwt wgt rts) ,
                      gstrs 
                      |> List.tryFind (Validator.Rule.filter gen gnd agt cag gag pca bwt wgt rts) with
                | Some pedr, Some gstr -> 
                    let isValid =
                        pedr |> Validator.Rule.isValid dos &&
                        gstr |> Validator.Rule.isValid dos
                    yield p |> toRow isValid pedr.Text gstr.Text
                | Some pedr, None -> 
                    let isValid =
                        pedr |> Validator.Rule.isValid dos
                    yield p |> toRow isValid pedr.Text ""
                | None, Some gstr -> 
                    let isValid =
                        gstr |> Validator.Rule.isValid dos
                    yield p |> toRow isValid "" gstr.Text
                | _ ->
                    yield p |> toRow false "" ""
        ]
        |> List.append [
            [
                "Department"
                "HospitalNumber"
                "LastName"
                "FirstName"
                "BirthDate"
                "Age"
                "GestAgeWeeks"
                "GestAgeDays"
                "BirthWeight"
                "BirthWghtUnit"
                "Weight"
                "WeightUnit"
                "Start"
                "PrescrStart"
                "Stop"
                "PrescrStop"
                "Generic"
                "Route"
                "Frequency"
                "FreqUnit"
                "Dose"
                "DoseUnit"
                "DoseTotal"
                "TotalUnit"
                "Text"
                "Valid"
                "Pediatric"
                "GStandard"
            ]
        ]


let path = IO.Path.Combine(Environment.CurrentDirectory, "prescriptions.xlsx")
Check.check path 
//|> List.map (fun x -> printfn "%A" x; x)
|> ExcelParser.ExcelWriter.seqToExcel "check" "medication"
//|> List.filter (fun (gen, vld, txt) -> txt |> Option.isSome) // |> List.length
//|> List.distinct
//|> List.iter (printfn "%A")

Check.form
|> Validator.Rule.fromPediatricFormulary
|> List.map (fun r -> [r.Text; r |> Validator.Rule.toString])
|> ExcelParser.ExcelWriter.seqToExcel "test" "test"

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

Zindex.Database.getProducts ()
|> Array.filter (fun gpp ->
    gpp.GenericProducts
    |> Array.exists (fun gp -> 
        gp.PrescriptionProducts
        |> Array.exists (fun pp ->
            pp.TradeProducts
            |> Array.exists (fun tp ->
                tp.Denominator > 0
            )
        )
    )
)
|> Array.collect (fun gpp -> 
    gpp.GenericProducts
    |> Array.collect (fun gp ->
        gp.PrescriptionProducts
        |> Array.collect (fun pp ->
            pp.TradeProducts
            |> Array.map (fun tp -> tp.Label)
        )
    )
) |> Array.iter (printfn "%s")
