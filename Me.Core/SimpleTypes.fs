namespace Me.Core
open System

module SimpleTypes = 
    let xpMultiplier = 200
    type DayOfWeek = 
            | Monday
            | Tuesday
            | Wendnesday
            | Thursday
            | Friday
    let createStr (ctor:string->string) maxLen str =
        match str with
        | null -> Error "Null"
        | s when String.length s > maxLen -> Error "String too big." 
        | _ -> Ok (ctor str)
    let createString fieldName (ctor:string->string) maxLen str = 
        if String.IsNullOrEmpty str then
            let msg  = sprintf "%s must not be null or empty" fieldName
            Error msg
        elif str.Length > maxLen then
            let msg  = sprintf "%s must not be greater than %i" fieldName maxLen
            Error msg
        else
            Ok (ctor str)
    let createStringOption fieldName ctor maxLen str = 
        if String.IsNullOrEmpty str then
            Ok None
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be greater than %i" fieldName maxLen
            Error msg
        else 
            Ok (ctor str |> Some)
    let createConstrainedInt fieldName ctor maxLen minLen i = 
        if i<minLen || i>maxLen then
            let msg  = sprintf "%s must not be greater than %i and lowe than %i" fieldName maxLen minLen
            Error msg
        else
            Ok (ctor i)
    type DistinctList<'T> = private DistinctList of List<'T>
    module DistinctList =
        let create list = if List.length list <> List.length (List.distinct list) then
                                failwith "List has duplicate values"
                          else
                                DistinctList list
        let value (DistinctList list) = list
    type SmallString = private SmallString of string
    module SmallString = 
        let create str = 
            let value  = createStr string 20 str
            match value with
            | Ok str -> SmallString str 
            | Error error -> failwith $"Error:{error}"
        let value (SmallString str) = str
    type TimeUnit = TimeUnit of TimeSpan
    type SingularUnit = private SingularUnit of int
    module SingularUnit = 
        let create i = 
            let value = createConstrainedInt (nameof SingularUnit) int 99 1 i
            match value with
            | Ok i -> SingularUnit i
            | Error error -> failwith $"Error: {error}"
    type TypeOfMeasure = 
    | Time of TimeUnit
    | Unit of SingularUnit
    module TypeOfMeasure = 
        let create value = 
            match box value with
            | :? int as i -> Unit (SingularUnit.create i)
            | :? TimeSpan as t -> Time (TimeUnit t)
            | _ -> failwith $"Unsupported type"
    type Frequency =  
     | Daily of unit
     | Weekly of DistinctList<DayOfWeek>
     | Monthly of int
    module Frequency = 
        let create value = 
            match box value with
            | :? unit as u -> Daily u
            | :? List<DayOfWeek> as l -> DistinctList.create l |> Weekly
            | :? int as i -> Monthly i
            | _ -> failwith "Invalid value"
    type Experience = 
        {
        CurrentValue: int
        Limit: int
        }
    type XpReward = private XpReward of int
    module XpReward  =
        let create i = 
            let result  = createConstrainedInt (nameof XpReward) int 1 50 i 
            match result with
                | Ok s -> XpReward s
                | Error e -> failwith e
        let value (XpReward i) = i
    module Experience = 
        type GainExperienceResult = 
        | LevelUp of int
        | NewExperience of Experience
        let create lmt = 
            if lmt < xpMultiplier then
                failwith "Invalid value for experience"
            else
                {
                CurrentValue=0
                Limit=lmt
                }
        let gainXp currentXp reward = 
            let currentValue  = (XpReward.value reward) + currentXp.CurrentValue
            if currentValue >= currentXp.Limit then
                LevelUp (currentValue - currentXp.Limit)
            else
                NewExperience {CurrentValue=currentValue; Limit=currentXp.Limit}
        let reset currentXp  = {currentXp with CurrentValue = 0}
            
    type Level  = private Level of int
    module Level = 
        let create lvl = 
            let value  = createConstrainedInt (nameof Level) int 100 1 lvl
            match value with
            | Ok v -> Level v
            | Error e -> failwith e
        let value (Level lvl) = lvl
        let generateMaxExp lvl  = value lvl * xpMultiplier
    type HitPoints  = private HitPoints of int
    module HitPoints = 
        let create hp = 
            let value = createConstrainedInt (nameof HitPoints) int 100 0 hp
            match value with
            | Ok v -> HitPoints v
            | Error e -> failwith $"Error when creating hit points: {e}"
        let value (HitPoints hp)  = hp
    type PersonaClass = 
        | Archer
        | Mage
        | Warrior
    type Persona = {
        HitPoints: HitPoints
        Class: PersonaClass
        Experience: Experience
        Level: Level
    }
    type CurrentStatus = 
        | Alive of Persona
        | Dead
    module Persona = 
        let create hp cl = 
            {
            HitPoints = HitPoints.create hp
            Class = cl
            Experience = {CurrentValue=0; Limit=xpMultiplier}
            Level = Level 1
            }
        let takeDmg persona dmg  = 
            let resultHp  = HitPoints.value persona.HitPoints - dmg
            if resultHp <= 0 then
                Dead
            else
                Alive {
                HitPoints= HitPoints.create resultHp
                Class=persona.Class
                Level = persona.Level
                Experience = persona.Experience
                }
        let die persona = {persona with HitPoints=HitPoints.create 100; Experience = Experience.reset persona.Experience }