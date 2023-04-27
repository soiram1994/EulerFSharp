namespace Me.Core

open SimpleTypes
open Domain

module HabitCreation = 
       let createHabbit name measure freq = 
            let desc = SmallString.create name
            let typeOfMeasure = TypeOfMeasure.create measure
            let frequency = Frequency.create freq
            {
            Frequency=frequency
            TypeOfMeasure=typeOfMeasure
            Desc=desc
            }
        

