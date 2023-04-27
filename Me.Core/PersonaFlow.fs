namespace Me.Core

open SimpleTypes

module PersonaFlow = 
    let applyDmg persona dmg = 
        let result  = Persona.takeDmg persona dmg
        match result with
        | Dead -> Persona.die persona
        | Alive a -> a
    let gainExperience persona xp = 
        let reward  = XpReward.create xp
        Experience.gainXp persona.Experience reward
        

        