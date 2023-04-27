namespace Euler

//The prime factors of 13195 are 5, 7, 13 and 29.

//What is the largest prime factor of the number 600851475143 ?
module ExcersiseTwo=
    let isPrime i =
        if i<=1 then
            false
        else
            let sqrtN = int (sqrt (float i))
            seq {2 .. sqrtN} |> Seq.forall (fun x -> i % x <> 0)
    let findGreatestFactor i  = 
        if isPrime i  then
            printf "%d is a prime" i
        else 
            let result = seq {2 .. i/2 - 1} |> Seq.where (fun x -> i % x = 0) |> Seq.max
            printf "%d is the greatest factorial" result

        
        

