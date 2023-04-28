namespace Euler
open System

// The prime factors of 13195 are 5, 7, 13 and 29.

// What is the largest prime factor of the number 600851475143 ?
module Ex3=
    let isPrime (i:float) =
            if i<=1 then
                false
            else
                let sqrtN = Math.Round (sqrt (float i))
                seq {2.0 .. sqrtN} |> Seq.forall (fun x -> i % x <> 0)
    let greatestFactor  (number:double) = 
        let initValue = Math.Round (sqrt number)
        seq {initValue .. -1.0 .. 2} |> Seq.find (fun x -> number % (double x)=0 && isPrime x) 
                                   |> printfn "Result %A"
    greatestFactor 600851475143.0
    
    
    

