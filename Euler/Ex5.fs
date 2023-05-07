
namespace Euler

// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

module Ex5 =
    let isDivisibleFor start ending  number = 
        if (ending< start) then
            failwith "The provided range of numbers is not valid."
        seq {start .. ending} |> Seq.forall (fun x -> number % x = 0)
    let infiniteSeq start = Seq.initInfinite (fun i -> start+i)
    infiniteSeq 2520 |> Seq.find (fun x -> isDivisibleFor 1 20 x) |> printfn "%d"
        