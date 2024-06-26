

namespace Euler
// The sum of the squares of the first ten natural numbers is,

// The square of the sum of the first ten natural numbers is,

// Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 
// .

// Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
module Ex6 = 
    let first100  = seq {1 .. 1.. 100}
    let sumOfSquares = first100 |>  Seq.map (fun x -> x*x) |> Seq.sum |> float
    let squareOfSum = first100 |> Seq.sum 
    let result  = (float (squareOfSum*squareOfSum)) - sumOfSquares 
    printfn $"%A{result}"
