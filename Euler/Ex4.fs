namespace Euler

open System.Linq

module Ex4 = 
    let indexOf  sequence value  = sequence |> Seq.findIndex (fun x -> x=value)
    let isPalindrome (number:int )= 
                        let stringNum = string number  
                        let charArray  = stringNum.ToCharArray()
                        let reverse  = Array.rev charArray
                        charArray |> Seq.forall2  (fun x y -> x=y) reverse
    
