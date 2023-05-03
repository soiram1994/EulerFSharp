namespace Euler

module Ex4 = 
    let isPalindrome (number:int )= 
                        let stringNum = string number  
                        let charArray  = stringNum.ToCharArray()
                        let reverse  = Array.rev charArray
                        charArray |> Seq.forall2  (fun x y -> x=y) reverse
    let comb lst =
            let combHelper el lst =
                lst |> List.map (fun lstEl -> el::[lstEl])
            let filterOut el lst =
                lst |> List.filter (fun lstEl -> lstEl <> el)
            lst |> List.map (fun lstEl -> combHelper lstEl (filterOut lstEl lst)) 
                |> List.concat 
                |> List.map (fun x -> x[0]*x[1]) 
                |> List.where (fun x -> isPalindrome x)
                |> List.sort
    List.ofSeq {999..-1..100} |> comb |> List.rev |> List.head |> printfn "%d"