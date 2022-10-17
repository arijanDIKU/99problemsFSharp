#load "P0.fsx"
open P0

// Problem 11: Only elements with duplicates are transferred as (N E) lists others are simply copied.
type Encoding = 
    | Single of char
    | Multiple of int*char 

let encodeModified ls = 
    encode ls |> List.map (fun (n,e) -> if n=1 then Single e else Multiple (n,e)) 



// Problem 12: reverse of problem 11
let decodeModified ls = 
    ls 
    |> List.map (fun x -> match x with 
                          | Single c -> List.init 1 (fun _ -> c)
                          | Multiple (n,e) -> List.init n (fun _ -> e)) 
    |> List.reduce (fun x y -> x@y) 


// Problem 13: L
