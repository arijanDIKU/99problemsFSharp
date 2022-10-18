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
                          | Single c -> List.replicate 1 c
                          | Multiple (n,e) -> List.replicate n e) 
    |> List.concat


// Problem 13: Run-length encoding but without creating the sublists for as was done in solution to Problem 11.
let encodeDirect ls =
    ls
    |> List.countBy id 
    |> List.map (fun (e,n) -> match n with 
                              | 1 -> Single e
                              | _ -> Multiple (n,e))


// Problem 14: Duplicate elements of list 
let duplicate ls =
    ls 
    |> List.collect (fun e -> List.replicate 2 e)


// Problem 15: Replicate elements of a list a given number of times
let repli ls r =
    ls 
    |> List.collect (fun e -> List.replicate r e)


// Problem 16: Drop every Nth element
let drop ls n =
    ls
    |> List.indexed |> List.filter (fun (i,c) -> not ( (i+1)%n=0 ) ) 
    |> List.map snd 


// Problem 17: Split list into two parts. Length of first part is given.
let split ls = function
    | out when out<0 || out>=length ls -> failwith "index error"
    | i -> ls.[0..i-1], ls.[i..]
    

// Problem 18: Slice list from two indices inclusive. Count from 1.
let slice ls = function
    | (fa,il) when fa>il || fa<=0 || il>=length ls -> failwith "index error"
    | (i,j) -> ls.[i-1..j-1]


// Problem 19: Rotate a list N places to the left 
let rotate ls n = 
    let index = let len = length ls in (len+n)%len 
    let a,b = List.splitAt index ls in
                   b@a

// Problem 20: Remove the kth element from a list
let removeAt ls = function 
    | out when out<=0 || out>length ls -> failwith "index error"
    | k -> List.removeAt (k-1) ls 

