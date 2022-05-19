open System 


//Problem 1: Last element of a list 
let rec last = function
           | [] -> failwith "empty list"
           | [x] -> x
           | _::xs -> last xs 


//Problem 2: Second last element of a list  
let rec las' = function
               | [] -> failwith "empty list"
               | [x] -> failwith "only one element" 
               | [x;_] -> x 
               | _::xs -> las' xs 


//Problem 3: k'th element of list where the first element is number 1 
let rec kth k = function
            | [] -> failwith "empty list or index too high"
            | x::xs -> if k=1 then x else kth (k-1) xs 

//Problem 4: number of elements 
let rec length = function
                 | [] -> 0 
                 | _::xs -> 1+length xs  


//Problem 5: reverse a list
let rec rev = function 
              | [] -> []
              | x::xs -> (rev xs)@[x] 


//Problem 6: check if list is a palindrome (i.e. can be read forward and backwards)
let isPalindrome = function
                   | [] -> true
                   | xs -> xs=rev xs

//Problem 7: flatten list of lists 
let rec flatten = function 
                  | [] -> []
                  | x::xs -> x@flatten xs


//Problem 8: eliminate consecutive duplicates 
let rec compress = function
                   | [] -> []
                   | [x] -> [x]
                   | [x;y] -> if x=y then [x] else [x;y]
                   | x::y::zs -> if x=y then compress (y::zs) else x::compress (y::zs)


//Problem 9: put consecutive duplicates into seperate sublists
let rec pack = function

