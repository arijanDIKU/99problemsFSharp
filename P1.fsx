#load "P0.fsx"
open P0

//Problem 11: Only elements with duplicates are transferred as (N E) lists others are simply copied.
type Encoding = 
    Single of Char
    Multiple of int*char 

let encodeModified ls = 
    encode ls 

