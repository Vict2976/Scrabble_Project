module internal Algorithm
    open System
    open System.IO

    let myChars = ['t', 'r', 'o', 'i', 'l', 'b', 's', 's']
    
    let words = System.IO.File.ReadLines("English.txt")
    
    