
//let a = 10
//a <- 20 (a is not mutable)

let mutable a = 10
a <- 20

let items = [1..5]
List.append items [6]

// items var still contains [1..5], the return of the List.append is the "new" list of [1..6]

let prefix prefixStr baseStr = 
    prefixStr + ", " + baseStr

prefix "Hello" "Pelle"

let names = ["Pelle"; "Mads"; "Haley"]

names 
|> Seq.map prefix "Hello"


let prefixWithHello = prefix "Hello" 

names
|> Seq.map prefixWithHello

let exclaim s =
    s + "!"

names
|> Seq.map prefixWithHello
|> Seq.map exclaim

let bigHello = prefixWithHello >> exclaim //optionally reverse the direction of the arrows to reverse the pipeline

names
|> Seq.map bigHello
|> Seq.sort
