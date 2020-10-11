module Project01_02

//
// max L
//
// Returns maximum element of L
// 
// Examples: max []          => raises an exception (Unhandled Exception: System.ArgumentException: The input sequence was empty.)
//           max [-2; 4]     => 4
//           max [34]        => 34
//           max [10; 10; 9] => 10
//           max ['a'; 'e'; 'c'] => e
// 
// You may not call List.max directly in your solution.
// 

// | [] -> raise (System.ArgumentException("The input sequence was empty."))

let max L =
    match L with
    | [] -> raise (System.ArgumentException("The input sequence was empty."))
    | _  ->
    let rec _max L maxSoFar =
        match L with
        | [] -> maxSoFar
        | e::rest when  e > maxSoFar -> _max rest e
        | _::rest -> _max rest maxSoFar
    _max L L.Head


[<EntryPoint>]
let main argv =
    printfn "Testing Project 02: max"

    let max2 = max [-2; 4]

    if max2 = 4 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let max3 = max [34]
    if max3 = 34 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let max4 = max [10; 10; 9]
    if max4 = 10 then
        printfn "Passed!"
    else
        printfn "Failed!" 
        
    let max5 = max ['a'; 'e'; 'c']
    if max5 = 'e' then
        printfn "Passed!"
    else
        printfn "Failed!"
    
    printfn ""
    0 // return an integer exit code
    

