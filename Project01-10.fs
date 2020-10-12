module Project01_10

//
// zip L1 L2
//
// Zip two lists
//
// Returns list of tuples
// 
// Examples: 
//          zip [] [] => []
//          zip [1] [1] => [(1, 1)]
//          zip [1; 2; 40] [3; 56; 6] => [(1, 3); (2, 56); (40, 6)]
//          zip [1; 2; 3] ['a'; 'b'; 'c'] => [(1, 'a'); (2, 'b'); (3, 'c')]
//          
// You may not call List.zip directly in your solution.
// 
//

let rec iter F L =
    match L with
    | [] -> ()
    | hd::tl -> F hd; 
                iter F tl

let rec flatten L =
    match L with
    | [ ] -> [ ]
    | [x] -> x
    | hd::rest -> hd@flatten rest

let rec length L =
    match L with
    | [] -> 0
    | e::rest -> 1 + length rest

// let zip L1 L2 =
//     []     //   TO BE IMPLEMENTED

let rec zip L1 L2 =
    match L1, L2 with
    | head1::tail1, head2::tail2 
                    when length L1 > length L2 || length L1 < length L2 
                    -> raise (System.ArgumentException("Lists are not the same length."))
    | [],[]                  -> [ ]
    | [x],[y]                -> [struct(x, y)]
    | hd1::rest1, hd2::rest2 -> [struct(hd1, hd2)] @ zip rest1 rest2
   
    // | (_), ([_]) -> []
    // | (_), ([]) -> []
    // | (_), ([_;_]) -> []
    // | (_,[_;_]) -> []



[<EntryPoint>]
let main argv =
    printfn "Testing Project 10: zip"

    // let L1 = [1]
    // let L2 = ['A']
    // let L3 = struct(L1.Head, L2.Head)
    // printf "L3: %A \n" L3



    let z1 = zip [] []
    if z1 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let z2 = zip [1] [1]
    if z2 = [(1,1)] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let z3 = zip [1; 2; 40] [3; 56; 6]
    if z3 = [(1, 3); (2, 56); (40, 6)] then
        printfn "Passed!"
    else
        printfn "Failed!"     

    let z4 = zip [1; 2; 3] ['a'; 'b'; 'c']
    if z4 = [(1, 'a'); (2, 'b'); (3, 'c')] then
        printfn "Passed!"
    else
        printfn "Failed!"     

    printfn ""
    0 // return an integer exit code
    

