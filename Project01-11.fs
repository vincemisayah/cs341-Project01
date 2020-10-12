module Project01_11

//
// unzip L
//
// Unzip a list of pairs to a pair of lists
//
// Returns tuple of lists
// 
// Examples: 
//          unzip [] => ([], [])
//          unzip [(1, 3); (2, 56); (40, 6)] => ([1; 2; 40], [3; 56; 6])
//          unzip [(1, 'a'); (2, 'b'); (3, 'c')] => ([1; 2; 3], ['a'; 'b'; 'c'])
//
// You may not call List.unzip directly in your solution.
//
// 

// let unzip L =
//     ([],[])     //   TO BE IMPLEMENTED

// let unzip L =
//     let a, b = L.Head   
//     let LA, LB = [a], [b]
//     let S1     = struct(LA, LB)
//     match L with
//     | []        -> ([],[])
//     | _ -> 
//     let rec _unzip L a b LA LB S1 =
//         match L with
//         | hd::rest -> a, b   = hd, rest.Head;
//                       LA, LB = [a], [b];
//                       S1     = struct(LA, LB);
//                       S1::_unzip rest a b LA LB S1
//     _unzip L a b LA LB S1

// let  unzip L =
//     match L with
//     | [] -> ([],[])
//     | _  ->
//     let rec _unzip L L1 L2 =
//         match L with
//         | e::[]    -> 
//                         let e1, e2 = e.Head;
//                         L1@e1;
//                         L2@e2;
//         | hd::tail ->   
//                         let x1, y1 = hd;
//                         let x2, y2 = tail.Head;
//        
          
let rec map F L =
    match L with
    | [] -> []
    | hd::tail -> F hd::map F tail

let rec iter F L =
    match L with
    | [] -> ()
    | hd::tl -> F hd; 
                iter F tl 

// let rec _unzip L soFar =
//     match L with
//     | []    -> []
//     | e::[] -> e::soFar
//     | hd::tail -> hd::tail.Head::_unzip tail (hd@tail.Head) 


                  

let rec unzip L =
    match L with
    | [] -> ([],[])
    | hd::tail -> let (e1, e2) = hd;
                  let S1       = unzip tail;
                  let (t1, t2) = S1;
                  (e1::t1, e2::t2)
    
[<EntryPoint>]
let main argv =
    printfn "Testing Project 11: unzip"

    // let L1 = [(1, 'A'), (2, 'B')]
    // let a, b = L1.Head  // TODO: Step One


    // let x1, y1 = a // TODO: Step One
    // let x2, y2 = b // TODO: Step One
    // // printf "    x1: %A \n" x1
    // // printf "    y1: %A \n" y1
    // // printf "    x2: %A \n" x2
    // // printf "    y2: %A \n" y2

    // let A1, A2 = [x1; x2], [y1;y2] // TODO: Step One
    // // printf "    A1: %A \n" A1
    // // printf "    A2: %A \n" A2
 
    // let S1 = struct(A1, A2) // TODO: Step One
    // printf "    S1: %A \n" S1
//---------------------



//?--------------------------------------------------------------------------

    let u11,u12 = unzip []
    if (u11 = []) && (u12 = []) then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let u21,u22 = unzip [(1, 3); (2, 56); (40, 6)]
    if (u21 = [1; 2; 40]) && (u22 = [3; 56; 6]) then
        printfn "Passed!"
    else
        printfn "Failed!"

    let u31,u32 = unzip [(1, 'a'); (2, 'b'); (3, 'c')]
    if (u31 = [1; 2; 3]) && (u32 = ['a'; 'b'; 'c']) then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    printfn ""
    0 // return an integer exit code
    

