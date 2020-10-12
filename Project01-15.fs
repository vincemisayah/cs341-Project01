module Project01_15

//
// slice L start stop
//
// Returns a slice of the list with the specified starting and ending indices (start inclusive and end non-inclusive)
// This function creates a list containing the copied values from the input list between the starting and ending index
//
// Examples:
//          slice [1; 2; 3; 4; 5] 0 0 => []
//          slice [1; 2; 3; 4; 5] 0 1 => [1]
//          slice [1; 2; 3; 4; 5] 1 4 => [2; 3; 4]
//          slice [1; 2; 3; 4; 5] 0 5 => [1; 2; 3; 4; 5]
//          slice [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 6 2 => []
//

// let slice L start stop =
//     []     //   TO BE IMPLEMENTED

// let rec slice L start stop =
//     match L, start, stop with
//     | [], _start, _stop when (_start = _stop) || (_start = _stop-1) -> printf "start: %A" _start; printf "stop: %A" _stop;[]
//     | list, _start, _stop when (_start = _stop) || (_start = _stop-1) -> printf "start: %A" _start; printf "stop: %A" _stop;[]
//     | [x], _start, _stop when _start  - _stop = 1 -> printf "start: %A" _start; printf "stop: %A" _stop; [x]
//     | (hd::tail), x, y -> hd::slice tail (start+1) stop
//     | _ -> []

// let rec slice L start stop =
//     match L, start, stop with
//     | e::rest, x, y when start = stop -> []
//     | hd::tail, x, y when x < y -> hd::slice (tail) (start+1) stop

let slice L start stop =
    let rec _slice L curr start stop =
        match L, curr, start, stop with
        | list, _curr, _start, _stop when _start > _stop -> []
        | list      , _curr, _start, _stop  when (_start=0 && _stop =0) -> []
        |  e::rest, _curr, _start, _stop    when _curr < _start -> _slice rest (_curr+1) start stop
        | list, _curr, _start, _stop when _curr = _stop -> []
        | hd::tail, _curr, _start, _stop    when _curr = _start       -> hd::_slice (tail) (_curr+1) (_curr+1) stop
        | _ -> []
    _slice L 0 start stop

[<EntryPoint>]
let main argv =
    printfn "Testing Project 15: slice"


// TODO 1
    let t1 = slice [1; 2; 3; 4; 5] 0 0
    printf "t1: %A\n" t1
    if t1 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"

// TODO 2        
    let t2 = slice [1; 2; 3; 4; 5] 0 1
    printf "\n t2: %A\n" t2
    if t2 = [1] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
// TODO 3          
    let t3 = slice [1; 2; 3; 4; 5] 1 4
    printf "\n t3: %A\n" t3
    if t3 = [2; 3; 4] then
        printfn "Passed!"
    else
        printfn "Failed!"     
        
// TODO 4         
    let t4 = slice [1; 2; 3; 4; 5] 0 5
    printf "t4: %A\n" t4
    if t4 = [1; 2; 3; 4; 5] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
// TODO 5          
    let t5 = slice [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] 6 2
    if t5 = [] then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    printfn ""
    0 // return an integer exit code
    

