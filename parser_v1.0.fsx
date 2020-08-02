open System

type Result<'a> =                                                                                       // --- Type for Success/Failure --- //
    | Success of 'a
    | Failure of string
                                                                                                        // --- Type for parser definition --- //                         
type Parser<'a> = Parser of (char list -> Result<'a * char list>)                                       

let parserRun parser input =                                                                            // --- Run a parser with input -- //                    
    let (Parser parserFunc) = parser
    parserFunc input


                                                                                                        // ---  Parse a single character --- //                 
let expectChar expectedChar = 
    let innerParser input =
        match input with
        | (head :: tail) when head = expectedChar -> Success (head, tail)
        | [] -> let empty_msg = (sprintf "Expected %c, but no input" expectedChar)
                Failure empty_msg
        | _ -> let msg = (sprintf "Expected %c, got %c" expectedChar input.Head)
               Failure msg
    Parser innerParser


                                                                                                        // --- And combinator --- //
let parseAnd p1 p2 =                                                                                    // i = char, r = remaining char
    let innerParser input =                                                                             // nested inner function
        let x = parserRun p1 input                                                                      // run input chars of parser 1
        match x with                                                                                    // match with Success or Failure 
        | Failure msg -> Failure msg                                                                    // failure, return failure msg
        | Success (i1, r1) ->                                                                           // success, return char[0] and remaining char[1..]
            let y = parserRun p2 r1                                                                     // run remaining char[1..] as parser 2 input
            match y with                                                                                // match the remaining with success/failure case
            | Failure msg -> Failure msg                                                                // failure, return failure msg
            | Success (i2, r2) -> Success ((i1, i2), r2)                                                // success, return x & y char and remaining characters
    Parser innerParser                                                                                  // returning the inner function
let ( .>>. ) = parseAnd                                                                                 // infix of parseAnd
                   
let parseOr p1 p2 =                                                                                     // --- Or combinator --- //
    let innerParser input =                                                                             // nested inner function
        let a = parserRun p1 input                                                                      // take parser1 as input
        match a with                                                                                    // match with Success or Failure
        | Success result -> Success result                                                              // if success case, return result
        | Failure _ ->                                                                                  // if failure case
            let b = parserRun p2 input                                                                  // take parser2 as input
            b                                                                                           // return b
    Parser innerParser                                                                                  // returning the inner function
let ( <|> ) = parseOr                                                                                   // infix of parseOr
                 


let run str =  List.ofSeq str                                                                                                                                                             

                                                                                                        // ---  Testing --- //
run "Hello"                                                                                             // expected character, print output                                                             
    |> parserRun (expectChar 'H') |> printfn "%A"        
