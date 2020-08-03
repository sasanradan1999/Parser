open System

type Result<'a> =                                                                                       // --- Type for Success/Failure --- //
    | Success of 'a
    | Failure of string
                                                                                                        // --- Type for parser definition --- //                         
type Parser<'a> = Parser of (char list -> Result<'a * char list>)

let parserRun parser input =                                                                            // --- Run a parser with input -- //                    
    let (Parser parserFunc) = parser
    parserFunc input



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
        let x = parserRun p1 input                                                                      // run input chars as parser 1 input
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
            
            

let parserMap funcMap parser =                                                                          // --- Map function --- //
    let innerParser input =                                                                             // nested inner function         
        let originalInput = parserRun parser input                                                      // take parser as input
        match originalInput with                                                                        // match with Success or Failure 
        | Failure msg -> Failure msg                                                                    // failure, return failure msg
        | Success (inputTransform, remaining) ->                                                        // if success
            let newInput = funcMap inputTransform                                                       // apply map function to result to be transformed
            Success (newInput, remaining)                                                               // return new mapped value
    Parser innerParser                                                                                  // returning the inner function
let  ( <!> ) = parserMap                                                                                // infix of parserMap



let parserApply parserFunc parserParam =                                                                // use case of parserMap function 
    (parserFunc .>>. parserParam)                                                                       // results tupled together using .>>. combinator
    |> (<!>) (fun (f, x) -> f x)                                                                        // map f (function) to x (parameter)
let ( <*> ) = parserApply                                                                               // infix of parserApply

let parserReturn result =                                                                               // -- Return function --- //
    let innerParser input =                                                                             // nested inner function 
        Success (result, input)                                                                         // if success, return result, ignore input
    Parser innerParser                                                                                  // returning the inner function  

let parserLift funcToLift parserParam1 parserParam2 =                                                   // -- Lift function --- //
    let lift = parserReturn funcToLift <*> parserParam1  <*> parserParam2                               // take normal function, use parserApply and turn to parser function
    lift                                                                                                // 2 parameter function lift


                                                                                                        // --- Sequence Combinator --- //
let rec parsersSeq parserList =                                                                         // tail recursion 
    let cons head tail = head :: tail                                                                   // prepend head to tail, return new list
    let parserCons = parserLift cons                                                                    // use lift function, lift cons

    match parserList with                                                                               // recursively process the list of parsers
    | [] -> 
        let empty = parserReturn []                                                                     // empty input, return parser with empty list
        empty
    | (parser :: parserRemaining) ->                                                                    // use recursion to process the list of parsers 
        let seq = parserCons parser (parsersSeq parserRemaining)
        seq



let run str =  List.ofSeq str         

let charList chars =  System.String(List.toArray chars) 

let parseString parserString =                                                                          // parsr a string
    parserString                                                                                        
    |> List.ofSeq                                                                                       // string -> list of characters
    |> List.map expectChar                                                                              // map char to parser char
    |> parsersSeq                                                                                       // convert to parser char list
    |> (<!>) charList                                                                                   // char list -> parse string


                                                                                                        // --- Testing --- //
run "hello"                                                                                             // input                                                                  
|> parserRun (parseString "hello" )                                                                     // expected string
|> printfn "%A"                                                                                         // print output
a
