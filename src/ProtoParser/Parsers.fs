namespace ProtoParser

open FParsec

module Parsers =
    let pletter<'a> : Parser<_, 'a> =
        anyOf ['a'..'z'] <|> anyOf ['A'..'Z']
    
    let pdecimalDigit<'a> : Parser<_, 'a> =
        anyOf ['0'..'9']

    let poctalDigit<'a> : Parser<_, 'a> =
        anyOf ['0'..'7']
    
    let phexDigit<'a> : Parser<_, 'a> =
        anyOf ['0'..'9'] <|> anyOf ['A'..'F'] <|> anyOf ['a'..'f']

    let pident<'a> : Parser<_, 'a> =
        pletter .>>. (many (pletter <|> pdecimalDigit <|> pchar '_')) |>> (fun (a, b) -> [a] |> List.append b |> string |> Ident)

    let pfullIdent<'a> : Parser<_, 'a> =
        let toNameSpace l =
            let rec make l a =
                match l with
                | [] -> a
                | [_] -> a
                | h :: r -> make r (h :: a)
            make l [] |> List.rev
        let rec toName l =
            match l with
            | [] -> invalidOp "No ident"
            | [ a ] -> a
            | _ :: r -> toName r
        let toIdent l = toNameSpace l, toName l
        pident .>>. (many pident) |>> (fun (a, b) -> [a] |> List.append b |> toIdent |> FullIdent)
    let pmessageName<'a> : Parser<_, 'a> = pident
    let penumName<'a> : Parser<_, 'a> = pident
    let pfieldName<'a> : Parser<_, 'a> = pident
    let poneOfName<'a> : Parser<_, 'a> = pident
    let pmapName<'a> : Parser<_, 'a> = pident
    let pserviceName<'a> : Parser<_, 'a> = pident
    let prpcName<'a> : Parser<_, 'a> = pident
    let pmessageType<'a> : Parser<_, 'a> = 
        opt (pstring ".") .>>. pfullIdent |>> (fun (pt, id) -> (Option.isSome pt, id) |> MessageType) 
    let penumType<'a> : Parser<_, 'a> = 
        opt (pstring ".") .>>. pfullIdent |>> (fun (pt, id) -> (Option.isSome pt, id) |> EnumType) 
    
    let private chTo10L =
            [
                '0', 0L; '1', 1L; '2', 2L; '3', 3L; '4', 4L; '5', 5L; '6', 6L; 
                '7', 7L; '8', 8L; '9', 9L;
            ] 
    let private chTo10 = chTo10L |> Map.ofList
             

    let private chTo16 = 
        chTo10L 
        |> List.append [ 'a', 10L; 'b', 11L; 'c', 12L; 'd', 13L; 'e', 14L; 'f', 15L ]
        |> List.append [ 'A', 10L; 'B', 11L; 'C', 12L; 'D', 13L; 'E', 14L; 'F', 15L ]
        |> Map.ofList
        
    
    let phexLiteral<'a> : Parser<_, 'a> =
        let rec toLong a l =
            match l with
            | [] -> a
            | [ch] -> a * 16L + chTo16.[ch] 
            | h :: tr -> toLong (a * 16L + chTo16.[h]) tr
        ((pstring "0x") <|> (pstring "0X")) >>. (many1 phexDigit) |>> toLong 0L

    let poctalLiteral<'a> : Parser<_, 'a> =
        let rec toLong a l =
            match l with
            | [] -> a
            | [ch] -> a * 8L + chTo16.[ch] 
            | h :: tr -> toLong (a * 8L + chTo16.[h]) tr
        pstring "0" >>. (many1 poctalDigit) |>> toLong 0L

    let pdecimalLiteral<'a> : Parser<_, 'a> =
        let rec toLong a l =
            match l with
            | [] -> a
            | [ch] -> a * 10L + chTo16.[ch] 
            | h :: tr -> toLong (a * 10L + chTo16.[h]) tr
        anyOf ['1'..'9'] .>>. (many1 pdecimalDigit)  |>> (fun (a, b) -> List.append [a] b |> toLong 0L)
    
    let pintLiteral<'a> : Parser<_, 'a> =
        phexLiteral <|> poctalLiteral <|> pdecimalLiteral
    
    let pfloatLit<'a> : Parser<_, 'a> = pfloat 
                   
    let pboolLit<'a> : Parser<_, 'a> =
        (pstring "true" |>> (fun _ -> true)) <|>(pstring "false" |>> (fun _ -> false))
