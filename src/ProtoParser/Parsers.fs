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
    
                

    let private chTo16 = 
        [ '0', 0uy; '1', 1uy; '2', 2uy; '3', 3uy; '4', 4uy; '5', 5uy; '6', 6uy; '7', 7uy; '8', 8uy; '9', 9uy; ]
        |> List.append [ 'a', 10uy; 'b', 11uy; 'c', 12uy; 'd', 13uy; 'e', 14uy; 'f', 15uy ]
        |> List.append [ 'A', 10uy; 'B', 11uy; 'C', 12uy; 'D', 13uy; 'E', 14uy; 'F', 15uy ]
        |> Map.ofList
        
    let chDigitToByte ch = chTo16.[ch]
    let chDigitToInt64 = chDigitToByte >> int64

    let rec private chlToInt64 bs st chl =
        match chl with
        | [] -> st
        | [ch] -> st * bs + (chDigitToInt64 ch)
        | h :: tr -> chlToInt64 bs (st * bs + (chDigitToInt64 h)) tr


    let phexLiteral< 'a>  : Parser<int64, 'a> =
        ((pstring "0x") <|> (pstring "0X")) >>. (many1 phexDigit) |>> chlToInt64 16L 0L

    let poctalLiteral<'a> : Parser<int64, 'a> =
        pstring "0" >>. (many1 poctalDigit) |>> chlToInt64 8L 0L

    let pdecimalLiteral<'a> : Parser<_, 'a> =
        anyOf ['1'..'9'] .>>. (many1 pdecimalDigit)  |>> (fun (a, b) -> List.append [a] b |> chlToInt64 10L 0L)
    
    let pintLiteral<'a> : Parser<_, 'a> =
        phexLiteral <|> poctalLiteral <|> pdecimalLiteral
    
    let pfloatLit<'a> : Parser<_, 'a> = pfloat 
                   
    let pboolLit<'a> : Parser<_, 'a> =
        (pstring "true" |>> (fun _ -> true)) <|>(pstring "false" |>> (fun _ -> false))
    
    let pquote<'a> : Parser<_, 'a> = anyOf ['\''; '"'] 
    let pcharEscape<'a> : Parser<_, 'a> = 
        let pairs = 
            [
                'a', '\a'; 'b', '\b'; 'f', '\f'; 'n', '\n'; 'r', '\r'; 't', '\t'; 'v', '\v'; '\\', '\\'; '\'', '\''; '"', '"'
            ]
        let escaped = 
            pairs |> Map.ofList
        let vals = pairs |> List.map snd
        pchar '\\' >>. anyOf vals |>> (fun p -> Map.find p escaped)
    let poctExcape<'a> : Parser<_, 'a> =
        pchar '\\' >>. (pipe3 poctalDigit poctalDigit poctalDigit (fun a b c -> [a; b; c;] |> chlToInt64 8L 0L |> char))
    let phexEscape<'a> : Parser<_, 'a> =
        ((pstring "0x") <|> (pstring "0X")) >>. (pipe2 phexDigit phexDigit (fun a b -> [a; b; ] |> chlToInt64 16L 0L |> char))
    
    let pcharNoEscape<'a> : Parser<_, 'a> = noneOf [ '\x00' ; '\\'; '\n']

    let pcharValue<'a> : Parser<_, 'a> = phexEscape <|> poctExcape <|> pcharEscape <|> pcharNoEscape
    //let pstrLiteral<'a> : Parser<_, 'a> = () 
        
