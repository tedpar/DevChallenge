module XmlFramer
    open System

    let validateXmlString (input:String) =
        let isWhiteSpace x = x = ' ' || x = char (0x09) || x = char (0x0a) || x = char(0x0d)

        let notEndOfTag () =
            let isInComment = ref false
            fun c -> 
                isInComment := if c = char("\"") then not !isInComment else !isInComment
                if !isInComment then true else c <> '>' && c <> '/'

        let (|StartTag|SelfClosingTag|EndTag|WhiteSpace|InvalidTag|) input =
            let rec loop remaining =
                match remaining with
                | [] -> WhiteSpace
                | head::tail when head |> isWhiteSpace -> loop tail
                | '<'::'/'::tail -> 
                    let endTail = tail |> Seq.skipWhile (notEndOfTag()) |> Seq.toList
                    match endTail with
                    | '>'::tail -> EndTag(tail)
                    | _ -> InvalidTag
                | '<'::tail -> 
                    let endTail = tail |> Seq.skipWhile (notEndOfTag()) |> Seq.toList
                    match endTail with
                    | '/'::'>'::tail -> SelfClosingTag(tail)
                    | '>'::tail -> StartTag(tail)
                    | _ -> InvalidTag
                | _ -> InvalidTag
            loop input

        let rec loop (remaining:char list) depth maxDepth =
            match remaining with
            | _ when depth = 0 && maxDepth > 0 -> 
                let index = (input.Length - remaining.Length)
                (Some(input.Substring(0, index)), input.Substring(index))
            | WhiteSpace -> loop [] depth maxDepth
            | StartTag(x) -> loop x (depth+1) (if depth+1 > maxDepth then depth+1 else maxDepth)
            | SelfClosingTag(x) -> loop x (depth) (if depth+1 > maxDepth then depth+1 else maxDepth)
            | EndTag(x) -> loop x (depth-1) (maxDepth)
            | InvalidTag -> failwith "Noooo!!"
            | _ -> (None, remaining.ToString())

        loop (input |> Seq.toList) 0 0
    

    let extractMessageString (data:ArraySegment<Byte>) =
        let dataString = System.Text.Encoding.UTF8.GetString(data.Array, data.Offset, data.Count)
        try
            let (validatedPart, remainder) = validateXmlString dataString
            match validatedPart with
            | Some(x) -> Some (x.Trim(), System.Text.Encoding.UTF8.GetBytes(x).Length)
            | _ -> None
        with
        | x -> None
