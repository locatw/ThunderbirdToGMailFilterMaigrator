module IMapUtf7

open System
open System.Text

type internal GroupType =
| Printable
| ModifiedBase64
    override self.ToString() =
        match self with
        | Printable -> "Printable"
        | ModifiedBase64 -> "Modified Base64"

type internal Group = 
    {
        Type : GroupType;
        Bytes : byte list;
    }
    override this.ToString() =
        sprintf "{Type = %s; Bytes = %A}" (this.Type.ToString()) this.Bytes

/// <summary>
/// Devide ASCII string into printable and modified Base64 string groups.
/// </summary>
/// <param name="input"></param>
let internal devideToPrintableAndUnprintable (input : string) =
    let isAmpersand (b : byte) = b = 0x26uy

    let isHyphen (b : byte) = b = 0x2duy

    let rec devide (bytes : byte list) =
        match bytes with
        | [] -> []
        // when start with "&-", take it as printable
        | first::second::tail when isAmpersand first && isHyphen second ->
            let newGroup = { Type = Printable; Bytes = [first; second] }
            newGroup::(devide tail)
        // when start with "&", take modified Base64 bytes group
        | head::tail when isAmpersand head ->
            let hyphenIndex = List.tryFindIndex isHyphen tail
            match hyphenIndex with
            | Some(index) ->
                let left, right = List.splitAt index tail
                let newGroup = { Type = ModifiedBase64; Bytes = left }
                // skip '-' from right
                newGroup::(devide (List.skip 1 right))
            | None -> failwith "invalid format"
        // when start with printable byte, take printable bytes group
        | _ ->
            let ampersandIndex = List.tryFindIndex isAmpersand bytes
            match ampersandIndex with
            | Some(index) ->
                let left, right = List.splitAt index bytes
                let newGroup = { Type = Printable; Bytes = left }
                newGroup::(devide right)
            | None ->
                [{ Type = Printable; Bytes = bytes }]

    let rec combineContinuousPrintableGroup (groups : Group list) =
        match groups with
        | head::second::tail when head.Type = Printable && second.Type = Printable ->
            let combinedBytes = List.append head.Bytes second.Bytes
            let combinedGroup = { Type = Printable; Bytes = combinedBytes }
            combineContinuousPrintableGroup (combinedGroup::tail)
        | head::tail ->
            head::(combineContinuousPrintableGroup tail)
        | _ ->
            groups

    Encoding.ASCII.GetBytes(input)
    |> Array.toList
    |> devide
    |> combineContinuousPrintableGroup 

let internal decodeIMapUtf7 (input : string) =
    let replaceCommaToSlash (s : string) =
        s.Replace(',', '/')

    let padWith (c : char) (s : string) =
        let replicate letter (times : int) =
            new String(letter, times)

        match s.Length % 4 with
        | 0 -> s
        | n when 1 <= n && n <= 3 -> s + (replicate c (4 - n))
        | _ -> failwith "never occurs"

    let decodeBase64 (s : string) =
        Encoding.BigEndianUnicode.GetString(Convert.FromBase64String(s))

    input
    |> replaceCommaToSlash
    |> padWith '='
    |> decodeBase64

let private decodeModifiedBase64Group (group : Group) =
    let bytes = List.toArray group.Bytes
    match group.Type with
    | Printable -> Encoding.ASCII.GetString(bytes)
    | ModifiedBase64 -> decodeIMapUtf7 (Encoding.ASCII.GetString(bytes))

/// <summary>
/// Decode IMAP-UTF7 string.
/// </summary>
/// <params name="input">Ascii string</params>
/// <returns>decoded UTF-16 string</returns>
let decode (input : string) =
    let replaceAmpersandHyphenToAmpersand (s: string) =
        s.Replace("&-", "&")

    input
    |> devideToPrintableAndUnprintable
    |> List.map decodeModifiedBase64Group
    |> String.concat ""
    |> replaceAmpersandHyphenToAmpersand