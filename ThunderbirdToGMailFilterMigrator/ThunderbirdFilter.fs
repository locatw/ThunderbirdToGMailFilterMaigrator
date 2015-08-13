module ThunderbirdFilter

open System.IO
open FParsec

type Header = {
    RootFolderUri : string;
    Version : string;
    Logging : bool
}

type Action =
| MoveToFolder
| Delete

type ConditionType =
| All
| Any

type MatchTarget =
| Subject
| From

type MatcherType =
| Is
| EndsWith

type Matcher =
    {
        Target : MatchTarget;
        Type : MatcherType;
        Pattern : string
    }
    override m.ToString() = sprintf "%A" m

type ConditionSet =
    {
        Type : ConditionType;
        Matchers : Matcher list
    }
    override m.ToString() = sprintf "%A" m

type Filter =
    {
        Name : string;
        Enabled : bool;
        Type : int;
        Action : Action;
        ActionValue : string;
        Condition : ConditionSet
    }
    override m.ToString() = sprintf "%A" m

type FilterSetting = {
    Header : Header
    Filters : Filter list
}

module internal Parser =
    open FParsec.CharParsers

    let private parseNormalChar : Parser<char, unit> = satisfy (fun c -> c <> '\\' && c <> '"')

    let private parseEnclosedString (quotation : string) =
        pstring quotation >>. (manyChars parseNormalChar) .>> pstring quotation

    let parseDoubleQuotedString = parseEnclosedString "\""

    //#region condition parsers
    let parseMatchTarget : Parser<MatchTarget, unit> =
        (stringCIReturn "from" From) <|> (stringCIReturn "subject" Subject)
        
    let parseMatcherType : Parser<MatcherType, unit> =
        (stringCIReturn "is" Is) <|> (stringCIReturn "ends with" EndsWith)
        
    let parseMatcher : Parser<Matcher, unit> =
        let parsePattern =
            manyChars (satisfy (fun c -> c <> ')'))
        let parseInner =
            pipe5 parseMatchTarget (pstring ",") parseMatcherType (pstring ",") parsePattern
                (fun item _ op _ pattern -> { Target = item; Type = op; Pattern = pattern })

        pipe3 (pstring "(") parseInner (pstring ")") (fun _ matcher _ -> matcher)

    let parseConditionType : Parser<ConditionType, unit> =
        (stringCIReturn "or" Any) <|> (stringCIReturn "and" All)

    let parseConditionSet =
        let conditionListToConditionSet (typeAndConditionList : (ConditionType * Matcher) list) =
            let checkAllConditionTypeIsSame =
                typeAndConditionList
                |> List.map (fun (conditionType, _) -> conditionType)
                |> List.distinct
                |> List.length = 1

            match checkAllConditionTypeIsSame with
            | true -> ()
            | false -> failwith "invalid condition value"

            let conditionType =
                typeAndConditionList
                |> List.map (fun (conditionType, _) -> conditionType)
                |> List.distinct
                |> List.head
            let conditionList =
                typeAndConditionList
                |> List.map (fun (_, condition) -> condition)

            { Type = conditionType; Matchers = conditionList }
            
        let parseConditionList =
            many (pipe4 parseConditionType spaces parseMatcher spaces
                        (fun conditionType _ matcher _ -> (conditionType, matcher)))

        parseConditionList |>> (fun conditionList -> conditionListToConditionSet conditionList)
    //#endregion

    //#region filter parsers
    let private parseFilterLine (key : string) parseValue =
        pipe5 (pstring key) spaces (pstring "=") spaces (pstring "\"" >>. parseValue .>> (pstring "\"") .>> (opt (anyOf "\r\n")))
              (fun _ _ _ _ value -> value)

    let parseBool=
        (stringCIReturn "yes" true) <|> (stringCIReturn "no" false)

    let parseNameLine : Parser<string, unit> = parseFilterLine "name" (manyChars parseNormalChar)

    let parseEnabledLine : Parser<bool, unit> = parseFilterLine "enabled" parseBool

    let parseTypeLine : Parser<int, unit> = parseFilterLine "type" pint32

    let parseAction =
        (stringCIReturn "Move to folder" MoveToFolder) <|> (stringCIReturn "Delete" Delete)

    let parseActionLine : Parser<Action, unit> = parseFilterLine "action" parseAction

    let parseActionValueLine : Parser<string, unit> = parseFilterLine "actionValue" (manyChars parseNormalChar)

    let parseConditionLine : Parser<ConditionSet, unit> = parseFilterLine "condition" parseConditionSet

    let parseFilter : Parser<Filter, unit> =
        parse {
            let! name = parseNameLine
            let! enabled = parseEnabledLine
            let! typeValue = parseTypeLine
            let! action = parseActionLine
            let! actionValue = opt parseActionValueLine
            let! condition = parseConditionLine
            return {
                Name = name;
                Enabled = enabled;
                Type = typeValue;
                Action = action;
                ActionValue = match actionValue with
                              | Some(x) -> x
                              | None -> ""
                Condition = condition
            }
        }

    let parseFilters : Parser<Filter list, unit> =
        many parseFilter

    //#endregion

let private loadHeader (lines : string list) =
    let testValue (key : string) (line : string) =
        let elems = line.Split([|'='|])
        if elems.[0] = key then Some(elems.[1]) else None

    { RootFolderUri =
        match (List.tryPick (testValue "RootFolderUri") lines) with
        | Some x -> x
        | None -> "";
      Version =
        match (List.tryPick (testValue "Version") lines) with
        | Some x -> x
        | None -> "";
      Logging =
        match (List.tryPick (testValue "logging") lines) with
        | Some x when x = "yes" -> true
        | Some x when x = "no" -> false
        | _ -> false; }
    
let private loadFilters (lines : string list) =
    let parseResult = String.concat "" lines
                      |> run Parser.parseFilters
    match parseResult with
    | Success(value, _, _) -> value
    | Failure(errorMessage, _, _) -> failwith errorMessage

let load (filePath:string) =
    let allLines = (File.ReadAllLines filePath) |> Array.toList

    { Header = loadHeader (allLines |> List.take(4));
      Filters = loadFilters (allLines |> List.skip(4)) }