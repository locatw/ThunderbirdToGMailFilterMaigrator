module ThunderbirdFilter.Parser.Test

open NUnit.Framework
open FsUnit
open FParsec
open ThunderbirdFilter
open ThunderbirdFilter.Parser

let parsedValue (x : ParserResult<'a, unit>) =
    match x with
    | Success(value, _, _) -> value
    | Failure(errorMessage, _, _) -> failwith errorMessage

[<Test>]
let parseDoubleQuatedString () =
    run parseDoubleQuotedString "\"value\"" |> parsedValue
    |> should equal "value"

[<TestCase("from")>]
[<TestCase("From")>]
[<TestCase("FROM")>]
let parseMatchTargetFrom(input : string) =
    run parseMatchTarget input |> parsedValue
    |> should equal From

[<TestCase("subject")>]
[<TestCase("Subject")>]
[<TestCase("SUBJECT")>]
let parseMatchTargetSubject(input : string) =
    run parseMatchTarget input |> parsedValue
    |> should equal Subject

[<TestCase("is")>]
[<TestCase("Is")>]
[<TestCase("IS")>]
let parseMatcherTypeIs (input : string) =
    run parseMatcherType input |> parsedValue
    |> should equal Is

[<TestCase("ends with")>]
[<TestCase("Ends with")>]
[<TestCase("ENDS WITH")>]
let parseMatcherTypeEndsWith (input : string) =
    run parseMatcherType input |> parsedValue
    |> should equal EndsWith

[<TestCase("(from,is,*@sample.com)")>]
let testParseMatcher (input : string) =
    run parseMatcher input |> parsedValue
    |> should equal { Target = From; Type = Is; Pattern = "*@sample.com" }

[<TestCase("or")>]
[<TestCase("Or")>]
[<TestCase("OR")>]
let parseConditionTypeOr (input: string) =
    run parseConditionType input |> parsedValue
    |> should equal Any

[<TestCase("and")>]
[<TestCase("And")>]
[<TestCase("AND")>]
let parseConditionTypeAnd (input: string) =
    run parseConditionType input |> parsedValue
    |> should equal All

[<TestCase("OR (from,is,a@sample.com)")>]
let parseOneCondition (input : string) =
    run parseConditionSet input |> parsedValue
    |> should equal
        { Type = Any; Matchers = [{ Target = From; Type = Is; Pattern = "a@sample.com" }]}

[<TestCase("OR (from,is,a@sample.com) OR (subject,ends with,b@sample.com)")>]
let parseManyCondition (input : string) =
    run parseConditionSet input |> parsedValue
    |> should equal
        { Type = Any; Matchers = [
                        { Target = From; Type = Is; Pattern = "a@sample.com" };
                        { Target = Subject; Type = EndsWith; Pattern = "b@sample.com" }]}

[<TestCase("type=\"1\"", 1)>]
[<TestCase("type=\"2\"", 2)>]
let parseType (input : string) (expected : int) =
    run parseTypeLine input |> parsedValue
    |> should equal expected

[<TestCase("action=\"Move to folder\"")>]
let parseAction (input : string) =
    run parseActionLine input |> parsedValue
    |> should equal MoveToFolder

[<TestCase("""name="Filter1"
enabled="yes"
type="17"
action="Move to folder"
actionValue="a folder"
condition="AND (from,ends with,@sample.com)"
""")>]
let parseOneFilter (input : string) =
    run parseFilter input |> parsedValue
    |> should equal
        {
            Name = "Filter1";
            Enabled = true;
            Type = 17;
            Action = MoveToFolder;
            ActionValue = "a folder";
            Condition =
                {
                    Type = All;
                    Matchers = [{ Target = From; Type = EndsWith; Pattern = "@sample.com"}]
                }
        }

[<TestCase("""name="Filter1"
enabled="yes"
type="17"
action="Delete"
condition="AND (from,ends with,@sample.com)"
""")>]
let parseOneFilterWithoutActionValue (input : string) =
    run parseFilter input |> parsedValue
    |> should equal
        {
            Name = "Filter1";
            Enabled = true;
            Type = 17;
            Action = Delete;
            ActionValue = "";
            Condition =
                {
                    Type = All;
                    Matchers = [{ Target = From; Type = EndsWith; Pattern = "@sample.com"}]
                }
        }
