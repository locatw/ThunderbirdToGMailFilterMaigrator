module Migrator.Test

open System.Text.RegularExpressions
open FsUnit
open NUnit.Framework
open Migrator
open ThunderbirdFilter
open GMailFilter

[<TestFixture>]
type ``makeIdGenerator tests`` () =
    [<Test>]
    static member ``first value is 1`` () =
        let generator = makeIdGenerator ()

        generator () |> should equal 1

    [<Test>]
    static member ``call twice, then get 1 and 2`` () =
        let generator = makeIdGenerator ()

        generator () |> should equal 1
        generator () |> should equal 2

    [<Test>]
    static member ``can make new id generator`` () =
        let generator1 = makeIdGenerator ()
        generator1 () |> should equal 1
        generator1 () |> should equal 2

        let generator2 = makeIdGenerator ()
        generator2 () |> should equal 1

[<TestFixture>]
type ``Single filter migration tests`` () =
    
    let filter =
        {
            Name = "Filter1";
            Enabled = true;
            Type = 17;
            Action = MoveToFolder;
            // "MNUwozDrML" represents "フィルタ"
            ActionValue = "imap://sample%40gmail.com@imap.googlemail.com/&MNUwozDrML8-";
            Condition =
                {
                    Type = Any;
                    Matchers =
                        [
                            {
                                Target = ThunderbirdFilter.From;
                                Type = EndsWith;
                                Pattern = "@yahoo.co.jp"
                            }
                        ]
                }
        }

    let filterSetting =
        {
            Header = { RootFolderUri = "";  Version = ""; Logging = false; };
            Filters = [filter];
        }

    let author = { Name = "Author"; Email = "sample@gmail.com"; }

    [<Test>]
    member this.``id has fixed format`` () =
        let feed = migrate filterSetting author
        
        let entry = List.head feed.Entries
        entry.Id |> should equal "tag:mail.google.com,2008:filter:1"

    [<Test>]
    member this.``"from" property's value is pattern string with wildcard`` () =
        let feed = migrate filterSetting author

        let entry = List.head feed.Entries
        entry.Properties |> Map.containsKey From |> should be True
        entry.Properties |> Map.find From |> should equal "*@yahoo.co.jp"

    [<Test>]
    member this.``"label" property's value is decoded filter name`` () =
        let feed = migrate filterSetting author

        let entry = List.head feed.Entries
        entry.Properties |> Map.containsKey Label |> should be True
        entry.Properties |> Map.find Label |> should equal "フィルタ"

    [<Test>]
    member this.``"sizeOperator" property's value is "s_sl"`` () =
        let feed = migrate filterSetting author

        let entry = List.head feed.Entries
        entry.Properties |> Map.containsKey SizeOperator |> should be True
        entry.Properties |> Map.find SizeOperator |> should equal "s_sl"

    [<Test>]
    member this.``"sizeUnit" property's value is "s_smb"`` () =
        let feed = migrate filterSetting author

        let entry = List.head feed.Entries
        entry.Properties |> Map.containsKey SizeUnit |> should be True
        entry.Properties |> Map.find SizeUnit |> should equal "s_smb"

