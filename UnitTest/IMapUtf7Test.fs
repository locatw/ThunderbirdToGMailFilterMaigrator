module IMapUtf7.Test

open NUnit.Framework
open FsUnit
open IMapUtf7

type System.String
with
    member this.toByteList =
        Array.toList (System.Text.Encoding.ASCII.GetBytes(this))

[<TestFixture>]
type ``devideToPrintableAndUnprintable tests`` () =
    [<Test>]
    static member ``devide string contains only printable letters to one group`` () =
        "ab"
        |> devideToPrintableAndUnprintable
        |> should equal [{ Type = Printable; Bytes = [(byte)'a'; (byte)'b'] }]

    [<Test>]
    static member ``devide string contains only unprintable letters to one group`` () =
        "&ab-"
        |> devideToPrintableAndUnprintable
        |> should equal [{ Type = ModifiedBase64; Bytes = [(byte)'a'; (byte)'b'] }]

    [<Test>]
    static member ``&- treats as printable bytes`` () =
        "a&-b"
        |> devideToPrintableAndUnprintable
        |> should equal [{ Type = Printable; Bytes = [(byte)'a'; (byte)'&'; (byte)'-'; (byte)'b'] }]

    [<Test>]
    static member ``devide to printable and modified Base64`` () =
        "a&b-c"
        |> devideToPrintableAndUnprintable
        |> should equal [
            { Type = Printable; Bytes = [(byte)'a'] };
            { Type = ModifiedBase64; Bytes = [(byte)'b'] };
            { Type = Printable; Bytes = [(byte)'c'] }
        ]

    [<Test>]
    static member ``devide to printable and two modified Base64`` () =
        "a&b-c&d-e"
        |> devideToPrintableAndUnprintable
        |> should equal [
            { Type = Printable; Bytes = [(byte)'a'] };
            { Type = ModifiedBase64; Bytes = [(byte)'b'] };
            { Type = Printable; Bytes = [(byte)'c'] };
            { Type = ModifiedBase64; Bytes = [(byte)'d'] };
            { Type = Printable; Bytes = [(byte)'e'] };
        ]

    [<Test>]
    static member ``devide when modified Base64 at first`` () =
        "&a-b"
        |> devideToPrintableAndUnprintable
        |> should equal [
            { Type = ModifiedBase64; Bytes = [(byte)'a'] };
            { Type = Printable; Bytes = [(byte)'b'] }
        ]

    [<Test>]
    static member ``devide when modified Base64 at last`` () =
        "a&b-"
        |> devideToPrintableAndUnprintable
        |> should equal [
            { Type = Printable; Bytes = [(byte)'a'] };
            { Type = ModifiedBase64; Bytes = [(byte)'b'] };
        ]

[<TestFixture>]
type ``tests for decodeModifiedUtf7 function``() =
    [<TestCase("ZeVnLIqe", "日本語")>]
    static member ``decode simple Modeified UTF7 string``(input : string, expected : string) =
        input
        |> decodeIMapUtf7
        |> should equal expected

    // "ＡＡＡ" is not need '=' and contains '/' when encode to Base64,
    // so it can test decoding string contains comma purely.
    [<TestCase(",yH,If8h", "ＡＡＡ")>]
    static member ``decode Modified UTF7 string contains comma``(input : string, expected : string) =
        input
        |> decodeIMapUtf7
        |> should equal expected

    // When "ab" is encoded in Base64, it becomes "AGEAYg==".
    // But Modified UTF7 does not require padding by '='.
    [<TestCase("AGEAYg", "ab")>]
    static member ``decode Modified UTF7 string that length is not multiple of 4``(input : string, expected : string) =
        input
        |> decodeIMapUtf7
        |> should equal expected

[<TestFixture>]
type ``decode tests`` () =
    [<TestCase("abc", "abc")>]
    static member ``decode modified UTF7 contains no modified Base64`` (input : string) (expected : string) =
        input
        |> decode
        |> should equal expected

    [<TestCase("a&-b", "a&b")>]
    static member ``decode modified UTF7 contains no modified Base64 but contains "&-"``
            (input : string) (expected : string) =
        input
        |> decode
        |> should equal expected

    [<TestCase("&,yH,If8h-", "ＡＡＡ")>]
    static member ``decode modified UTF7 contains only modified Base64``
            (input : string) (expected : string) =
        input
        |> decode
        |> should equal expected

    [<TestCase("a&,yH,If8h-b", "aＡＡＡb")>]
    static member ``decode modified UTF7 contains normal string and modified Base64``
            (input : string) (expected : string) =
        input
        |> decode
        |> should equal expected