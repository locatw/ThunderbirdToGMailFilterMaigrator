module Main

open System

Console.WriteLine("Thunderbird filter file:")
let thunderbirdFilterFile = Console.ReadLine()

Console.WriteLine("Output to:")
let gmailFilterFile = Console.ReadLine()

Console.WriteLine("Author name:")
let authorName = Console.ReadLine()

Console.WriteLine("Author's email address:")
let authorEmail = Console.ReadLine()

try

    let thunderbirdFilter = ThunderbirdFilter.load(thunderbirdFilterFile)
    let feed = Migrator.migrate thunderbirdFilter { Name = authorName; Email = authorEmail; }

    GMailFilter.writeToFile gmailFilterFile feed
with
    | :? System.IO.FileNotFoundException ->
        failwith "Thunderbird filter file not found"
    | :? System.Exception as e ->
        failwith (String.Format("unknown exception occurred: {0}", e.Message))
