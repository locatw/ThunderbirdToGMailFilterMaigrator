module Main

let writeToFile filterSetting (filePath : string) =
    use file = System.IO.File.CreateText(filePath)
    fprintfn file "%A" filterSetting

writeToFile
    (ThunderbirdFilter.load @"D:\work\mail_filter\thunderbird_filter.txt")
    @"D:\work\mail_filter\output.txt"
