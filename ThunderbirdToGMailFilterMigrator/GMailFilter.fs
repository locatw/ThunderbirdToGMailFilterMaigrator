module GMailFilter

open System.Collections.Generic
open System.Xml.Linq

type Author = {
    Name : string;
    Email : string
}

type CategoryTerm =
| Filter
with
    override self.ToString() =
        match self with
        | Filter -> "filter"

type EntryPropertyKey = 
| From
| Label
| SizeOperator
| SizeUnit
with
    override self.ToString() =
        match self with
        | From -> "from"
        | Label -> "label"
        | SizeOperator -> "sizeOperator"
        | SizeUnit -> "sizeUnit"

type Entry = {
    CategoryTerm : CategoryTerm;
    Title : string;
    Id : string;
    Updated : System.DateTime;
    Content : string;
    Properties : Map<EntryPropertyKey, string>;
}

type Feed = {
    Title : string;
    Id : string;
    Updated : System.DateTime;
    Author : Author
    Entries : Entry list;
}

let private xnamespace ns = XNamespace.op_Implicit ns

let private xname name = XName.Get name

let private toDateTimeString (dateTime : System.DateTime) =
    dateTime.ToString("yyyy-MM-ddT")

let private atomNs = xnamespace "http://www.w3.org/2005/Atom"
let private appsNs = xnamespace "http://schemas.google.com/apps/2006"

let private entryToXml (entry : Entry) =
    let propertyToXml (property : KeyValuePair<EntryPropertyKey, string>) =
        XElement(appsNs + "property",
            XAttribute(xname "name", property.Key),
            XAttribute(xname "value", property.Value))

    XElement(atomNs + "entry",
        XElement(atomNs + "category",
            XAttribute(xname "term", entry.CategoryTerm)),
        XElement(atomNs + "title", entry.Title),
        XElement(atomNs + "id", entry.Id),
        XElement(atomNs + "updated", entry.Updated.ToUniversalTime().ToString("o")),
        XElement(atomNs + "content", entry.Content),
        query { for property in entry.Properties do select (propertyToXml property) })

let private authorToXml (author : Author) =
    XElement(atomNs + "author",
        XElement(atomNs + "name", author.Name),
        XElement(atomNs + "email", author.Email))

let private feedToXml (feed : Feed) =
    XElement(atomNs + "feed",
        XAttribute(XNamespace.Xmlns + "apps", appsNs),
        XElement(atomNs + "title", feed.Title),
        XElement(atomNs + "id", feed.Id),
        XElement(atomNs + "updated", feed.Updated.ToUniversalTime().ToString("o")),
        authorToXml feed.Author,
        query { for entry in feed.Entries do select (entryToXml entry) })

let writeToFile (filePath : string) (feed : Feed) =
    let feedElem = feedToXml feed
    feedElem.Save(filePath)
