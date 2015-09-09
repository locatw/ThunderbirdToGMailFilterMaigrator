module Migrator

open ThunderbirdFilter
open GMailFilter

let internal makeIdGenerator () =
    let id = ref 0
    fun () ->
        id := !id + 1
        !id

let internal makeProperty (filter : ThunderbirdFilter.Filter) =
    let condition = filter.Condition
    let headMacther = List.head condition.Matchers
    match headMacther.Target with
    | ThunderbirdFilter.From ->
        let fromValue = 
            match headMacther.Type with
            | ThunderbirdFilter.EndsWith -> "*" + headMacther.Pattern
            | ThunderbirdFilter.Is ->
                // when matcher type is "is",
                // change matcher pattern into any pattern from original domain.
                let value = headMacther.Pattern
                let atIndex = value.IndexOf('@')
                "*" + value.Substring(atIndex)
        let labelHead = filter.ActionValue.IndexOf("googlemail.com/")
        let imapUtf7Label = filter.ActionValue.Substring(labelHead + 15)
        let label = IMapUtf7.decode imapUtf7Label
        Map.ofList
            [
                (GMailFilter.EntryPropertyKey.From, fromValue);
                (GMailFilter.EntryPropertyKey.Label, label);
                (GMailFilter.EntryPropertyKey.SizeOperator, "s_sl");
                (GMailFilter.EntryPropertyKey.SizeUnit, "s_smb");
            ]
    | ThunderbirdFilter.Subject -> failwith "not support \"Subject\""

let internal convertFilter idGenerator (filter : ThunderbirdFilter.Filter) =
    let id = idGenerator ()
    let idValue = sprintf "tag:mail.google.com,2008:filter:%d" id
    {
        GMailFilter.CategoryTerm = GMailFilter.Filter;
        GMailFilter.Title = "Mail Filter";
        GMailFilter.Id = idValue;
        GMailFilter.Updated = System.DateTime.Now;
        GMailFilter.Content = "";
        GMailFilter.Properties = makeProperty filter
    }

let migrate (filters : ThunderbirdFilter.FilterSetting) (author : Author) =
    let idGenerator = makeIdGenerator ()

    let entries =
        filters.Filters
        |> List.map (convertFilter idGenerator)

    {
        Title = "Mail Filters";
        Id = "tag:mail.google.com,2008:filters:1,1";
        Updated = System.DateTime.Now;
        Author = author;
        Entries = entries;
    }
