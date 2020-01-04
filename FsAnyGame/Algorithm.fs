module Algorithm

/// Returns true if 'item' does not exist in 'theList', where keySelector is
/// a function that returns a comparison key, given the item.
let NotInList theList keySelector item =
    
    let itemToFindKey = item |> keySelector
    
    theList 
        |> List.tryFind (fun listItem -> 
            let thisItemKey = listItem |> keySelector
            itemToFindKey = thisItemKey) 

        |> Option.isNone


/// If all items in the list satisfy the predicate then
/// just return the original list.  Otherwise, perform
/// a List.filter to reduce the list to only those that
/// satisfy the predicate.
let PlanetSavingListFilter predicate theList =

    if theList |> List.forall predicate then
        theList
    else
        theList |> List.filter predicate

