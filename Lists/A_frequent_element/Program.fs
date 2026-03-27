open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Convert.ToInt32(Console.ReadLine())
        let tail = readList (n - 1)
        head :: tail

let writeList list =
    let rec loop lst =
        match lst with
        | [] -> ()
        | h :: t ->
            printfn "%d" h
            loop t
    loop list

let mostFrequent list =
    let rec countElement lst elem acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let newAcc = if h = elem then acc + 1 else acc
            countElement t elem newAcc
    
    let rec findMostFrequent lst bestElem bestCount =
        match lst with
        | [] -> bestElem
        | h :: t ->
            let cnt = countElement list h 0
            let (newElem, newCount) = if cnt > bestCount then (h, cnt) else (bestElem, bestCount)
            findMostFrequent t newElem newCount
    
    match list with
    | [] -> failwith "Список пуст"
    | h :: t -> findMostFrequent t h 1

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов:" n
    let lst = readList n
    
    printfn "\nИсходный список:"
    writeList lst
    
    let result = mostFrequent lst
    printfn "\nСамый частый элемент: %d" result
    
    0