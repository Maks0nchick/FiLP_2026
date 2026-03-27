open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Convert.ToInt32(Console.ReadLine())
        let tail = readList (n - 1)
        head :: tail

let writeList list =
    List.iter (printfn "%d") list

let mostFrequent list =
    list
    |> List.countBy id
    |> List.maxBy snd
    |> fst

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