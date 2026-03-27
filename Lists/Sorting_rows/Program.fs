open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Console.ReadLine()
        let tail = readList (n - 1)
        head :: tail

let writeList list =
    List.iter (printfn "%s") list

let sortByLength list =
    list |> List.sortBy (fun s -> s.Length)

[<EntryPoint>]
let main argv =
    printf "Введите количество строк: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d строк:" n
    let lst = readList n
    
    printfn "\nИсходный список:"
    writeList lst
    
    let sorted = sortByLength lst
    printfn "\nСписок, отсортированный по длине строк:"
    writeList sorted
    
    0