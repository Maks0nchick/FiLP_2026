open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Convert.ToInt32(Console.ReadLine())
        let tail = readList (n - 1)
        head :: tail

let writeList list =
    List.iter (printfn "%d") list

let countSquareElements list =
    let squares = list |> List.map (fun x -> x * x) |> Set.ofList
    list 
    |> List.filter (fun x -> Set.contains x squares)
    |> List.length

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов:" n
    let lst = readList n
    
    printfn "\nИсходный список:"
    writeList lst
    
    let result = countSquareElements lst
    printfn "\nКоличество элементов, являющихся квадратом другого элемента: %d" result
    
    0