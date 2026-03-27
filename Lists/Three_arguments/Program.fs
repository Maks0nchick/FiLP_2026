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

let foldFilter f p acc list =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | h :: t ->
            let newAcc = if p h then f acc h else acc
            loop t newAcc
    loop list acc

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов:" n
    let lst = readList n
    
    printfn "\nИсходный список:"
    writeList lst
    
    let sumEven = foldFilter (+) (fun x -> x % 2 = 0) 0 lst
    printfn "\nСумма четных элементов: %d" sumEven
    
    let productOdd = foldFilter (*) (fun x -> x % 2 <> 0) 1 lst
    printfn "Произведение нечетных элементов: %d" productOdd
    
    let countGreaterThan5 = foldFilter (fun acc _ -> acc + 1) (fun x -> x > 5) 0 lst
    printfn "Количество элементов больше 5: %d" countGreaterThan5
    
    0