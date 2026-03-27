open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Convert.ToInt32(Console.ReadLine())
        let tail = readList (n - 1)
        head :: tail

let writeList list =
    List.iter (printfn "%d") list

let writeTuples tuples =
    tuples |> List.iter (fun (a, b, c) -> printfn "(%d, %d, %d)" a b c)

let sumDigits x =
    let rec loop n acc =
        if n = 0 then acc
        else loop (n / 10) (acc + n % 10)
    loop (abs x) 0

let countDivisors x =
    let rec loop d acc =
        if d > abs x then acc
        elif abs x % d = 0 then loop (d + 1) (acc + 1)
        else loop (d + 1) acc
    loop 1 0

let createTuples list1 list2 list3 =
    let sorted1 = list1 |> List.sortDescending
    
    let sorted2 = 
        list2 
        |> List.sortBy (fun x -> (sumDigits x, abs x))
    
    let sorted3 = 
        list3 
        |> List.sortByDescending (fun x -> (countDivisors x, abs x))
    
    List.zip3 sorted1 sorted2 sorted3

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов в первом списке: "
    let n1 = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов первого списка:" n1
    let list1 = readList n1
    
    printf "\nВведите количество элементов во втором списке: "
    let n2 = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов второго списка:" n2
    let list2 = readList n2
    
    printf "\nВведите количество элементов в третьем списке: "
    let n3 = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов третьего списка:" n3
    let list3 = readList n3
    
    let result = createTuples list1 list2 list3
    
    printfn "\nРезультат:"
    writeTuples result
    
    0