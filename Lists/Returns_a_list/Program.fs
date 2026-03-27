open System

let rec readList n = 
    if n = 0 then []
    else
        let head = Convert.ToInt32(Console.ReadLine())
        let tail = readList (n - 1)
        head :: tail

let readChurchList n =
    printfn "Введите %d элементов:" n
    readList n


[<EntryPoint>]
let main argv =
    printfn "Демонстрация работы функции readChurchList"
    printfn ""
    
    printf "Введите количество элементов: "
    let n = Convert.ToInt32(Console.ReadLine())
    
    let result = readChurchList n
    printfn "\nСозданный список Чёрча: %A" result
    
    0