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

[<EntryPoint>]
let main argv =
    printf "Введите количество элементов: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d элементов:" n
    let lst = readList n
    printfn "\nСписок:"
    writeList lst
    0