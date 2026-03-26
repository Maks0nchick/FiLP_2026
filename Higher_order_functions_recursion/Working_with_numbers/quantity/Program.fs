open System

// Функция для нахождения наибольшего общего делителя (алгоритм Евклида)
let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

    
// Функция для проверки, являются ли числа взаимно простыми
let areCoprime a b = gcd a b = 1

// Функция для проверки, является ли число четным
let isEven x = x % 2 = 0

// Функция для проверки, не является ли число взаимно простым с данным
let isNotCoprimeWith n x = not (areCoprime n x)

// Основная функция: количество четных чисел, не взаимно простых с n
// Используем хвостовую рекурсию и функции высших порядков
let countEvenNotCoprime n =
    let rec loop i acc =
        if i > n then
            acc
        else
            // Проверяем: число четное И не взаимно простое с n
            let newAcc = if isEven i && isNotCoprimeWith n i then acc + 1 else acc
            loop (i + 1) newAcc
    loop 1 0

[<EntryPoint>]
let main argv =
    printfn "Поиск количества четных чисел, не взаимно простых с данным"
    printfn ""
    
    // Тестирование на примерах
    let testNumbers = [6; 8; 10; 12; 15; 20; 30; 36]
    
    printfn "%-10s %-10s %-35s" "Число N" "Кол-во" "Четные числа, не взаимно простые с N"
    printfn "%s" (String.replicate 60 "-")
    
    for n in testNumbers do
        let count = countEvenNotCoprime n
        
        // Получаем список четных чисел, не взаимно простых с n
        let numbers = 
            [1..n] 
            |> List.filter (fun x -> isEven x && isNotCoprimeWith n x)
        
        let listStr = if numbers = [] then "нет" else sprintf "%A" numbers
        printfn "%-10d %-10d %-35s" n count listStr
    
    printfn ""
    
    // Ввод
    printf "Введите число N: "
    let userNumber = int (Console.ReadLine())
    
    let result = countEvenNotCoprime userNumber
    
    let evenNotCoprime = 
        [1..userNumber] 
        |> List.filter (fun x -> isEven x && isNotCoprimeWith userNumber x)
    
    printfn "\nРезультат для числа %d:" userNumber
    printfn "Четные числа, не взаимно простые с %d: %A" userNumber evenNotCoprime
    printfn "Количество: %d" result
    
    0

