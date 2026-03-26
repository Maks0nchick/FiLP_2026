open System

// Функция для проверки, делится ли цифра на 3
let isDivisibleBy3 x = x % 3 = 0

// Функция для обхода цифр числа с хвостовой рекурсией
let maxDigitNotDivisibleBy3 number =
    let rec loop n acc =
        if n = 0 then
            acc
        else
            let digit = n % 10
            // Если цифра не делится на 3 и больше текущего максимума
            let newAcc = 
                if not (isDivisibleBy3 digit) && digit > acc then 
                    digit 
                else 
                    acc
            loop (n / 10) newAcc
    // Начинаем с -1, если не найдем подходящих цифр, вернем -1
    let result = loop (abs number) -1
    if result = -1 then None else Some result

// Функция для получения всех цифр, не делящихся на 3
let getDigitsNotDivisibleBy3 number =
    let rec loop n acc =
        if n = 0 then
            acc
        else
            let digit = n % 10
            let newAcc = if not (isDivisibleBy3 digit) then digit :: acc else acc
            loop (n / 10) newAcc
    loop (abs number) [] |> List.rev

[<EntryPoint>]
let main argv =
    printfn "Поиск максимальной цифры числа, не делящейся на 3"
    printfn ""
    
    // Тестирование на примерах
    let testNumbers = [12345; 333; 139; 579; 123456789; 369; 1245]
    
    printfn "%-15s %-30s %-20s" "Число" "Цифры, не делящиеся на 3" "Максимальная"
    printfn "%s" (String.replicate 70 "-")
    
    for n in testNumbers do
        let digits = getDigitsNotDivisibleBy3 n
        let maxDigit = maxDigitNotDivisibleBy3 n
        
        let digitsStr = if digits = [] then "нет" else sprintf "%A" digits
        let maxStr = 
            match maxDigit with
            | Some d -> sprintf "%d" d
            | None -> "нет"
        
        printfn "%-15d %-30s %-20s" n digitsStr maxStr
    
    printfn ""
    
    // Ввод от пользователя
    printf "Введите число: "
    let userNumber = int (Console.ReadLine())
    
    let digits = getDigitsNotDivisibleBy3 userNumber
    let maxDigit = maxDigitNotDivisibleBy3 userNumber
    
    printfn "\nРезультат для числа %d:" userNumber
    printfn "Цифры, не делящиеся на 3: %A" digits
    
    match maxDigit with
    | Some d -> printfn "Максимальная цифра, не делящаяся на 3: %d" d
    | None -> printfn "Нет цифр, не делящихся на 3"
    
    0