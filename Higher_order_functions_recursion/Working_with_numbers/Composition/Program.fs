open System

// Функция для нахождения наибольшего общего делителя
let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

// Проверка на взаимную простоту
let areCoprime a b = gcd a b = 1

// Нахождение наименьшего делителя числа (больше 1)
let smallestDivisor n =
    if n <= 1 then 1
    else
        let rec loop d =
            if d * d > n then n
            elif n % d = 0 then d
            else loop (d + 1)
        loop 2

// Функция для нахождения максимального числа от 1 до n,
// которое:
//   1. не взаимно простое с n
//   2. не делится на наименьший делитель n
let maxNotCoprimeNotDivisibleByMinDivisor n =
    let minDiv = smallestDivisor n
    
    let rec loop i acc =
        if i > n then
            acc
        else
            // Проверяем: не взаимно простое И не делится на minDiv
            let newAcc = 
                if not (areCoprime n i) && i % minDiv <> 0 && i > acc then
                    i
                else
                    acc
            loop (i + 1) newAcc
    loop 1 0

// Функция для нахождения суммы цифр числа, которые меньше 5
let sumDigitsLessThan5 number =
    let rec loop n acc =
        if n = 0 then
            acc
        else
            let digit = n % 10
            let newAcc = if digit < 5 then acc + digit else acc
            loop (n / 10) newAcc
    loop (abs number) 0

// Основная функция: произведение
let productResult n =
    let maxNum = maxNotCoprimeNotDivisibleByMinDivisor n
    let sumDigits = sumDigitsLessThan5 n
    maxNum * sumDigits

[<EntryPoint>]
let main argv =
    printfn "Поиск произведения:"
    printfn "  A = максимальное число, не взаимно простое с N,"
    printfn "      не делящееся на наименьший делитель N"
    printfn "  B = сумма цифр N, меньших 5"
    printfn "  Результат = A × B"
    printfn ""
    
    // Тестирование на примерах
    let testNumbers = [12; 15; 20; 24; 30; 36; 48; 60]
    
    printfn "%-10s %-15s %-15s %-10s %-15s" "N" "Мин. делитель" "Макс. число" "Сумма цифр<5" "Произведение"
    printfn "%s" (String.replicate 70 "-")
    
    for n in testNumbers do
        let minDiv = smallestDivisor n
        let maxNum = maxNotCoprimeNotDivisibleByMinDivisor n
        let sumDigits = sumDigitsLessThan5 n
        let product = maxNum * sumDigits
        
        printfn "%-10d %-15d %-15d %-10d %-15d" n minDiv maxNum sumDigits product
    
    printfn ""
    
    // Подробный разбор для числа 30
    printfn "--- Подробный разбор для N = 30 ---"
    let n30 = 30
    let minDiv30 = smallestDivisor n30
    let maxNum30 = maxNotCoprimeNotDivisibleByMinDivisor n30
    let sumDigits30 = sumDigitsLessThan5 n30
    let product30 = maxNum30 * sumDigits30
    
    printfn "Наименьший делитель 30: %d" minDiv30
    printfn "Числа от 1 до 30, не взаимно простые с 30:"
    let notCoprime = [1..30] |> List.filter (fun x -> not (areCoprime n30 x))
    printfn "%A" notCoprime
    printfn "Из них не делящиеся на %d: %A" minDiv30 (notCoprime |> List.filter (fun x -> x % minDiv30 <> 0))
    printfn "Максимальное из них: %d" maxNum30
    printfn ""
    printfn "Цифры числа 30: 3, 0"
    printfn "Цифры, меньшие 5: 3, 0"
    printfn "Сумма цифр <5: %d" sumDigits30
    printfn ""
    printfn "Результат: %d × %d = %d" maxNum30 sumDigits30 product30
    
    printfn ""
    
    // Ввод от пользователя
    printf "Введите число N: "
    let userNumber = int (Console.ReadLine())
    
    let minDiv = smallestDivisor userNumber
    let maxNum = maxNotCoprimeNotDivisibleByMinDivisor userNumber
    let sumDigits = sumDigitsLessThan5 userNumber
    let product = maxNum * sumDigits
    
    printfn "\nРезультат для числа %d:" userNumber
    printfn "Наименьший делитель: %d" minDiv
    printfn "Максимальное число, не взаимно простое с %d, не делящееся на %d: %d" userNumber minDiv maxNum
    printfn "Сумма цифр, меньших 5: %d" sumDigits
    printfn "Произведение: %d × %d = %d" maxNum sumDigits product
    
    0