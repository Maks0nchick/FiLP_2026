// ==================== Задание 10 ====================
printfn "\nЗадание 10: Проверка функции на 3 различных примерах"

let digitOperationWithCondition number operation init condition =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            let newAcc = if condition digit then operation acc digit else acc
            loop (n / 10) newAcc
    loop (abs number) init

printfn "--- Тестирование на числе 5736251 ---"

let number = 5736251
printfn "Исходное число: %d" number
printfn ""

// Пример 1: Количество цифр 5 в числе
let countDigit5 = digitOperationWithCondition number (fun acc _ -> acc + 1) 0 (fun x -> x = 5)
printfn "1. Количество цифр 5 в %d: %d (ожидается 2)" number countDigit5

// Пример 2: Сумма цифр, которые больше 4
let sumGreaterThan4 = digitOperationWithCondition number (+) 0 (fun x -> x > 4)
printfn "2. Сумма цифр >4 в %d: %d (ожидается 23)" number sumGreaterThan4

// Пример 3: Минимальная нечётная цифра
let minOddDigit = digitOperationWithCondition number min 9 (fun x -> x % 2 <> 0)
printfn "3. Минимальная нечётная цифра в %d: %d (ожидается 1)" number minOddDigit