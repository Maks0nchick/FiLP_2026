let digitOperationWithCondition number operation init condition =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            // Если цифра удовлетворяет условию, применяем операцию
            let newAcc = if condition digit then operation acc digit else acc
            loop (n / 10) newAcc
    loop (abs number) init

let testNumber = 352814

// 1. Сумма чётных цифр (цифры, которые делятся на 2)
let sumEvenDigits = digitOperationWithCondition testNumber (+) 0 (fun x -> x % 2 = 0)
printfn "Сумма чётных цифр: %d (ожидается 10)" sumEvenDigits

// 2. Сумма нечётных цифр
let sumOddDigits = digitOperationWithCondition testNumber (+) 0 (fun x -> x % 2 <> 0)
printfn "Сумма нечётных цифр: %d (ожидается 8)" sumOddDigits

// 3. Произведение цифр больше 3
let productGreaterThan3 = digitOperationWithCondition testNumber (*) 1 (fun x -> x > 3)
printfn "Произведение цифр >3: %d (ожидается 280)" productGreaterThan3

// 4. Сумма цифр, которые меньше 5
let sumLessThan5 = digitOperationWithCondition testNumber (+) 0 (fun x -> x < 5)
printfn "Сумма цифр <5: %d (ожидается 9)" sumLessThan5

// 5. Количество цифр, равных 5
let countDigit5 = digitOperationWithCondition testNumber (fun acc _ -> acc + 1) 0 (fun x -> x = 5)
printfn "Количество цифр 5: %d (ожидается 2)" countDigit5

printfn ""