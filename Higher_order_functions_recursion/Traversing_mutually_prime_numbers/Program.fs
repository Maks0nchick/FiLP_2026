let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

let coprimeComponents number operation init =
    let rec loop i acc =
        if i > number then
            acc
        else
            // Если числа взаимно просты, применяем операцию
            let newAcc = if gcd number i = 1 then operation acc i else acc
            loop (i + 1) newAcc
    loop 1 init

let number = 8
printfn "Число: %d" number
printfn "Числа от 1 до 8, взаимно простые с 8: 1, 3, 5, 7"
printfn ""

// 1. Сумма взаимно простых чисел
let sumCoprime = coprimeComponents number (+) 0
printfn "1. Сумма взаимно простых чисел: %d (1+3+5+7 = 16)" sumCoprime

// 2. Произведение взаимно простых чисел
let productCoprime = coprimeComponents number (*) 1
printfn "2. Произведение взаимно простых чисел: %d (1*3*5*7 = 105)" productCoprime

// 3. Количество взаимно простых чисел
let countCoprime = coprimeComponents number (fun acc _ -> acc + 1) 0
printfn "3. Количество взаимно простых чисел: %d (ожидается 4)" countCoprime