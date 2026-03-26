let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

let coprimeComponents number operation init =
    let rec loop i acc =
        if i > number then
            acc
        else
            let newAcc = if gcd number i = 1 then operation acc i else acc
            loop (i + 1) newAcc
    loop 1 init

let coprimeComponentsWithCondition number operation init condition =
    let rec loop i acc =
        if i > number then
            acc
        else
            // Проверяем: число взаимно простое И удовлетворяет условию
            let newAcc = if gcd number i = 1 && condition i then operation acc i else acc
            loop (i + 1) newAcc
    loop 1 init

let n = 30
printfn "Число N = %d" n
printfn "Взаимно простые числа с %d: 1, 7, 11, 13, 17, 19, 23, 29" n
printfn ""

// Пример 1: Сумма взаимно простых чисел, которые являются простыми
let isPrime x =
    if x < 2 then false
    else
        let rec check d =
            if d * d > x then true
            elif x % d = 0 then false
            else check (d + 1)
        check 2

let sumPrimeCoprimes = coprimeComponentsWithCondition n (+) 0 isPrime
printfn "1. Сумма взаимно простых чисел, являющихся простыми: %d" sumPrimeCoprimes
printfn "   (простые среди {1,7,11,13,17,19,23,29}: 7+11+13+17+19+23+29 = 119)"


// Пример 2: Количество взаимно простых чисел, которые больше 10
let countGreaterThan10 = coprimeComponentsWithCondition n (fun acc _ -> acc + 1) 0 (fun x -> x > 10)
printfn "2. Количество взаимно простых чисел >10: %d" countGreaterThan10
printfn "   (>10: 11,13,17,19,23,29 → 6 чисел)"

// Пример 3: Произведение взаимно простых чисел, которые меньше 20
let productLessThan20 = coprimeComponentsWithCondition n (*) 1 (fun x -> x < 20)
printfn "3. Произведение взаимно простых чисел <20: %d" productLessThan20
printfn "   (<20: 1,7,11,13,17,19 → 1*7*11*13*17*19 = 323323)"