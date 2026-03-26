let digitOperation number operation init =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            loop (n / 10) (operation acc digit)
    loop (abs number) init

// 1. Сумма цифр числа
let sumDigits = digitOperation 35214 (+) 0
printfn "Сумма цифр 35214: %d (ожидается 15)" sumDigits

// 2. Произведение цифр числа
let productDigits = digitOperation 123 (*) 1
printfn "Произведение цифр 123: %d (ожидается 6)" productDigits

// 3. Максимальная цифра числа
let maxDigit = digitOperation 491 max 0
printfn "Максимальная цифра 491: %d (ожидается 9)" maxDigit

// 4. Минимальная цифра числа
let minDigit = digitOperation 573 min 9
printfn "Минимальная цифра 573: %d (ожидается 3)" minDigit

// 5. Количество цифр в числе
let countDigits = digitOperation 35214 (fun acc _ -> acc + 1) 0
printfn "Количество цифр в 35214: %d (ожидается 5)" countDigits