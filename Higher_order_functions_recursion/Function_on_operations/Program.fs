let digitOperation number operation init =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            loop (n / 10) (operation acc digit)
    loop (abs number) init

let testNumber = 35214

// 1. Сумма цифр (лямбда-выражение)
let sumDigits = digitOperation testNumber (fun acc x -> acc + x) 0
printfn "Сумма цифр: %d (ожидается 15)" sumDigits

// 2. Произведение цифр (лямбда-выражение)
let productDigits = digitOperation testNumber (fun acc x -> acc * x) 1
printfn "Произведение цифр: %d (ожидается 120)" productDigits

// 3. Максимальная цифра (лямбда-выражение)
let maxDigit = digitOperation testNumber (fun acc x -> if x > acc then x else acc) 0
printfn "Максимальная цифра: %d (ожидается 5)" maxDigit

// 4. Минимальная цифра (лямбда-выражение)
let minDigit = digitOperation testNumber (fun acc x -> if x < acc then x else acc) 9
printfn "Минимальная цифра: %d (ожидается 1)" minDigit