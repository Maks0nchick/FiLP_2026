let sumDigits n =
    let rec loop num acc =
        match num with
        | 0 -> acc
        | _ -> loop (num / 10) (acc + num % 10)
    loop (abs n) 0

let factorial n =
    let rec loop num acc =
        match num with
        | 0 | 1 -> acc
        | _ -> loop (num - 1) (acc * num)
    loop n 1

let getOperation flag =
    match flag with
    | true -> 
        printfn "Выбрана функция: сумма цифр"
        sumDigits
    | false -> 
        printfn "Выбрана функция: факториал"
        factorial

// Пример 1: true (сумма цифр)
let sumFunc = getOperation true
printfn "Сумма цифр 12345: %d (ожидается 15)" (sumFunc 12345)

// Пример 2: false (факториал)
let factFunc = getOperation false
printfn "Факториал 5: %d (ожидается 120)" (factFunc 5)

