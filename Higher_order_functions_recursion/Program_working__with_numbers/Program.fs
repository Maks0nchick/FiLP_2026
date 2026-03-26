
let rec sumDigitsUp n =
    if n = 0 then 0
    else (n % 10) + sumDigitsUp (n / 10)

let sumDigits n = sumDigitsUp (abs n)

printfn "Тестирование рекурсии вверх:"
printfn "Сумма цифр 123: %d (ожидается 6)" (sumDigits 123)
printfn "Сумма цифр 4567: %d (ожидается 22)" (sumDigits 4567)
printfn "Сумма цифр 0: %d (ожидается 0)" (sumDigits 0)
printfn "Сумма цифр -123: %d (ожидается 6)" (sumDigits -123)

printfn "\n--- Ввод от пользователя ---"
printf "Введите число: "
let number = int (System.Console.ReadLine())

printfn "Сумма цифр числа %d = %d" number (sumDigits number)