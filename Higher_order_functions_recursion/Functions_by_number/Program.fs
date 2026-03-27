// ==================== Задание 20 ====================
// Программа: пользователь вводит кортеж (номер функции, аргумент)
// Реализация через каррирование и суперпозицию

open System

// ==================== Три функции из задания 19 ====================

// Метод 1: количество четных чисел, не взаимно простых с данным
let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

let areCoprime a b = gcd a b = 1

let isEven x = x % 2 = 0
let isNotCoprimeWith n x = not (areCoprime n x)

let countEvenNotCoprime n =
    let rec loop i acc =
        if i > n then acc
        else
            let newAcc = if isEven i && isNotCoprimeWith n i then acc + 1 else acc
            loop (i + 1) newAcc
    loop 1 0

// Метод 2: максимальная цифра числа, не делящаяся на 3
let isNotDivisibleBy3 x = x % 3 <> 0

let maxDigitNotDivisibleBy3 number =
    let rec loop n acc =
        if n = 0 then acc
        else
            let digit = n % 10
            let newAcc = if isNotDivisibleBy3 digit && digit > acc then digit else acc
            loop (n / 10) newAcc
    let result = loop (abs number) -1
    if result = -1 then 0 else result

// Метод 3: произведение максимального числа, не взаимно простого с данным,
//         не делящегося на наименьший делитель, и суммы цифр <5
let smallestDivisor n =
    if n <= 1 then 1
    else
        let rec loop d =
            if d * d > n then n
            elif n % d = 0 then d
            else loop (d + 1)
        loop 2

let isNotDivisibleBy divisor x = x % divisor <> 0

let maxNotCoprimeNotDivisibleByMinDivisor n =
    let minDiv = smallestDivisor n
    let rec loop i acc =
        if i > n then acc
        else
            let newAcc = if not (areCoprime n i) && isNotDivisibleBy minDiv i && i > acc then i else acc
            loop (i + 1) newAcc
    loop 1 0

let sumDigitsLessThan5 number =
    let rec loop n acc =
        if n = 0 then acc
        else
            let digit = n % 10
            let newAcc = if digit < 5 then acc + digit else acc
            loop (n / 10) newAcc
    loop (abs number) 0

let productResult n =
    let maxNum = maxNotCoprimeNotDivisibleByMinDivisor n
    let sumDigits = sumDigitsLessThan5 n
    maxNum * sumDigits

// ==================== Функция, возвращающая функцию по номеру ====================
let getFunctionByNumber number =
    match number with
    | 1 -> countEvenNotCoprime
    | 2 -> maxDigitNotDivisibleBy3
    | 3 -> productResult
    | _ -> failwith "Неверный номер функции. Введите число от 1 до 3"

// ==================== Ввод данных ====================
let readInput () =
    printfn "Введите кортеж (номер функции, аргумент):"
    printfn "Номер функции (1 - количество четных не взаимно простых,"
    printfn "                2 - максимальная цифра не делящаяся на 3,"
    printfn "                3 - произведение по условию):"
    let funcNum = int (Console.ReadLine())
    printfn "Введите аргумент (целое число):"
    let arg = int (Console.ReadLine())
    (funcNum, arg)

// ==================== СПОСОБ 1: КАРРИРОВАНИЕ ====================
printfn "\n========== СПОСОБ 1: КАРРИРОВАНИЕ =========="

// Каррирование: последовательное применение функций
let curryMain =
    let input = readInput()                                    // читаем кортеж
    let func = getFunctionByNumber (fst input)                 // получаем функцию по номеру
    let result = func (snd input)                              // применяем к аргументу
    printfn "\nРезультат: %d" result                           // выводим результат

// Запуск через каррирование (раскомментировать для запуска)
// curryMain

// ==================== СПОСОБ 2: СУПЕРПОЗИЦИЯ ====================
printfn "\n========== СПОСОБ 2: СУПЕРПОЗИЦИЯ =========="

// Суперпозиция: композиция функций через оператор >>
let applyFunction (funcNum, arg) = (getFunctionByNumber funcNum) arg
let printResult result = printfn "\nРезультат: %d" result

// Композиция: readInput >> applyFunction >> printResult
let superposeMain = readInput >> applyFunction >> printResult

// Запуск через суперпозицию (раскомментировать для запуска)
// superposeMain ()

// ==================== ТОЧКА ВХОДА ====================
[<EntryPoint>]
let main argv =
    printfn "Выберите способ выполнения:"
    printfn "1 - Каррирование"
    printfn "2 - Суперпозиция"
    printf "Ваш выбор: "
    
    match int (Console.ReadLine()) with
    | 1 ->
        printfn "\n--- Реализация через каррирование ---"
        let input = readInput()
        let func = getFunctionByNumber (fst input)
        let result = func (snd input)
        printfn "\nРезультат: %d" result
    | 2 ->
        printfn "\n--- Реализация через суперпозицию ---"
        let apply (funcNum, arg) = (getFunctionByNumber funcNum) arg
        let print res = printfn "\nРезультат: %d" res
        let program = readInput >> apply >> print
        program ()
    | _ ->
        printfn "Неверный выбор"
    
    0