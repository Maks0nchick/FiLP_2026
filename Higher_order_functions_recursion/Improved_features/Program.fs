// ==================== Задание 19 ====================
// Модифицированные функции из задания 16 с использованием функций из заданий 1-17

open System

// ==================== Функции из предыдущих заданий ====================

// Из задания 13: НОД (наибольший общий делитель)
let rec gcd a b =
    match b with
    | 0 -> abs a
    | _ -> gcd b (a % b)

// Из задания 13: проверка на взаимную простоту
let areCoprime a b = gcd a b = 1

// Из задания 13: обход взаимно простых чисел
let coprimeComponents number operation init =
    let rec loop i acc =
        if i > number then acc
        else
            let newAcc = if gcd number i = 1 then operation acc i else acc
            loop (i + 1) newAcc
    loop 1 init

// Из задания 15: обход взаимно простых чисел с условием
let coprimeComponentsWithCondition number operation init condition =
    let rec loop i acc =
        if i > number then acc
        else
            let newAcc = if gcd number i = 1 && condition i then operation acc i else acc
            loop (i + 1) newAcc
    loop 1 init

// Из задания 9: обход цифр числа с условием
let digitOperationWithCondition number operation init condition =
    let rec loop n acc =
        match n with
        | 0 -> acc
        | _ ->
            let digit = n % 10
            let newAcc = if condition digit then operation acc digit else acc
            loop (n / 10) newAcc
    loop (abs number) init

// ==================== Общие вспомогательные функции ====================

// Проверка на четность
let isEven x = x % 2 = 0

// Проверка, что число НЕ взаимно простое с данным
let isNotCoprimeWith n x = not (areCoprime n x)

// Проверка, что число НЕ делится на 3
let isNotDivisibleBy3 x = x % 3 <> 0

// Проверка, что цифра меньше 5
let isLessThan5 x = x < 5

// Проверка, что число НЕ делится на делитель
let isNotDivisibleBy divisor x = x % divisor <> 0

// Нахождение наименьшего делителя числа (больше 1)
let smallestDivisor n =
    if n <= 1 then 1
    else
        let rec loop d =
            if d * d > n then n
            elif n % d = 0 then d
            else loop (d + 1)
        loop 2

// Функция для нахождения максимума по условию (обход чисел от 1 до n)
let maxNumberWithCondition n condition =
    let rec maxOp acc x = if x > acc then x else acc
    coprimeComponentsWithCondition n maxOp 0 condition

// Функция для нахождения максимума по условию (обход цифр)
let maxDigitWithCondition number condition =
    digitOperationWithCondition number (fun acc x -> if x > acc then x else acc) 0 condition

// Функция для нахождения суммы по условию (обход цифр)
let sumDigitsWithCondition number condition =
    digitOperationWithCondition number (+) 0 condition

// Функция для подсчета чисел по условию (обход чисел от 1 до n)
let countNumbersWithCondition n condition =
    coprimeComponentsWithCondition n (fun acc _ -> acc + 1) 0 condition

// ==================== МЕТОД 1 ====================
// Найти количество четных чисел, не взаимно простых с данным
let countEvenNotCoprime n =
    let condition i = isEven i && isNotCoprimeWith n i
    countNumbersWithCondition n condition

// ==================== МЕТОД 2 ====================
// Найти максимальную цифру числа, не делящуюся на 3
let maxDigitNotDivisibleBy3 number =
    maxDigitWithCondition number isNotDivisibleBy3

// ==================== МЕТОД 3 ====================
// Найти произведение:
//   1. максимального числа, не взаимно простого с данным,
//      не делящегося на наименьший делитель исходного числа
//   2. суммы цифр числа, меньших 5

// Максимальное число, не взаимно простое с n, не делящееся на наименьший делитель n
let maxNotCoprimeNotDivisibleByMinDivisor n =
    let minDiv = smallestDivisor n
    let condition i = isNotCoprimeWith n i && isNotDivisibleBy minDiv i
    maxNumberWithCondition n condition

// Сумма цифр числа, меньших 5
let sumDigitsLessThan5 number =
    sumDigitsWithCondition number isLessThan5

// Итоговое произведение
let productResult n =
    let maxNum = maxNotCoprimeNotDivisibleByMinDivisor n
    let sumDigits = sumDigitsLessThan5 n
    maxNum * sumDigits