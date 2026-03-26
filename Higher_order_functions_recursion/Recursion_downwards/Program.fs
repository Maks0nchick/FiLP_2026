let rec sumDigitsDownExpr n =
    match n with
    | 0 -> 0
    | _ -> (n % 10) + sumDigitsDownExpr (n / 10)

// 2. Хвостовая рекурсия (рекурсия вниз с аккумулятором)
// Результат накапливается в параметре acc, нет операций после вызова
let rec sumDigitsTailRec n acc =
    match n with
    | 0 -> acc
    | _ -> sumDigitsTailRec (n / 10) (acc + n % 10)

// Функция-обёртка для удобства
let sumDigitsTail n = sumDigitsTailRec (abs n) 0

// Функция для работы с отрицательными числами (рекурсия вниз)
let sumDigits n = sumDigitsDownExpr (abs n)