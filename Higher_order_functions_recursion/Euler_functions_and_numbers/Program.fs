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

// ==================== 1. Тестирование функции ====================
printfn "--- 1. Тестирование функции coprimeComponents ---"

let testNumbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 15; 20; 30; 36]

printfn "%-8s %-20s %-10s %-15s" "n" "Взаимно простые числа" "Кол-во" "Сумма"
printfn "%s" (String.replicate 55 "-")

for n in testNumbers do
    // Получаем список взаимно простых чисел
    let coprimes = coprimeComponents n (fun acc x -> acc @ [x]) []
    let count = coprimeComponents n (fun acc _ -> acc + 1) 0
    let sum = coprimeComponents n (+) 0
    
    // Формируем строку со списком (не более 20 символов)
    let listStr = 
        if List.length coprimes <= 5 then
            sprintf "%A" coprimes
        else
            sprintf "%A" (List.take 5 coprimes) + "..."
    
    printfn "%-8d %-20s %-10d %-15d" n listStr count sum

printfn ""

// ==================== 2. Функция Эйлера φ(n) ====================
printfn "--- 2. Функция Эйлера φ(n) (количество взаимно простых чисел) ---"

// Функция Эйлера через coprimeComponents
let eulerPhi n =
    coprimeComponents n (fun acc _ -> acc + 1) 0

printfn "Проверка функции Эйлера:"
printfn "%-8s %-8s %-10s" "n" "φ(n)" "Ожидание"
printfn "%s" (String.replicate 30 "-")

let expectedPhi = [
    (1, 1); (2, 1); (3, 2); (4, 2); (5, 4); (6, 2); (7, 6); (8, 4); 
    (9, 6); (10, 4); (11, 10); (12, 4); (13, 12); (14, 6); (15, 8); 
    (16, 8); (20, 8); (30, 8); (36, 12)
]

for (n, expected) in expectedPhi do
    let result = eulerPhi n
    printfn "%-8d %-8d %-10s" n result (if result = expected then "✅" else "❌")

printfn ""

// ==================== 3. Функция для суммы взаимно простых чисел ====================
printfn "--- 3. Сумма чисел, взаимно простых с n ---"

let sumOfCoprimes n =
    coprimeComponents n (+) 0

printfn "%-8s %-10s %-10s" "n" "Сумма" "Формула (n*φ(n)/2)"
printfn "%s" (String.replicate 35 "-")

for n in [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 12; 15; 20; 30] do
    let sum = sumOfCoprimes n
    let phi = eulerPhi n
    let formula = n * phi / 2
    printfn "%-8d %-10d %-10d" n sum formula

printfn "\nПримечание: Сумма чисел, взаимно простых с n, равна n * φ(n) / 2"