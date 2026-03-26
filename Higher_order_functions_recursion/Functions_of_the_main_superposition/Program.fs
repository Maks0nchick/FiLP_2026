open System

// Базовые функции
let normalize (s: string) = s.Trim().ToLower()

let getComment (language: string) : string =
    match language with
    | "f#" | "fsharp" | "f шарп" | "f sharp" -> 
        "Ты — подлиза! F# — отличный выбор! 🚀"
    | "prolog" | "пролог" -> 
        "Ты — подлиза! Prolog — это классика логического программирования! 🧠"
    | "python" | "питон" -> 
        "Python — это просто и элегантно! Хороший выбор для начинающих 🐍"
    | _ -> 
        $"\"{language}\" — интересный выбор! Программирование — это здорово на любом языке! 😊"

let printResponse response = printfn "\n%s" response

let readInput () = Console.ReadLine()

// Способ 1: Суперпозиция
let processWithSuperposition = readInput >> normalize >> getComment >> printResponse

// Способ 2: Каррирование
let processWithCurrying input =
    let normalized = normalize input
    let comment = getComment normalized
    printResponse comment

[<EntryPoint>]
let main argv =
    printfn "=== Демонстрация суперпозиции ==="
    printfn "Какой язык программирования вы любите? "
    processWithSuperposition()   // суперпозиция
    
    printfn "\n=== Демонстрация каррирования ==="
    printfn "Какой язык программирования вы любите? "
    let input = readInput()
    processWithCurrying input    // каррирование
    
    0