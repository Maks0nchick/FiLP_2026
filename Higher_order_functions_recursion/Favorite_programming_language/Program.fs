let getComment (language: string) : string =
    let lang = language.Trim().ToLower()  // приводим к нижнему регистру и убираем пробелы
    
    match lang with
    | "f#" | "fsharp" | "f шарп" | "f sharp" -> 
        "Ты — подлиза! F# — отличный выбор!"
    
    | "prolog" | "пролог" -> 
        "Ты — подлиза! Prolog — это классика логического программирования!"
    
    | "python" | "питон" -> 
        "Python — это просто и элегантно! Хороший выбор для начинающих"
    
    | _ -> 
        $"\"{language}\" — интересный выбор! Программирование — это здорово на любом языке!"
    
printfn "--- Проверка на примерах ---"

let testLanguages = ["F#"; "Prolog"; "Python"; "Java"; "C#"; "Rust"; "JavaScript"; "Brainfuck"]

for lang in testLanguages do
    printfn "Язык: %s → %s" lang (getComment lang)

printfn ""

printfn "--- Ввод от пользователя ---"

printf "Какой язык программирования вы любите? "
let userLanguage = System.Console.ReadLine()

let response = getComment userLanguage
printfn "\n%s" response