printfn "Решаем квадратное уравнение ax² + bx + c = 0"
printfn "Введите a:"
let a = float (System.Console.ReadLine())

printfn "Введите b:"
let b = float (System.Console.ReadLine())

printfn "Введите c:"
let c = float (System.Console.ReadLine())

printfn ""

if a = 0.0 then
    printfn "Это линейное уравнение!"
else
    let D = b * b - 4.0 * a * c

    printfn "Дискриминант = %f" D

    if D < 0.0 then
        printfn "Нет вещественных корней"
    elif D = 0.0 then
        let x = -b / (2.0 * a)
        printfn "Один корень: x = %f" x
    else
        let sqrtD = sqrt D
        let x1 = (-b + sqrtD) / (2.0 * a)
        let x2 = (-b - sqrtD) / (2.0 * a)
        printfn "Два корня:"
        printfn "x1 = %f" x1
        printfn "x2 = %f" x2
