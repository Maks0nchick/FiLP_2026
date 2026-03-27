// ==================== Задание 6 ====================
printfn "\n========== Задание 6 =========="
printfn "Двоичное дерево с элементом строка"
printfn ""

open System

type BinaryTree = 
    | Node of string * BinaryTree * BinaryTree
    | Nil

let rec insert tree value =
    match tree with
    | Nil -> Node(value, Nil, Nil)
    | Node(v, left, right) ->
        if value < v then Node(v, insert left value, right)
        elif value > v then Node(v, left, insert right value)
        else tree

let rec inorder tree =
    match tree with
    | Nil -> []
    | Node(v, left, right) -> inorder left @ [v] @ inorder right

let rec preorder tree =
    match tree with
    | Nil -> []
    | Node(v, left, right) -> [v] @ preorder left @ preorder right

let rec postorder tree =
    match tree with
    | Nil -> []
    | Node(v, left, right) -> postorder left @ postorder right @ [v]

let rec contains tree value =
    match tree with
    | Nil -> false
    | Node(v, left, right) ->
        if value = v then true
        elif value < v then contains left value
        else contains right value

let rec height tree =
    match tree with
    | Nil -> 0
    | Node(_, left, right) -> 1 + max (height left) (height right)

let rec size tree =
    match tree with
    | Nil -> 0
    | Node(_, left, right) -> 1 + size left + size right

let rec printTree tree indent =
    match tree with
    | Nil -> ()
    | Node(v, left, right) ->
        printTree right (indent + "    ")
        printfn "%s%s" indent v
        printTree left (indent + "    ")

let rec readList n = 
    if n = 0 then []
    else
        let head = Console.ReadLine()
        let tail = readList (n - 1)
        head :: tail

[<EntryPoint>]
let main argv =
    printf "Введите количество строк: "
    let n = Convert.ToInt32(Console.ReadLine())
    printfn "Введите %d строк:" n
    let lst = readList n
    
    let rec buildTree list tree =
        match list with
        | [] -> tree
        | h :: t -> buildTree t (insert tree h)
    
    let tree = buildTree lst Nil
    
    printfn "\nДвоичное дерево:"
    printTree tree ""
    
    printfn "\nКоличество узлов: %d" (size tree)
    printfn "Высота дерева: %d" (height tree)
    
    printfn "\nСимметричный обход (inorder):"
    inorder tree |> List.iter (printfn "%s")
    
    printfn "\nПрямой обход (preorder):"
    preorder tree |> List.iter (printfn "%s")
    
    printfn "\nОбратный обход (postorder):"
    postorder tree |> List.iter (printfn "%s")
    
    printf "\n\nВведите строку для поиска: "
    let searchValue = Console.ReadLine()
    printfn "Элемент найден: %b" (contains tree searchValue)
    
    0