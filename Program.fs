open System    //10,9 2,6 4,7 18,9 20,1 1,5

type BinaryTree =
    | Node of int * BinaryTree * BinaryTree
    | Empty

    // функция для графического отображения дерева
let rec printt tree (indent: string) (last: bool) =
    match tree with
    | Empty -> ()
    | Node(data, left, right) ->
        printf "%s%s%d\n" indent (if last then "└── " else "├── ") data
        let indent' = indent + (if last then "    " else "│   ")
        printt left indent' false
        printt right indent' true



    // Функция для округления вещественных чисел
let rounding_up (x: float) : int =
    Math.Round(x) |> int
    

    //  Функция для вставки значения в бинарное дерево
let rec insert (tree: BinaryTree) (value: int) : BinaryTree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(data, left, right) ->
        if value < data then
            Node(data, insert left value, right)
        else
            Node(data, left, insert right value)



    // Функция для чтения ввода пользователя
let readinput() =
    let mutable numbers = []
    while numbers.Length = 0 do
        printfn "Введите список вещественных чисел через пробел:"
        let input = Console.ReadLine()
        numbers <-
            input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.map (fun s -> 
                let success, value = Double.TryParse(s)
                if success then rounding_up value else failwith $"Неверный ввод: '{s}' не является действительным числом")
        
        if numbers.Length = 0 then
            printfn "Не были введены числа. Попробуйте снова."
    numbers

[<EntryPoint>]
let main argv =
    let numbers = readinput()
    
    // Сначала создаем дерево с корнем
    let root = List.head numbers
    let mutable tree = Empty
    tree <- insert tree (rounding_up (float root))

    // Вставляем остальные элементы
    for number in List.tail numbers do
        tree <- insert tree (rounding_up (float number))

    printfn "\nДерево:"
    printt tree "" true
    
    
    Console.ReadKey() |> ignore
    0