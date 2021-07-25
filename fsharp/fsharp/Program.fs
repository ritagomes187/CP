// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Cp
open Nat
open LTree
open BTree

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    let t1 : BTree<int> = Node(2, (Empty, Empty))
    let t2 : BTree<int> = Node(5, (Empty, Node(2, (Empty, Empty))))
    let e1 = Right (2, (Empty, Empty))
    let e2 = Right (5, (Empty, Node (2,(Empty, Empty))))
    
    printfn "Bubble Sort [5,3,2,9,11] = %A\n" (bSort [5;3;2;9;11])
    printfn "inBTree Right (2, (Empty, Empty)) = %A" (inBTree e1)
    printfn "outBTree Node(2, (Empty, Empty)) = %A\n" (outBTree t1)
    printfn "inBTree Right (5, (Empty, Node (2,(Empty, Empty)))) = %A" (inBTree e2)
    printfn "outBTree Node(5, (Empty, Node(2, (Empty, Empty)))) = %A\n" (outBTree t2)
    printfn "countBTree Node(2, (Empty, Empty)) = %A\n" (countBTree t1)
    printfn "countBTree Node(5, (Empty, Node(2, (Empty, Empty)))) = %A\n" (countBTree t2)
    printfn "Strategy (true,5) = %A\n" (strategy (true,5))
    printfn "Hanoi (true, 5) = %A\n" (hanoi (true, 5))
    printfn "Merge Sort [5,3,2,9,11] = %A\n" (mSort' [5;3;2;9;11])
    printfn "Result union [1;1;2;3;4] [1;5] = %A\n" (union [1;1;2;3;4] [1;5])
    printfn "Tunion = %A\n" (tunion (1, ([[8;66;54]], [[7;9;0]])))
    printfn "Quick sort partition [5;4;5;1;2;8;3] = %A\n" (qsep [5;4;5;1;2;8;3])
    printfn "Quick sort result [5;4;5;1;2;8;3] = %A\n" (qSort' [5;4;5;1;2;8;3])
    printfn "Bal Depth Node(2, (Empty, Empty)) ? = %A\n" (balBTree t1)
    printfn "Bal Depth Node(5, (Empty, Node(2, (Empty, Empty)))) ? = %A\n" (balBTree t2)
    printfn "Bal Node(2, (Empty, Empty)) ? = %A\n" (balBTree t1)
    printfn "Bal Node(5, (Empty, Node(2, (Empty, Empty)))) ? = %A\n" (balBTree t2)
    printfn "Depth Node(5, (Empty, Node(2, (Empty, Empty)))) = %A\n" (depthBTree t2)
    0 // return an integer exit code