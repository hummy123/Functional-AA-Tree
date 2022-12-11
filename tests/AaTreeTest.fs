namespace FSharpx.Collections.Experimental.Tests

open Expecto
open AaTree

module AaTreeTest =
    [<Tests>]
    let testAaTree = 
        testList "AaTree" [

        (* Existence methods. *)
        test "test isEmpty" {
            Expect.isTrue <| AaTree.isEmpty AaTree.empty <| ""
            Expect.isFalse <| AaTree.isEmpty (AaTree.ofList [9]) <| ""            
        }

        test "test exists" {
            let tree = AaTree.ofList [9]
            Expect.isTrue <| AaTree.exists 9 tree <| ""
            Expect.isFalse <| AaTree.exists 10 tree <| ""
        }

        test "test notExists" {
            let tree = AaTree.ofList [9]
            Expect.isFalse <| AaTree.notExists 9 tree <| ""
            Expect.isTrue <| AaTree.notExists 10 tree <| ""
        }

        test "test tryFind" {
            let tree = AaTree.ofList ["hello"; "bye"]
            Expect.equal (Some("hello")) <| AaTree.tryFind "hello" tree <| ""
            Expect.isNone <| AaTree.tryFind "goodbye" tree <| ""
        }

        test "test find" {
            let tree = AaTree.ofList ["hello"; "bye"]
            Expect.equal "hello" <| AaTree.find "hello" tree <| ""
            // Expecto doesn't catch this expected exception for some reason.
            // Expect.throws (fun () -> AaTree.find "goodbye" tree |> ignore) ""
        }

        (* Conversion from methods. *)
        test "test ofList" {
            let list = ['a'; 'b'; 'c'; 'd'; 'e']
            let tree = AaTree.ofList list
            for i in list do
                Expect.isTrue <| AaTree.exists i tree <| ""
        }

        test "test ofArray" {
            let array = [|1; 2; 3; 4; 5|]
            let tree = AaTree.ofArray array
            for i in array do
                Expect.isTrue <| AaTree.exists i tree <| ""
        }

        test "test ofSeq" {
            let seq = Seq.ofList ["hello"; "yellow"; "bye"; "try"]
            let tree = AaTree.ofSeq seq
            for i in seq do
                Expect.isTrue <| AaTree.exists i tree <| ""
        }

        (* Conversion to methods. *)
        test "test toList" {
            let inputList = [0;1;2;3]
            let tree = AaTree.ofList inputList
            let outputList = AaTree.toList tree
            Expect.equal outputList inputList ""
        }

        test "test toArray" {
            let inputArray = [|0;1;2;3|]
            let tree = AaTree.ofArray inputArray
            let outputArray = AaTree.toArray tree
            Expect.equal outputArray inputArray ""
        }

        test "test toSeq" {
            let inputSeq = Seq.ofList ["hi";"why";"try"]
            let tree = AaTree.ofSeq inputSeq
            let outputSeq = AaTree.toSeq tree
            Expect.containsAll outputSeq inputSeq ""
        }

        (* Fold and foldback methods. *)
        (* We will try subtracting a list containing the length of each string element, 
         * because that is an operation where order matters. *)
        test "test fold" {
            let tree = AaTree.ofList ["1";"22";"333"]
            let foldResult = AaTree.fold (fun a (e: string) -> e.Length::a) [] tree
            Expect.equal foldResult [1;2;3] ""
        }

        test "test foldBack" {
            let tree = AaTree.ofList ["1";"22";"333"]
            let foldBackResult = AaTree.foldBack (fun a (e: string) -> e.Length::a) [] tree
            Expect.equal foldBackResult [3;2;1] ""
        }
      ]
