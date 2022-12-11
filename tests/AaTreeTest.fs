namespace FSharpx.Collections.Experimental.Tests

open Expecto
open AaTree

module AaTreeTest =
    [<Tests>]
    let testAaTree = 
        testList "AaTree" [
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

        test "test findind" {
            let tree = AaTree.ofList ["hello"; "bye"]
            Expect.equal "hello" <| AaTree.find "hello" tree <| ""
            Expect.throws (fun () -> AaTree.find "goodbye" tree |> ignore) ""
        }
      ]
