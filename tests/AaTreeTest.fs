namespace FSharpx.Collections.Experimental.Tests

open Expecto

module AaTreeTest =
    [<Tests>]
    let testAaTree = 
      testList "AaTree" [
        testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
          let subject = true
          Expect.isTrue subject "I compute, therefore I am."

      ]
