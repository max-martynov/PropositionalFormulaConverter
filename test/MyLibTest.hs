module Main (main) where
import Text.Read (Lexeme(String))
import MyLib
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests:: TestTree
tests = testGroup "My test group"
    [
        testGroup "NNF tests" [
            testCase "test 1: simple" $ toNNF (Not (Var "a"))
                @?= Not (Var "a"),
            testCase "test 2: De Morgan's laws" $ toNNF (Not (Var "a" :|: Not (Var "b")))
                @?= Not (Var "a") :&: Var "b",
            testCase "test 3: double not" $ toNNF (Not (Not (Var "a" :<->: Var "b")))
                @?= (Not (Var "a") :|: Var "b") :&: (Var "a" :|: Not (Var "b")),
            testCase "test 4: already good" $ toNNF ((Not (Var "a") :&: Var "b") :|: (Var "a" :|: Not (Var "b")) :|: (Var "c"))
                @?= ((Not (Var "a") :&: Var "b") :|: (Var "a" :|: Not (Var "b"))) :|: Var "c",
            testCase "test5: complex test" $ toNNF (Not (Var "a" :<->: Not (Var "b") :->: Var "c"))
                 @?= (Not (Var "a") :|: Not (Var "b")) :&: (Var "a" :|: Var "b") :&: Not (Var "c"),
            testCase "test6: complex test" $ toNNF ((Not (Not (Not (Var "a" :|: Var "b") ))) :&: (Not (Var "a") :|: Not (Var "b")))
                @?= Not (Var "a") :&: Not (Var "b") :&: (Not (Var "a") :|: Not (Var "b"))
        ],
        testGroup "CNF tests" [
            testCase "test 1: simple" $ toCNF (Not (Var "a") :|: (Var "b" :&: Var "c"))
                @?= (Not (Var "a") :|: Var "b") :&: (Not (Var "a") :|: Var "c"),
            testCase "test2: with NNF" $ toCNF (Not (Var "a" :->: Var "b"))
                @?= Var "a" :&: Not (Var "b"),
            testCase "test 3: already good" $ toCNF ((Not (Var "a") :|: Var "b") :|: (Var "c" :|: Var "d"))
                @?= (Not (Var "a") :|: Var "b") :|: (Var "c" :|: Var "d"),
            testCase "test 4: complex" $ toCNF ((Var "a" :->: Var "b") :&: ((Not (Var "b") :->: Var "c") :->: Not (Var "a")))
                @?= (Not (Var "a") :|: Var "b") :&: ((Not (Var "b") :|: Not (Var "a")) :&: (Not (Var "c") :|: Not (Var "a"))),
            testCase "test 5: complex" $ toCNF ((Var "a" :<->: Var "b") :&: Var "c")
                @?= ((Not (Var "a") :|: Var "b") :&: (Var "a" :|: Not (Var "b"))) :&: Var "c"
        ],
        testGroup "DNF tests" [
            testCase "test 1: simple" $ toDNF (Not (Var "a") :&: (Var "b" :|: Var "c"))
                @?= (Not (Var "a") :&: Var "b") :|: (Not (Var "a") :&: Var "c"),
            testCase "test2: with NNF" $ toDNF (Not (Var "a" :->: Var "b"))
                @?= Var "a" :&: Not (Var "b"),
            testCase "test 3: already good" $ toDNF ((Not (Var "a") :&: Var "b") :|: (Var "c" :&: Var "d"))
                @?= (Not (Var "a") :&: Var "b") :|: (Var "c" :&: Var "d"),
            testCase "test 4: complex" $ toDNF (Not ((Var "a" :->: Var "b") :|: (Not ((Var "b") :->: Var "c"))))
                @?= ((Var "a" :&: Not (Var "b")) :&: Not (Var "b")) :|: ((Var "a" :&: Not (Var "b")) :&: Var "c"),
            testCase "test 5: complex" $ toDNF ((Var "a" :<->: Var "b") :&: Var "c")
                @?= (((Not (Var "a") :|: Var "b") :&: Var "a") :&: Var "c") :|: (((Not (Var "a") :|: Var "b") :&: Not (Var "b")) :&: Var "c")
        ]
    ]



