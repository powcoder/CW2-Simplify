https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module Main (main) where

import Simplify
import Expr
import Poly

import Test.Tasty -- This is a testing framework which can nicely format our QuickCheck tests
import Test.Tasty.QuickCheck -- This is just Test.QuickCheck with a few extras for Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Simplify Coursework"
          [ task1
          , task2
          , task3
          , task4
          , task5
          , task6
          , task7
          ]

task1 :: TestTree
task1 = testGroup "Task 1: `add`"
  [ testProperty "No-negative exponents: noNegExp (add expr1 expr2)"
      (\expr1 expr2 -> noNegExp (add expr1 expr2))
  , testProperty "eval x (add a b) == (eval x a + eval x b)"
      (\x a b -> eval x (add a b) === (eval x a + eval x b))
  , testProperty "noJunk x && noJunk y ==> noJunk (add x y)"
      (\x y -> noJunk x && noJunk y ==> noJunk (add x y))
  ]

task2 :: TestTree
task2 = testGroup "Task 2: `mul`"
  [ testProperty "No-negative exponents: noNegExp (mul expr1 expr2)"
      (\expr1 expr2 -> noNegExp (mul expr1 expr2))
  , testProperty "eval x (mul a b) == (eval x a * eval x b)"
      (\x a b -> eval x (mul a b) === (eval x a * eval x b))
  , testProperty "noJunk x && noJunk y ==> noJunk (mul x y)"
      (\x y -> noJunk x && noJunk y ==> noJunk (mul x y))
  ]

task3 :: TestTree
task3 = testGroup "Task 3: `addAll`"
  [ testProperty "No-negative exponents: noNegExp (addAll exprs)"
      (\exprs -> noNegExp (addAll exprs))
  , testProperty "sum [eval x expr | expr <- exprs] == eval x (addAll exprs)"
      (\x exprs -> sum [eval x expr | expr <- exprs] === eval x (addAll exprs))
  , testProperty "all noJunk exprs ==> noJunk (addAll exprs)"
      (\exprs -> all noJunk exprs ==> noJunk (addAll exprs))
  ]

task4 :: TestTree
task4 = testGroup "Task 4: `mulAll`"
  [ testProperty "No-negative exponents: noNegExp (mulAll exprs)"
      (\exprs -> noNegExp (mulAll exprs))
  , testProperty "product [eval x expr | expr <- exprs] == eval x (mulAll exprs)"
      (\x exprs -> product [eval x expr | expr <- exprs] === eval x (mulAll exprs))
  , testProperty "all noJunk exprs ==> noJunk (mulAll exprs)"
      (\exprs -> all noJunk exprs ==> noJunk (mulAll exprs))
  ]

task5 :: TestTree
task5 = testGroup "Task 5: `exprToPoly`"
  [ testProperty "eval x expr === evalPoly x (exprToPoly expr)"
      (\x expr -> eval x expr === evalPoly x (exprToPoly expr))
  ]

task6 :: TestTree
task6 = testGroup "Task 6: `polyToExpr`"
  [ testProperty "eval x (polyToExpr poly) === evalPoly x poly"
      (\x poly -> eval x (polyToExpr poly) === evalPoly x poly)
  , testProperty "noJunk (polyToExpr poly)"
      (\poly -> noJunk (polyToExpr poly))
  ]

task7 :: TestTree
task7 = testGroup "Task 7: `simplify`"
  [ testProperty "Same eval: eval x (simplify expr) === eval x expr"
      (\x expr -> eval x (simplify expr) === eval x expr)
  , testProperty "Idempotent: simplify expr === simplify (simplify expr)"
      (\expr -> simplify expr === simplify (simplify expr))
  , testProperty "noJunk (simplify expr)"
      (\expr -> noJunk (simplify expr))
  ]

noJunk :: Expr -> Bool
noJunk (Op MulOp (NumLit 1) b) = False
noJunk (Op MulOp a (NumLit 1)) = False
noJunk (Op MulOp (ExpX _) (ExpX _)) = False
noJunk (Op _ (NumLit 0) b) = False
noJunk (Op _ a (NumLit 0)) = False
noJunk (Op _ (NumLit _) (NumLit _)) = False
noJunk (ExpX 0) = False
noJunk (Op _ a b) = noJunk a && noJunk b
noJunk _ = True

noNegExp :: Expr -> Bool
noNegExp (ExpX n) = n >= 0
noNegExp (NumLit _) = True
noNegExp (Op _ x y) = noNegExp x && noNegExp y

instance Arbitrary Expr where
  arbitrary = sized exprGen
    where
      exprGen :: Int -> Gen Expr
      exprGen n
        | n < 0 = oneof [ numGen, expGen ]
        | otherwise = oneof [ numGen, expGen, opGen ]
        where
          numGen = NumLit <$> arbitrary
          expGen = ExpX <$> nonneg
          opGen  = Op <$> elements [AddOp, MulOp]
                      <*> exprGen (n `div` 2)
                      <*> exprGen (n `div` 2)
          nonneg :: Gen Int
          nonneg = getNonNegative <$> arbitrary

-- | Allow QuickCheck to generate random polynomials.
instance Arbitrary Poly where
  arbitrary = do
            l <- choose (1,9) -- Stick the nice looking powers
            ns <- vectorOf l arbitrary
            return $ listToPoly ns