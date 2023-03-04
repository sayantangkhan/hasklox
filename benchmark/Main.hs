module Main (main) where

import Criterion
import Criterion.Main (defaultMain)
import HaskLox qualified as H

main :: IO ()
main =
  defaultMain
    [ bgroup
        "while"
        [ bench "fib 20: while" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/while-20.lox"),
          bench "fib 40: while" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/while-40.lox"),
          bench "fib 80: while" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/while-80.lox"),
          bench "fib 160: while" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/while-160.lox")
        ],
      bgroup
        "for"
        [ bench "fib 20: for" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/for-20.lox"),
          bench "fib 40: for" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/for-40.lox"),
          bench "fib 80: for" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/for-80.lox"),
          bench "fib 160: for" $ nfIO (H.runFile "lox-scripts/benchmark-scripts/for-160.lox")
        ]
    ]
