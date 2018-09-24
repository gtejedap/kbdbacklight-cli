
module Main where

import System.Environment
import System.Exit
import KbdBacklight
import Data.Int

main = getArgs >>= parse >> exitSuccess

parse ["inc", val] = setBrightness $ Inc $ parseValue val
parse ["dec", val] = setBrightness $ Dec $ parseValue val
parse ["set", val] = setBrightness $ Set $ case val of "max" -> Max
                                                       "min" -> Min
                                                       x -> Const $ parseValue val

parse [x, _] = die' $ "COMMAND not found: " ++ x ++ "\n" ++ usage
parse ["max"] = do
  val <- getMaxBrightness
  putStrLn $ show val
parse ["curr"] = do
  val <- getCurrBrightness
  putStrLn $ show val
parse ["-h"] = putStrLn usage
parse ["-v"] = putStrLn "0.1.0.0"
parse _ = putStrLn usage

parseValue val = case last val of
  '%' -> Pct  (read (init val) :: Int32)
  _ -> Val (read val :: Int32)

usage = "\
\Usage:\n\
\  kbdbacklight-cli (inc|dec) (<number>%|<number>)\n\
\  kbdbacklight-cli set (max|min|<number>|<number>%)\n\
\  kbdbacklight-cli max\n\
\  kbdbacklight-cli curr\n\
\  kbdbacklight-cli -v\n\
\  kbdbacklight-cli -h\n\
\Options:\n\
\  -h\tShow this screen\n\
\  -v\tShow version\n"

die' message = putStrLn message >> exitWith (ExitFailure 1)
