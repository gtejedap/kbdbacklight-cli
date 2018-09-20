
module Main where

import System.Environment
import System.Exit
import KbdBacklight
import Data.Int

main = getArgs >>= parse

parse [op, val] = case op of
  "inc" -> incBrightness (read val :: Int32) >> exitSuccess
  "dec" -> decBrightness (read val :: Int32) >> exitSuccess
  "set" -> case val of
    "max" -> incBrightness 100 >> exitSuccess
    "min" -> decBrightness 100 >> exitSuccess
    _ -> die' $ "set COMMAND not valid: " ++ val ++ "\n" ++ usage
  _ -> die' $ "COMMAND not valid: " ++ op ++ "\n" ++ usage
parse ["-h"] = putStrLn usage >> exitSuccess
parse ["-v"] = putStrLn "0.1.0.0" >> exitSuccess
parse _ = putStrLn usage >> exitSuccess

usage = "\
\Usage:\n\
\  kbdbacklight-cli (inc|dec) <%>\n\
\  kbdbacklight-cli set (max|min)\n\
\  kbdbacklight-cli -v\n\
\  kbdbacklight-cli -h\n\
\Options:\n\
\  -h\tShow this screen\n\
\  -v\tShow version\n"

die' message = putStrLn message >> exitWith (ExitFailure 1)
