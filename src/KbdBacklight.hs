{-# LANGUAGE OverloadedStrings #-}

module KbdBacklight (getMaxBrightness, getCurrBrightness, setBrightness, Modify(Inc,Dec,Set), BConst(Max,Min,Const), BVal(Val,Pct)) where

import Data.Maybe
import Data.Int
import DBus
import DBus.Client

dbusName' = "org.freedesktop.UPower" :: BusName
dbusPath' = "/org/freedesktop/UPower/KbdBacklight" :: ObjectPath
dbusInterface = "org.freedesktop.UPower.KbdBacklight" :: InterfaceName

mGetMaxBrightness = "GetMaxBrightness" :: MemberName
mSetBrightness = "SetBrightness" :: MemberName
mGetBrightness = "GetBrightness" :: MemberName

data Modify = Inc BVal | Dec BVal | Set BConst
data BConst = Max | Min | Const BVal
data BVal = Val Int32 | Pct Int32

limit' :: Int32 -> Int32 -> Int32 -> Int32
limit' maxb minb curr
  | curr < minb = minb
  | curr > maxb = maxb
  | otherwise   = curr

toPercentage :: Int32 -> Int32 -> Int32
toPercentage maxv val = val * 100 `div` maxv

getMaxBrightness = do
  client <- connectSystem
  (kbdBacklightCall client mGetMaxBrightness []) >>= toIntVal

getCurrBrightness = do
  client <- connectSystem
  (kbdBacklightCall client mGetBrightness []) >>= toIntVal

setBrightness :: Modify -> IO ()
setBrightness cmd = do
  client <- connectSystem
  (maxB, currB) <- maxCurr client
  let newVal = case cmd of  Inc val -> modify val maxB currB (+)
                            Dec val -> modify val maxB currB (-)
                            Set val -> case val of
                              Max -> maxB
                              Min -> 0
                              Const x -> case x of
                                Pct y -> y * 255 `div` 100
                                Val y -> y
  result <- kbdBacklightCall client mSetBrightness [toVariant (limit' maxB 0 newVal)]
  return ()

modify val maxB currB op = case val of
  Pct x -> (toPercentage maxB currB `op` x) *  maxB `div` 100
  Val x -> currB `op` x

maxCurr client = do
  maxB <- (kbdBacklightCall client mGetMaxBrightness []) >>= toIntVal
  currB <- (kbdBacklightCall client mGetBrightness []) >>= toIntVal
  return (maxB, currB)

toIntVal :: MethodReturn -> IO Int32
toIntVal reply = do
  return (fromMaybe (error "No value") (fromVariant $ head (methodReturnBody reply)))

kbdBacklightCall :: Client -> MemberName -> [Variant] -> IO MethodReturn
kbdBacklightCall client method params = call_ client
      ( methodCall dbusPath' dbusInterface method )
      {
          methodCallDestination = Just dbusName'
        , methodCallBody = params
      }
