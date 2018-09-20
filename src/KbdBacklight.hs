{-# LANGUAGE OverloadedStrings #-}

module KbdBacklight (incBrightness, decBrightness) where

import Data.Maybe
import Data.Int
import DBus
import DBus.Client

dbusName' :: BusName
dbusName' = "org.freedesktop.UPower"

dbusPath' :: ObjectPath
dbusPath' = "/org/freedesktop/UPower/KbdBacklight"

dbusInterface :: InterfaceName
dbusInterface = "org.freedesktop.UPower.KbdBacklight"

mGetMaxBrightness :: MemberName
mGetMaxBrightness = "GetMaxBrightness"

mSetBrightness :: MemberName
mSetBrightness = "SetBrightness"

mGetBrightness :: MemberName
mGetBrightness = "GetBrightness"

limit' :: Int32 -> Int32 -> Int32 -> Int32
limit' maxb minb curr
  | curr < minb = minb
  | curr > maxb = maxb
  | otherwise   = curr

toPercentage :: Int32 -> Int32 -> Int32
toPercentage maxv val = val * 100 `div` maxv

incBrightness :: Int32 -> IO ()
incBrightness brightness = do
  client <- connectSystem
  maxB <- (kbdBacklightCall client mGetMaxBrightness []) >>= toIntVal
  currB <- (kbdBacklightCall client mGetBrightness []) >>= toIntVal
  let newValB = (toPercentage maxB currB + brightness) *  maxB `div` 100
  setBrightness_ client (limit' maxB 0 newValB)

decBrightness :: Int32 -> IO ()
decBrightness brightness = do
  client <- connectSystem
  maxB <- (kbdBacklightCall client mGetMaxBrightness []) >>= toIntVal
  currB <- (kbdBacklightCall client mGetBrightness []) >>= toIntVal
  let newValB = (toPercentage maxB currB - brightness) *  maxB `div` 100

  setBrightness_ client (limit' maxB 0 newValB)

setBrightness_ :: Client -> Int32 -> IO ()
setBrightness_ client value = do
  result <- kbdBacklightCall client mSetBrightness [toVariant value]
  return ()

getMaxBrightness :: IO Int32
getMaxBrightness = do
  client <- connectSystem
  kbdBacklightCall client mGetMaxBrightness [] >>= toIntVal

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
