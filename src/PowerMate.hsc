{-# LANGUAGE ForeignFunctionInterface #-}
-- PowerMate bindings for Haskell.
-- Copyright (C) 2006 Evan Martin <martine@danga.com>

module PowerMate (
  getUSBName,
  searchForDevice, openDevice,
  readEvent, readEventWithSkip,
  Event(..),

  Status(..), statusInit,
  writeStatus
) where

import Foreign
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.Types
-- ioctl wants an Fd, so we use System.Posix.IO for that,
import System.Posix.Types (Fd (..))
import System.Posix.IO
-- and then System.IO for everything else.
import System.IO
import System.Environment
import Data.List (isPrefixOf, find)
import Control.Monad (filterM, liftM)
import Control.Exception (bracket)
import System.Directory (getDirectoryContents)
import Foreign.C.String (withCAString, peekCString)
import Debug.Trace (trace)
import Data.Bits (testBit)

#include <linux/input.h>

foreign import ccall "sys/ioctl.h ioctl" ioctlChar ::
  CInt -> CInt -> Ptr CChar -> IO CInt

data Status = Status {
  brightness, pulse_speed, pulse_mode :: Int,
  pulse_asleep, pulse_awake :: Bool
}
statusInit = Status 0 0 0 False False

ioctlName :: Fd -> IO String
ioctlName (Fd fd) = do
  withCAString (take 255 (repeat '\0')) $ \buf -> do
    throwErrnoIf (< 0) "ioctl" $ ioctlChar fd #{const EVIOCGNAME(255)} buf
    peekCString buf

getUSBName :: FilePath -> IO String
getUSBName filename = do
  bracket (openFd filename ReadOnly Nothing defaultFileFlags) closeFd ioctlName

searchForDevice :: IO (Maybe FilePath)
searchForDevice = do
  files <- getDirectoryContents basedir
  let goodfiles = filter ("event" `isPrefixOf`) files
  let paths = [basedir ++ "/" ++ file | file <- goodfiles]
  -- There's this: find :: (a -> Bool) -> [a] -> Maybe a
  -- but I want: (a -> m Bool) -> [a] -> m (Maybe a)
  inputs <- filterM deviceIsGood paths
  return $ case inputs of
    []    -> Nothing
    (x:_) -> Just x
  where basedir = "/dev/input"
        deviceIsGood path = do
          putStr (path ++ ": ")
          hFlush stdout
          name <- getUSBName path
          putStrLn name
          return $ nameIsGood name
        nameIsGood "Griffin PowerMate" = True
        nameIsGood _                   = False

openDevice :: FilePath -> IO Handle
openDevice file = do
  handle <- openBinaryFile file ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle

data Event = Button Bool | Rotate Int | StatusChange Status
--instance Show Event where
--  show (typ, code, value) =
decodeEvent :: (Word16, Word16, Word32) -> Maybe Event
decodeEvent (#{const EV_KEY}, _, value) = Just $ Button (value == 1)
decodeEvent (#{const EV_REL}, _, value) = Just $ Rotate (fromIntegral value)
decodeEvent (#{const EV_MSC}, _, value) = Just $ StatusChange (decodePulseLED value)
decodeEvent (0, 0, 0) = Nothing  -- where do these come from?
decodeEvent (typ, code, value) = trace ("Unhandled event: " ++ show typ ++ "," ++ show code ++ "," ++ show value) Nothing

eventSize = #{size struct input_event}

readEvent :: Handle -> IO (Maybe Event)
readEvent handle = do
  allocaBytes eventSize $ \buf -> do
    readsize <- hGetBuf handle buf eventSize
    -- putStrLn ("read " ++ show readsize ++ " bytes, wanted " ++ show size)
    -- XXX die if readsize < size...
    typ   <- #{peek struct input_event, type}  buf :: IO Word16
    code  <- #{peek struct input_event, code}  buf :: IO Word16
    value <- #{peek struct input_event, value} buf :: IO Word32
    return $ decodeEvent (typ, code, value)

readEventWithSkip :: Handle -> Maybe Event -> IO (Maybe Event)
readEventWithSkip handle prev = do
  event <- readEvent handle
  let actualevent = case event of
                      Nothing -> prev
                      _       -> event
  more <- hReady handle
  if more then readEventWithSkip handle actualevent
          else return actualevent

writeEvent :: Handle -> Word16 -> Word16 -> Word32 -> IO ()
writeEvent handle typ code value = do
  allocaBytes eventSize $ \buf -> do
    #{poke struct input_event, type}  buf typ
    #{poke struct input_event, code}  buf code
    #{poke struct input_event, value} buf value
    hPutBuf handle buf eventSize

encodePulseLED :: Status -> Word32
encodePulseLED status =
  enc_brightness .|. enc_speed .|. enc_mode .|. enc_asleep .|. enc_awake where
    enc_brightness = fromIntegral (brightness status)
    enc_speed      = fromIntegral (pulse_speed status) `shiftL` 8
    enc_mode       = fromIntegral (pulse_mode status) `shiftL` 17
    enc_asleep     = boolBit (pulse_asleep status) `shiftL` 19
    enc_awake      = boolBit (pulse_awake status) `shiftL` 20
    boolBit True  = 1
    boolBit False = 0

decodePulseLED :: Word32 -> Status
decodePulseLED word = Status { brightness=b, pulse_speed=ps, pulse_mode=pm,
                               pulse_asleep=pas, pulse_awake=paw } where
  b = fromIntegral $ word .&. 0xFF
  ps = fromIntegral $ (word `shiftR` 8) .&. 0x1FF
  pm = fromIntegral $ (word `shiftR` 17) .&. 0x3
  pas = Data.Bits.testBit word 19
  paw = Data.Bits.testBit word 20

showBinary :: Word32 -> String
showBinary word = concatMap showBit [31,30..0] where
  showBit n = if Data.Bits.testBit word n then "1" else "0"

writeStatus :: Handle -> Status -> IO ()
writeStatus handle status = writeEvent handle typ code value where
  typ   = #{const EV_MSC}
  code  = #{const MSC_PULSELED}
  value = encodePulseLED status

-- vim: set ts=2 sw=2 et ft=haskell :
