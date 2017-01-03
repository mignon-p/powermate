{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : PowerMate
Description : PowerMate bindings for Haskell.
Copyright   : (C) 2006 Evan Martin <martine@danga.com>,
              (C) 2017 Patrick Pelletier <code@funwithsoftware.org>
License     : BSD3
Maintainer  : Patrick Pelletier <code@funwithsoftware.org>
Stability   : experimental
Portability : Linux

This module is for interfacing the Griffin PowerMate USB (a
big silver knob you can turn and click) with Haskell.  You can
read events from the PowerMate, and control the brightness,
pulse speed, and pulse waveform of the built-in blue LED.
-}

module PowerMate (
  PowerMate,
  getUSBName,
  searchForDevice, openDevice,
  readEvent, readEventWithSkip,
  Event(..),

  Status(..), statusInit,
  writeStatus,
  closeDevice
) where

import Foreign
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.Types
-- ioctl wants an Fd, so we use System.Posix.IO for that,
import System.Posix.Types (Fd (..))
import System.Posix.IO
-- and then System.IO for everything else.
import System.IO
import Data.List (isPrefixOf)
import Control.Monad (filterM)
import Control.Exception (bracket)
import System.Directory (getDirectoryContents)
import Foreign.C.String (withCAString, peekCString)
import Debug.Trace (trace)
import Data.Bits (testBit)

#include <linux/input.h>

foreign import ccall "sys/ioctl.h ioctl" ioctlChar ::
  CInt -> CInt -> Ptr CChar -> IO CInt

-- | Represents a PowerMate USB controller.
data PowerMate =
  PowerMate { readHandle :: Handle, writeHandle :: Handle }
  deriving (Eq, Show)

-- | Represents the status of the blue LED.
data Status = Status {
  brightness :: Int,    -- ^ Range: 0-255.  0 = off, 255 = max
  pulse_speed :: Int,   -- ^ Range: 0-510.  0 = slowest, 255 = typical, 510 = fastest
  pulse_mode :: Int,    -- ^ Range: 0-2.  Each possible value is a different shape of pulse
  pulse_asleep :: Bool, -- ^ Not sure.
  pulse_awake :: Bool   -- ^ Should the LED pulse or be constant?
} deriving (Eq, Ord, Show, Read)

-- | A 'Status' initialized to default values.
--   (Specifically, all zero.)
statusInit :: Status
statusInit = Status 0 0 0 False False

ioctlName :: Fd -> IO String
ioctlName (Fd fd) = do
  withCAString (take 255 (repeat '\0')) $ \buf -> do
    throwErrnoIf (< 0) "ioctl" $ ioctlChar fd #{const EVIOCGNAME(255)} buf
    peekCString buf

-- | Given the name of a device file, return the name of the USB
--   device associated with it.
getUSBName :: FilePath -> IO String
getUSBName filename = do
  bracket (openFd filename ReadOnly Nothing defaultFileFlags) closeFd ioctlName

-- | Returns the name of the device file associated with the
--   Griffin PowerMate USB, or 'Nothing' if no PowerMate can be found.
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

-- | Given the name of the device file for the PowerMate USB,
--   opens it and returns a 'PowerMate'.
openDevice :: FilePath -> IO PowerMate
openDevice file = do
  rHandle <- openBinaryFile file ReadMode
  hSetBuffering rHandle NoBuffering
  wHandle <- openBinaryFile file WriteMode
  hSetBuffering wHandle NoBuffering
  return $ PowerMate { readHandle = rHandle, writeHandle = wHandle }

-- | An event returned by the PowerMate USB.
data Event = Button Bool          -- ^ True = press, False = release
           | Rotate Int           -- ^ Positive is clockwise, negative is counterclockwise
           | StatusChange Status  -- ^ When you change the LED status, it is echoed back to you for some reason
           deriving (Eq, Ord, Show, Read)

decodeEvent :: (Word16, Word16, Word32) -> Maybe Event
decodeEvent (#{const EV_KEY}, _, value) = Just $ Button (value == 1)
decodeEvent (#{const EV_REL}, _, value) = Just $ Rotate (fromIntegral value)
decodeEvent (#{const EV_MSC}, _, value) = Just $ StatusChange (decodePulseLED value)
decodeEvent (0, 0, 0) = Nothing  -- where do these come from?
decodeEvent (typ, code, value) = trace ("Unhandled event: " ++ show typ ++ "," ++ show code ++ "," ++ show value) Nothing

eventSize :: Int
eventSize = #{size struct input_event}

-- | Block until the PowerMate USB controller generates an event, and
--   then return that event.  (Or, sometimes just returns 'Nothing',
--   which you can ignore.)
readEvent :: PowerMate -> IO (Maybe Event)
readEvent handle = do
  allocaBytes eventSize $ \buf -> do
    readsize <- hGetBuf (readHandle handle) buf eventSize
    -- putStrLn ("read " ++ show readsize ++ " bytes, wanted " ++ show size)
    -- XXX die if readsize < size...
    typ   <- #{peek struct input_event, type}  buf :: IO Word16
    code  <- #{peek struct input_event, code}  buf :: IO Word16
    value <- #{peek struct input_event, value} buf :: IO Word32
    return $ decodeEvent (typ, code, value)

-- | If multiple events are available, discard all but the last.
readEventWithSkip :: PowerMate -> Maybe Event -> IO (Maybe Event)
readEventWithSkip handle prev = do
  event <- readEvent handle
  let actualevent = case event of
                      Nothing -> prev
                      _       -> event
  more <- hReady (readHandle handle)
  if more then readEventWithSkip handle actualevent
          else return actualevent

writeEvent :: PowerMate -> Word16 -> Word16 -> Word32 -> IO ()
writeEvent handle typ code value = do
  allocaBytes eventSize $ \buf -> do
    #{poke struct input_event, type}  buf typ
    #{poke struct input_event, code}  buf code
    #{poke struct input_event, value} buf value
    hPutBuf (writeHandle handle) buf eventSize

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

{-
showBinary :: Word32 -> String
showBinary word = concatMap showBit [31,30..0] where
  showBit n = if Data.Bits.testBit word n then "1" else "0"
-}

-- | Control the blue LED on the PowerMate USB.
writeStatus :: PowerMate -> Status -> IO ()
writeStatus handle status = writeEvent handle typ code value where
  typ   = #{const EV_MSC}
  code  = #{const MSC_PULSELED}
  value = encodePulseLED status

-- | Close the 'PowerMate'.
closeDevice :: PowerMate -> IO ()
closeDevice pmate = do
  hClose (readHandle pmate)
  hClose (writeHandle pmate)

-- vim: set ts=2 sw=2 et ft=haskell :
