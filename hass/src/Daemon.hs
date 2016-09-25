{-|
Module      : Daemon
Description : \-\- A simple helper daemon.
Portability : POSIX

A simple dedicated daemon used to efficiently manage password-to-clipboard selections.
After start it waits a number of seconds (45 by default) and simply sets the clipboard to an empty string.
-}
module Daemon (setClipboardAndDaemonize
             , daemonAliveAtClip
             , startDaemon
              ) where

import System.Posix.Daemon
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.Hclip (setClipboard)


-- |Copy a string to the clipboard, starting a daemon on the way to ensure the selection gets erased after a certain delay.
setClipboardAndDaemonize :: String -> IO ()
setClipboardAndDaemonize str = daemonAliveAtClip >> setClipboard str >> startDaemon

-- |To be used before setting the clipboard when __hass__ is invoked with @--clip@.
daemonAliveAtClip :: IO ()
daemonAliveAtClip = do
  alive <- isRunning hd
  when alive $ killAndWait hd

-- |Start our daemon.
startDaemon :: IO ()
startDaemon = runDetached (Just hd) DevNull cleanClipboard

-- Daemon's task.
cleanClipboard :: IO ()
cleanClipboard = threadDelay waittime >> setClipboard ""

-------- Configuration constants ---------

-- The string to be used as a daemon's pidfile.
hd = ".hassdaemon.pid"

-- The default number of seconds to wait.
waittime = 45 * 10^6
