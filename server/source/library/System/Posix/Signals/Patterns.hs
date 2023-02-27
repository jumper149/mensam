{-# LANGUAGE PatternSynonyms #-}

module System.Posix.Signals.Patterns where

import System.Posix.Signals

-- Source: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/kill.html
-- Other signals are not defined by POSIX.

pattern SIGHUP :: Signal
pattern SIGHUP = 1

pattern SIGINT :: Signal
pattern SIGINT = 2

pattern SIGQUIT :: Signal
pattern SIGQUIT = 3

pattern SIGABRT :: Signal
pattern SIGABRT = 6

pattern SIGALRM :: Signal
pattern SIGALRM = 14

pattern SIGTERM :: Signal
pattern SIGTERM = 15
