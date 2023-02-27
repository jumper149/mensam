module Homepage where

import Homepage.Application
import Homepage.Server

main :: IO ()
main = runApplicationT server
