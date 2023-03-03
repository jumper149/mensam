module Main (main) where

import Mensam.Application
import Mensam.Database

main :: IO ()
main = runApplicationT initDatabase
