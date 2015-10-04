module Main where

import Scripting.Lua

main :: IO ()
main = do
    l <- newstate
    openlibs l
    loadfile l "tests/hello.lua"
    call l 0 0
    close l
