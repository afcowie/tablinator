module Main where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as S
import Scripting.Lua

main :: IO ()
main = do
    l <- newstate
    openlibs l
    loadfile l "tests/hello.lua"
    call l 0 0

    putStrLn "--"

    getglobal l "a"

    pos <- gettop l
    print pos

    result <- istable l pos
    print result

    pushnil l

    display l pos

    close l


display :: LuaState -> Int -> IO ()
display l position = do
    more <- next l position
    when more $ do
        tostring l (-2) >>= S.putStrLn
        tostring l (-1) >>= S.putStrLn
        pop l 1
        display l position

