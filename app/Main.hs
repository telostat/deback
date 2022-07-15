module Main where

import Control.Monad (join)
import Deback.Cli (parser)
import qualified Options.Applicative as OA


main :: IO ()
main = join $ OA.execParser parser
