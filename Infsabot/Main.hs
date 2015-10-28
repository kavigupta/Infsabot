module Main(createDemoBoard) where

import Infsabot.Board
import Codec.Picture
import Infsabot.Parameters

createDemoBoard :: Int -> IO ()
createDemoBoard demoBoardSize
    = writePng "./demo-starting-board.png"
        $ renderBoard
        $ startingBoard
            (defaultParameters {paramBoardSize = demoBoardSize})
            (\_ -> undefined)
