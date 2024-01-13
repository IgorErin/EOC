{-# LANGUAGE TemplateHaskell #-}

module Frame (Frame, empty, nextOffset, getSize) where

import Control.Monad.State
import Control.Lens

newtype Frame = Frame { _fsize :: Int }

$(makeLenses ''Frame)

empty :: Frame
empty = Frame { _fsize = 0 }

nextOffset :: State Frame Int
nextOffset = do
    current <- gets $ view fsize
    modify $ over fsize succ

    return $ current * 4

getSize :: Frame -> Int
getSize = (* 4) . view fsize
