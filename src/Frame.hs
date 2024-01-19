{-# LANGUAGE TemplateHaskell #-}

module Frame (Frame, empty, nextOffset, getSize) where

import Control.Monad.State
import Control.Lens

newtype Frame = Frame { _fsize :: Int }

$(makeLenses ''Frame)

empty :: Frame
empty = Frame { _fsize = 0 }

intSize :: Int
intSize = 4

nextOffset :: State Frame Int
nextOffset = do
    current <- gets $ view fsize
    modify $ over fsize succ

    return $ current * intSize

getSize :: Frame -> Int
getSize fr =
    let finSize :: Int
        finSize = (* intSize) . view fsize $ fr
    in foldr (\left acc ->
            if left >= finSize then left else acc)
        (error "infinit finit")
        $ iterate (*4) 4
