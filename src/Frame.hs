{-# LANGUAGE TemplateHaskell #-}

module Frame (Frame, empty, nextOffset, getSize) where

import Control.Monad.State
import Control.Lens

-- frame layout
---------------
-- (-8)     | retrun adress
-- (0)      | rbp
-- (8)      | fst arg
-- ...      | ...
-- (8 * i)  | ith arg

newtype Frame = Frame { _fsize :: Int }

$(makeLenses ''Frame)

qword :: Int
qword = 8

initOffset :: Int
initOffset = qword

empty :: Frame
empty = Frame { _fsize = initOffset }

nextOffset :: State Frame Int
nextOffset = do
    current <- gets $ view fsize
    modify $ over fsize (+ qword)

    return current

------------- Alignment ----------------------

alignConst :: Int
alignConst = 16

align :: Frame -> Frame
align fr =
    let localAlign size =
            foldr (\left acc ->
                if left >= size then left else acc)
            (error "infinit finit")
            $ 0 : iterate (+ alignConst) alignConst
    -- since return address we must add qword
        alignSize = localAlign (view fsize fr + qword)
    in Frame { _fsize = alignSize }

getSize :: Frame -> Int
getSize = view fsize . align
