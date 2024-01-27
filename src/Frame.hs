{-# LANGUAGE TemplateHaskell #-}

module Frame (Frame, empty, nextOffset, getSize) where

import Control.Monad.State
import Control.Lens

-- frame layout
---------------
-- (-8)     | retrun adress
-- (0)      | rbp
-- start of stack allocated manually
-- (8)      | fst arg
-- ...      | ...
-- (8 * i)  | ith arg
-- end of manually allocatate stack

newtype Frame = Frame { _fsize :: Int }

$(makeLenses ''Frame)

qword :: Int
qword = 8

empty :: Frame
empty = Frame { _fsize = 0 }

nextOffset :: State Frame Int
nextOffset = do
    current <- gets $ view fsize
    modify $ over fsize (+ qword)

    -- since count start before push rbp
    return $ current + 8

------------- Alignment ----------------------

alignConst :: Int
alignConst = 16

align :: Frame -> Frame
align fr =
    let localAlign size =
            foldr (\left acc -> if left >= size then left else acc)
            (error "infinit finit")
            $ iterate (+ alignConst) 0
    -- since return address we must add qword
        alignSize = localAlign $ view fsize fr
    in Frame { _fsize = alignSize }

getSize :: Frame -> Int
getSize = view fsize . align
