{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ident (
    Ident, Seed,
    initSeed,
    newTemp, newTempWith, newTempWithText
    ) where

import Data.Text (Text)

import Control.Monad.State
import Control.Lens

import Fmt ((+|), (|+))

data Ident = Ident {
    _icount :: Int,
    _iname :: Text
} deriving Eq

$(makeLenses ''Ident)

instance Ord Ident where
    compare :: Ident -> Ident -> Ordering
    compare f s =
        compare (view icount f) (view icount s)

instance Show Ident where
    show :: Ident -> String
    show ident ="("+|view iname ident|+":"+|view icount ident|+")"

newtype Seed = Seed Int deriving (Show, Eq, Enum)

initSeed :: Seed
initSeed = Seed 0

tempName :: Text
tempName = "tmp"

newSeed :: State Seed Int
newSeed = do
    Seed seed <- get
    modify succ

    return seed

newIdent :: Maybe Text -> State Seed Ident
newIdent Nothing = do
    seed <- newSeed

    return $ Ident seed tempName
newIdent (Just name) = do
    seed  <- newSeed

    return $ Ident seed name

newTemp :: State Seed Ident
newTemp = newIdent Nothing

newTempWith :: Ident -> State Seed Ident
newTempWith name =  newTempWithText $ view iname name

newTempWithText :: Text -> State Seed Ident
newTempWithText name = newIdent $ Just name
