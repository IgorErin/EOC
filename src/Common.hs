module Common (Env) where

import Control.Monad.State (State)
import Ident (Seed)

type Env = State Seed