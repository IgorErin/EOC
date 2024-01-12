module Flatten where

import qualified R1 as R
import qualified C0 as C

-- fetch :: R.Expr -> ([C.Ident], [C.Stmt], C.Expr)
-- fetch (R.EInt n) = ([], [], C.EArg $ C.AInt n)
-- fetch R.ERead = ([], [], C.ERead)
-- fetch (R.ESub expr) =
--     let (vars, stms, expr') = fetch expr

--     in
-- fetch _ = undefined

run :: R.Program -> C.Program
run = undefined

runExpr = undefined