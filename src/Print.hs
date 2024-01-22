module Print (run) where

import X86

import Fmt ((+|), (|+), (+||), (||+), Builder, pretty)
import Data.Text.Lazy (Text)

run :: Program -> Text
run (Program _ instr) =
    let instr' = instr -- TODO prelude
        body = mconcat $ map textInstrWithOffset instr'
        header = "    .global main\nmain:\n"
    in pretty $ header <> body

-- TODO move to appropriate place

textInstrWithOffset :: Instr -> Builder
textInstrWithOffset x = "    "+|textInstr x|+""

textInstr :: Instr -> Builder
textInstr (Addq sr1 ds) = "addq "+||sr1||+", "+||ds||+"\n"
textInstr (Subq sr1 ds) = "subq "+||sr1||+", "+||ds||+"\n"
textInstr (Negq arg)    = "negq "+||arg||+"\n"
textInstr (Movq sr ds)  = "movq "+||sr||+", "+||ds||+"\n"
textInstr (Pushq arg)   = "pushq "+||arg||+"\n"
textInstr (Popq arg)    = "popq "+||arg||+"\n"
textInstr (Callq lb)    = "callq "+|lb|+"\n"
textInstr Retq          = "retq "
textInstr (Jmp lb)      = "jmp "+||lb||+"\n"
