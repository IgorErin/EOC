module Tests.Print (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ((@?=), testCase)

import Lang (toString)
import X86

cases :: [(String, String)]
cases = [
    ("let x = (- (+ 4 5)) in x",
    "    .globl main\n\
    \main:\n\
    \    pushq    %rbp\n\
    \    movq    %rsp, %rbp\n\
    \    subq    $16, %rsp\n\
    \    movq    $4, 0(%rbp)\n\
    \    addq    $5, 0(%rbp)\n\
    \    movq    $0, 8(%rbp)\n\
    \    subq    0(%rbp), 8(%rbp)\n\
    \    movq    8(%rbp), %rax\n\
    \    movq    %rax, %rdi\n\
    \    callq    print_int\n\
    \    addq    $16, %rsp\n\
    \    movq    $0, %rax\n\
    \    popq    %rbp\n\
    \    retq\n")]

tests :: TestTree
tests =
    testGroup "Print" $
    map (\ (str, expected) ->
        let x = toString str
        in testCase str $ x @?= expected)
        cases