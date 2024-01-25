module Unit.Inter () where

-- import Test.Tasty ( TestTree, testGroup )
-- import Test.Tasty.HUnit ((@?=), testCase)

-- import Data.ByteString.Lazy
-- import Data.Text.Lazy             as TL
-- import Data.Text.Lazy.Encoding    as TL

-- import Lang (run)

-- cases :: [(ByteString, [Int], Int)]
-- cases = [
--     ("4", [], 4),
--     ("5425", [], 5425),
--     ("let x = 4 in (+ 2 x)", [], 6),
--     ("let x = 4 in (- x)", [], -4),
--     ("(+ 3 (- 2))", [], 1)]

-- bsToString :: ByteString -> String
-- bsToString = TL.unpack . TL.decodeUtf8

-- tests :: TestTree
-- tests =
--     testGroup "Inter" $
--     Prelude.map (\ (str, input, expected) ->
--         let x = run input str in
--         testCase (bsToString str) $ x @?= expected)
--         cases
