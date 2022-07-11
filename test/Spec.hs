module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Data.Text as T
import qualified Parser
import qualified Text.Megaparsec as M

main :: IO ()
main =
    Tasty.defaultMain $
    HUnit.testCase "mainFunction" $ do
        M.parse Parser.parser "" mainFunction HUnit.@?= Right ()


mainFunction :: T.Text
mainFunction =
    "func main() {\n\
    \\tGO <- Cmds([]Cmd{\n\
    \\t\tfmtPrintln(\"What is your first name?\"),\n\
    \\t\tfmtScanln{}}).RUN\n\
    \\tfor {\n\
    \\t\tgo (<-GO)()\n\
    \\t}\n\
    \}"
