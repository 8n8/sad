module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Data.Text as T
import qualified Parser
import qualified Text.Megaparsec as M

main :: IO ()
main =
    Tasty.defaultMain $
    Tasty.testGroup "Sad" $
    map rightTest rightTestCases


parse :: T.Text -> Either ParseError ()
parse code =
    case M.parse Parser.parser "" code of
        Left err ->
            Left $ ParseError (M.errorBundlePretty err)

        Right () ->
            Right ()


newtype ParseError
    = ParseError String
    deriving Eq


instance Show ParseError where
    show (ParseError s) =
        s


rightTest :: (Tasty.TestName, T.Text) -> Tasty.TestTree
rightTest (description, code) =
    HUnit.testCase description $ do
        parse code HUnit.@?= Right ()


rightTestCases :: [(Tasty.TestName, T.Text)]
rightTestCases =
    [ ( "asyncronous main function"
      , "func main() {\n\
        \\tGO <- Cmds([]Cmd{\n\
        \\t\tfmtPrintln(\"What is your first name?\"),\n\
        \\t\tfmtScanln{}}).RUN\n\
        \\tfor {\n\
        \\t\tgo (<-GO)()\n\
        \\t}\n\
        \}"
      )
    , ( "synchronous main function"
      , "func main() {\n\
        \\tfmtPrintln(\"hello\").RUN()\n\
        \}"
      )
    ]
