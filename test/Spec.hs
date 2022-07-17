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
    map rightTest rightTestCases <> map leftTest leftTestCases


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


leftTest :: (Tasty.TestName, T.Text) -> Tasty.TestTree
leftTest (description, code) =
    HUnit.testCase description $ do
        HUnit.assertBool "expecting Left" (parse code /= Right ())


leftTestCases :: [(Tasty.TestName, T.Text)]
leftTestCases =
    [ ( "call RUN in pure function"
      , "func f(a int) int {\n\
        \\tRUN()\n\
        \}"
      )
    , ( "call UPDATIO in pure function"
      , "func f(a int) int {\n\
        \\tUPDATEIO()\n\
        \}"
      )
    ]


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
    , ( "simple pure function"
      , "func add(a, b int) int {\n\
        \\treturn a + b\n\
        \}"
      )
    , ( "multline pure function"
      , "func f(x int) int {\n\
        \\tvar y = x + 1\n\
        \\treturn y * 2\n\
        \}"
      )
    , ( "simple pure method"
      , "func (a int) F() int {\n\
        \\treturn a + 1\n\
        \}"
      )
    , ( "anonymous function"
      , "var x = func(a int) int {\n\
        \\treturn a + 1\n\
        \}"
      )
    , ( "function with if statement"
      , "func f(x int) int {\n\
        \\tif x == 0 {\n\
        \\t\treturn 0\n\
        \\t}\n\
        \\treturn 1\n\
        \}"
      )
    ]
