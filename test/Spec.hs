module Main (main) where

import qualified Data.Text as T
import qualified Parser
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
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
  deriving (Eq)

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
  [ ( "call RUN in pure function",
      "func f(a int) int {\n\
      \\tRUN()\n\
      \}"
    ),
    ( "call UPDATIO in pure function",
      "func f(a int) int {\n\
      \\tUPDATEIO()\n\
      \}"
    ),
    ( "write to STDOUT in a pure function",
      "func f(a int) int {\n\
      \\treturn os.stdio.Write([]byte(\"hi\"))\n\
      \}"
    ),
    ( "mutate a variable",
      "var x = 2\n\
      \x = 3\n\
      \}"
    ),
    ( "panic",
      "var x = panic(4)"
    )
  ]

rightTestCases :: [(Tasty.TestName, T.Text)]
rightTestCases =
  [ ( "asyncronous main function",
      "func main() {\n\
      \\tGO <- Cmds([]Cmd{\n\
      \\t\tfmtPrintln(\"What is your first name?\"),\n\
      \\t\tfmtScanln{}}).RUN\n\
      \\tfor {\n\
      \\t\tgo (<-GO)()\n\
      \\t}\n\
      \}"
    ),
    ( "synchronous main function",
      "func main() {\n\
      \\tfmtPrintln(\"hello\").RUN()\n\
      \}"
    ),
    ( "simple pure function",
      "func add(a, b int) int {\n\
      \\treturn a + b\n\
      \}"
    ),
    ( "multline pure function",
      "func f(x int) int {\n\
      \\tvar y = x + 1\n\
      \\treturn y * 2\n\
      \}"
    ),
    ( "simple pure method",
      "func (a int) F() int {\n\
      \\treturn a + 1\n\
      \}"
    ),
    ( "anonymous function",
      "var x = func(a int) int {\n\
      \\treturn a + 1\n\
      \}"
    ),
    ( "function with if statement",
      "func f(x int) int {\n\
      \\tif x == 0 {\n\
      \\t\treturn 0\n\
      \\t}\n\
      \\treturn 1\n\
      \}"
    ),
    ( "map literal",
      "var x = map[int]string{3: \"hi\", 4: \"chip\"}"
    ),
    ( "type declaration",
      "type A b"
    ),
    ( "string literal",
      "var x string = \"x\""
    ),
    ( "struct literal",
      "var x = a{\n\
      \\tb: 3,\n\
      \\td: 4\n\
      \}"
    ),
    ( "function call",
      "var x = y(3)"
    ),
    ( "function type",
      "type a func(int) int"
    ),
    ( "indexing into a struct member",
      "var x = a.b[0]\n"
    ),
    ( "infix operation including slice lookup",
      "var x = a + b[0]"
    ),
    ( "slice lookup on a simple name",
      "var x = b[c]"
    ),
    ( "generic loop",
      "type Increment struct {\n\
      \\tsum   int\n\
      \\ti     int\n\
      \\tslice []int\n\
      \}\n\
      \\n\
      \func sumOfSlice(slice []int) int {\n\
      \\tvar result = LOOP(\n\
      \\t\tIncrement{sum: 0, i: 0, slice: slice},\n\
      \\t\tfunc(x Increment) Increment {\n\
      \\t\t\treturn Increment{\n\
      \\t\t\t\tsum:   x.sum + x.slice[x.i],\n\
      \\t\t\t\ti:     x.i + 1,\n\
      \\t\t\t\tslice: slice,\n\
      \\t\t\t}\n\
      \\t\t},\n\
      \\t\tfunc(x Increment) bool {\n\
      \\t\t\treturn x.i != len(x.slice)\n\
      \\t\t})\n\
      \\treturn result.sum\n\
      \}\n\
      \\n\
      \func LOOP[T any](mutable T, update func(T) T, keepGoing func(T) bool) T {\n\
      \\tfor keepGoing(mutable) {\n\
      \\t\tmutable = update(mutable)\n\
      \\t}\n\
      \\treturn mutable\n\
      \}\n\
      \"
    )
  ]
