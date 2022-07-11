module Main (main) where

import qualified Data.ByteString as B
import qualified Data.Text.Encoding
import qualified Parser
import qualified Text.Megaparsec as M

main :: IO ()
main =
  do
    contents <- B.getContents
    case Data.Text.Encoding.decodeUtf8' contents of
      Left err ->
        putStrLn $
          mconcat $
            [ "invalid character encoding: expecting UTF8: ",
              show err
            ]
      Right raw ->
        case M.parse Parser.parser "" raw of
          Left err ->
            putStrLn (M.errorBundlePretty err)
          Right () ->
            return ()

