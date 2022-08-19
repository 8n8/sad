module Parser (parser) where

import qualified Data.Text as T
import qualified Data.Void
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C

-- import Text.Megaparsec.Debug (dbg)

type Parser =
  M.Parsec Data.Void.Void T.Text

parser :: Parser ()
parser =
  do
    _ <- M.some (M.choice topLevelParsers)
    M.eof
    return ()

topLevelParsers :: [Parser ()]
topLevelParsers =
  [ packageDeclaration,
    parseWhitespace,
    parseImports,
    M.try (parseAsyncMain M.<?> "asynchronous main function"),
    parseSyncMain M.<?> "synchronous main function",
    parsePredefined M.<?> "pre-defined function",
    parseTypeDeclaration,
    parseBind,
    parseNamedPureFunction
  ]

parseAnonymousFunction :: Parser ()
parseAnonymousFunction =
  do
    _ <- M.chunk "func"
    _ <- C.char '('
    _ <- M.takeWhileP Nothing (\c -> c /= ')')
    _ <- C.char ')'
    _ <- C.char ' '
    _ <-
      M.choice
        [ parseType,
          do
            _ <- C.char '('
            _ <- M.takeWhileP Nothing (\c -> c /= ')')
            _ <- C.char ')'
            return ()
        ]
    _ <- C.char ' '
    _ <- parsePureFunctionBodyBlock
    return ()

parsePureFunctionBodyBlock :: Parser ()
parsePureFunctionBodyBlock =
  do
    _ <- C.char '{'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- M.some (parsePureFunctionElement >> M.choice [parseWhitespace >> return ()])
    _ <- M.choice [parseWhitespace, return ()]
    _ <- C.char '}'
    return ()

parseNamedPureFunction :: Parser ()
parseNamedPureFunction =
  do
    _ <- M.chunk "func"
    _ <- C.char ' '
    _ <- M.choice [parseFunctionClass >> C.char ' ' >> return (), return ()]
    _ <- parseName
    _ <- C.char '('
    _ <- M.takeWhileP Nothing (\c -> c /= ')')
    _ <- C.char ')'
    _ <- C.char ' '
    _ <-
      M.choice
        [ parseType,
          do
            _ <- C.char '('
            _ <- M.takeWhileP Nothing (\c -> c /= ')')
            _ <- C.char ')'
            return ()
        ]
    _ <- C.char ' '
    _ <- parsePureFunctionBodyBlock
    return ()

parsePureFunctionElement :: Parser ()
parsePureFunctionElement =
  do
    _ <- M.choice [parseIf, parseReturn, M.try parseBind, parseMultiBind]
    return ()

parseMultiBind :: Parser ()
parseMultiBind =
  do
    _ <- M.chunk "var"
    _ <- C.char ' '
    _ <- M.some (parseCommaSeparated (parseName >> return ()) '=')
    _ <- C.char '='
    _ <- C.char ' '
    _ <- parseValue
    return ()

parseIf :: Parser ()
parseIf =
  do
    _ <- M.chunk "if"
    _ <- C.char ' '
    _ <- parseValue
    _ <- C.char ' '
    _ <- C.char '{'
    _ <- parseWhitespace
    _ <-
      M.some
        ( parsePureFunctionElement
            >> (M.choice [parseWhitespace, return ()])
        )
    _ <- C.char '}'
    return ()

parseReturn :: Parser ()
parseReturn =
  do
    _ <- M.chunk "return"
    _ <- C.char ' '
    _ <- M.choice [parseMultiValue, parseValue]
    return ()

parseMultiValue :: Parser ()
parseMultiValue =
  do
    _ <- M.some parseOneOfMultiValue
    return ()

parseOneOfMultiValue :: Parser ()
parseOneOfMultiValue =
  do
    _ <- parseValue
    _ <- M.choice [C.char ',', M.lookAhead (C.char '\n')]
    _ <- M.choice [C.char ' ' >> return (), return ()]
    return ()

parseFunctionClass :: Parser ()
parseFunctionClass =
  do
    _ <- C.char '('
    _ <- M.takeWhileP Nothing (\c -> c /= ')')
    _ <- C.char ')'
    return ()

parseBind :: Parser ()
parseBind =
  do
    _ <- M.chunk "var"
    _ <- C.char ' '
    _ <- parseName
    _ <-
      M.choice
        [ M.try $ do
            _ <- C.char ' '
            _ <- parseType
            return (),
          return ()
        ]
    _ <- C.char ' '
    _ <- C.char '='
    _ <- C.char ' '
    _ <- parseValue
    return ()

parsePredefined :: Parser ()
parsePredefined =
  do
    _ <- M.choice $ map M.chunk predefined
    return ()

predefined :: [T.Text]
predefined =
  [ "func (cmds Cmds) RUN() {\n\
    \\tfor _, cmd := range cmds {\n\
    \\t\tcmd.RUN()\n\
    \\t}\n\
    \}",
    "func (fmtScanln) RUN() {\n\
    \\tvar input string\n\
    \\tvar n, err = fmt.Scanln(&input)\n\
    \\tUPDATEIO(fmtScanlnResult{input, n, err})\n\
    \}",
    "func UPDATEIO(msg Msg) {\n\
    \\tvar cmd Cmd\n\
    \\tLOCK.Lock()\n\
    \\tMODEL, cmd = msg.update(MODEL)\n\
    \\tLOCK.Unlock()\n\
    \\tcmd.RUN()\n\
    \}",
    "func UPDATEIO(msg Msg) {\n\
    \\tvar cmd Cmd\n\
    \\tMODEL, cmd = msg.update(MODEL)\n\
    \\tcmd.RUN()\n\
    \}",
    "func (p fmtPrintln) RUN() {\n\
    \\tfmt.Println(string(p))\n\
    \}",
    "var LOCK sync.Mutex",
    "func (HTTPHANDLER) ServeHTTP(w http.ResponseWriter, r *http.Request) {\n\
    \\tdone := make(chan httpResponse, 1)\n\
    \\trequest := httpRequest{\n\
    \\t\tMethod:     r.Method,\n\
    \\t\tPath:       r.URL.Path,\n\
    \\t\tHeader:     r.Header,\n\
    \\t\tRemoteAddr: r.RemoteAddr,\n\
    \\t\tDone:       done,\n\
    \\t}\n\
    \\tUPDATEIO(request)\n\
    \\tresponse := <-done\n\
    \\tfor key, values := range response.Header {\n\
    \\t\tfor _, value := range values {\n\
    \\t\t\tw.Header().Set(key, value)\n\
    \\t\t}\n\
    \\t}\n\
    \\tw.WriteHeader(response.StatusCode)\n\
    \\t_, _ = w.Write(response.Body)\n\
    \}",
    "func (w writeHttpResponse) RUN() {\n\
    \\tGO <- func() {\n\
    \\t\tw.w.Write(w.message)\n\
    \\t\tw.done <- struct{}{}\n\
    \\t}\n\
    \}",
    "func (p println) RUN() {\n\
    \\tfmt.Println(string(p))\n\
    \}",
    "func (s startHttpServer) RUN() {\n\
    \\tsh := &http.Server{\n\
    \\t\tHandler:        HTTPHANDLER{},\n\
    \\t\tReadTimeout:    s.ReadTimeout,\n\
    \\t\tWriteTimeout:   s.WriteTimeout,\n\
    \\t\tMaxHeaderBytes: s.MaxHeaderBytes,\n\
    \\t}\n\
    \\tGO <- func() {\n\
    \\t\tUPDATEIO(crashedHttpServer{sh.ListenAndServe()})\n\
    \\t}\n\
    \}",
    "var GO = make(chan func(), 1)",
    "type Msg interface {\n\
    \\tupdate(Model) (Model, Cmd)\n\
    \}",
    "type Cmd interface {\n\
    \\tRUN()\n\
    \}",
    "func (w writeHttpResponse) RUN() {\n\
    \\tw.Done <- w.Response\n\
    \}",
    "func (r ReadFile) RUN() {\n\
    \\tcontents, err := os.ReadFile(string(r))\n\
    \\tUPDATEIO(fileContents{string(r), contents, err})\n\
    \}",
    "func (w WriteFile) RUN() {\n\
    \\terr := os.WriteFile(w.path, w.contents, 0600)\n\
    \\tUPDATEIO(fileWritten{err, w.path})\n\
    \}",
    "func (f fileWritten) update(model Model) (Model, Cmd) {\n\
    \\tif f.err != nil {\n\
    \\t\treturn model, Panic(f.err.Error())\n\
    \\t}\n\
    \\treturn model, CmdNone{}\n\
    \}",
    "func (CmdNone) RUN() {\n\
    \}",
    "func (p Panic) RUN() {\n\
    \\tpanic(p)\n\
    \}",
    "func (s startHttpServer) RUN() {\n\
    \\tsh := &http.Server{\n\
    \\t\tAddr:           s.Addr,\n\
    \\t\tHandler:        HTTPHANDLER{},\n\
    \\t\tReadTimeout:    s.ReadTimeout,\n\
    \\t\tWriteTimeout:   s.WriteTimeout,\n\
    \\t\tMaxHeaderBytes: s.MaxHeaderBytes,\n\
    \\t}\n\
    \\tGO <- func() {\n\
    \\t\tUPDATEIO(crashedHttpServer{sh.ListenAndServe()})\n\
    \\t}\n\
    \}",
    "func LOOP[T any](mutable T, update func(T) T, keepGoing func(T) bool) T {\n\
    \\tfor keepGoing(mutable) {\n\
    \\t\tmutable = update(mutable)\n\
    \\t}\n\
    \\treturn mutable\n\
    \}",
    "func INSERT[K comparable, V any](key K, value V, oldMap map[K]V) map[K]V {\n\
    \\tvar newMap = make(map[K]V)\n\
    \\tfor k, v := range oldMap {\n\
    \\t\tnewMap[k] = v\n\
    \\t}\n\
    \\tnewMap[key] = value\n\
    \\treturn newMap\n\
    \}",
    "func DELETE[K comparable, V any](key K, oldMap map[K]V) map[K]V {\n\
    \\tvar newMap = make(map[K]V)\n\
    \\tfor k, v := range oldMap {\n\
    \\t\tnewMap[k] = v\n\
    \\t}\n\
    \\tdelete(newMap, key)\n\
    \\treturn newMap\n\
    \}"
  ]

parseTypeDeclaration :: Parser ()
parseTypeDeclaration =
  do
    _ <- M.chunk "type"
    _ <- C.char ' '
    _ <- parseName
    _ <- C.char ' '
    _ <- parseType
    return ()

parseType :: Parser ()
parseType =
  M.choice
    [ parsePointerType,
      M.try $ parseFunctionType,
      M.try $ parseChanType,
      parseStructType,
      parseMapType,
      M.try $ M.choice [M.try parseImportedLookup, parseName >> return ()],
      parseSliceType
    ]

parseFunctionType :: Parser ()
parseFunctionType =
  do
    _ <- M.chunk "func"
    _ <- C.char '('
    _ <- M.takeWhileP Nothing (\c -> c /= ')')
    _ <- C.char ')'
    _ <- C.char ' '
    _ <-
      M.choice
        [ parseType,
          do
            _ <- C.char '('
            _ <- M.takeWhileP Nothing (\c -> c /= ')')
            _ <- C.char ')'
            return ()
        ]
    return ()

parseMapType :: Parser ()
parseMapType =
  do
    _ <- M.chunk "map"
    _ <- C.char '['
    _ <- parseType
    _ <- C.char ']'
    _ <- parseType
    return ()

parseChanType :: Parser ()
parseChanType =
  do
    _ <- M.chunk "chan"
    _ <- C.char ' '
    _ <- parseType
    return ()

parsePointerType :: Parser ()
parsePointerType =
  do
    _ <- C.char '*'
    _ <- parseType
    return ()

parseStructType :: Parser ()
parseStructType =
  do
    _ <- M.chunk "struct"
    _ <- M.choice [C.char ' ' >> return (), return ()]
    _ <- C.char '{'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- M.many (M.try parseStructTypeField)
    _ <- C.char '}'
    return ()

parseStructTypeField :: Parser ()
parseStructTypeField =
  do
    _ <- parseName
    _ <- M.takeWhile1P Nothing (\c -> c == ' ')
    _ <- parseType
    _ <- C.char '\n'
    _ <- M.choice [parseWhitespace, return ()]
    return ()

parseSliceType :: Parser ()
parseSliceType =
  do
    _ <- M.chunk "[]"
    _ <- parseType
    return ()

parseAsyncMain :: Parser ()
parseAsyncMain =
  do
    _ <- M.chunk "func main() {\n\t"
    _ <- M.chunk "GO <- "
    _ <- parseValue
    _ <- M.chunk ".RUN"
    _ <- C.char '\n' M.<?> "new line"
    _ <- M.chunk "\tfor {\n\t\tgo (<-GO)()\n\t}\n}"
    return ()

parseSyncMain :: Parser ()
parseSyncMain =
  do
    _ <- M.chunk "func main() {\n\t"
    _ <- parseValue
    _ <- M.chunk ".RUN()"
    _ <- C.char '\n' M.<?> "new line"
    _ <- C.char '}'
    return ()

valueParsers :: [Parser ()]
valueParsers =
  [ M.try parseInfixOperation,
    M.try parseMapLiteral,
    M.try doubleQuotedString,
    M.try slice,
    M.try parseAnonymousFunction,
    M.try parseTypeConversion,
    M.try functionCall,
    M.try parseMake,
    M.try structLiteral,
    M.try parseImportedLookup,
    M.try parseIntLiteral,
    M.try parseHex,
    M.try parseBracketedValue,
    M.try parseSliceLookup,
    M.try parseDotLookup,
    parseName >> return ()
  ]

parseSliceLookup :: Parser ()
parseSliceLookup =
  do
    _ <- M.choice [M.try parseDotLookup, parseName >> return ()]
    _ <- C.char '['
    _ <- parseValue
    _ <- C.char ']'
    return ()

parseBracketedValue :: Parser ()
parseBracketedValue =
  do
    _ <- C.char '('
    _ <- M.choice [parseWhitespace, return ()]
    _ <- parseValue
    _ <- M.choice [parseWhitespace, return ()]
    _ <- C.char ')'
    return ()

parseHex :: Parser ()
parseHex =
  do
    _ <- M.chunk "0x"
    _ <-
      M.takeWhile1P
        Nothing
        (\c -> elem c ("0123456789ABCDEF" :: [Char]))
    return ()

parseMake :: Parser ()
parseMake =
  do
    _ <- M.chunk "make"
    _ <- C.char '('
    _ <- parseType
    _ <- C.char ')'
    return ()

parseIntLiteral :: Parser ()
parseIntLiteral =
  do
    _ <- M.takeWhile1P Nothing (\c -> elem c ("0123456789" :: [Char]))
    return ()

infixValueParsers :: [Parser ()]
infixValueParsers =
  [ M.try doubleQuotedString,
    M.try slice,
    M.try functionCall,
    M.try structLiteral,
    M.try parseImportedLookup,
    M.try parseSliceLookup,
    M.try parseDotLookup,
    parseHex,
    M.try parseIntLiteral,
    parseBracketedValue,
    parseName >> return ()
  ]

parseInfixOperation :: Parser ()
parseInfixOperation =
  do
    _ <- parseInfixValue
    _ <- M.some $
      M.try $
        do
          parseWhitespace
          parseInfixOperator
          parseWhitespace
          parseInfixValue
    return ()

parseInfixValue :: Parser ()
parseInfixValue =
  do
    _ <- M.choice infixValueParsers
    return ()

parseInfixOperator :: Parser ()
parseInfixOperator =
  do
    _ <- M.choice (map M.chunk ["!=", "==", ">>", ">", "<<", "<", "<=", ">=", "+", "-", "*", "/", "&&", "&", "||", "|"])
    return ()

parseImportedLookup :: Parser ()
parseImportedLookup =
  do
    _ <- M.choice (map M.chunk pureImports)
    return ()

pureImports :: [T.Text]
pureImports =
  [ "fmt.Sprintf",
    "fmt.Errorf",
    "time.Second",
    "time.Duration",
    "http.ResponseWriter",
    "http.Request",
    "errors.Is",
    "errors.New"
  ]

parseDotLookup :: Parser ()
parseDotLookup =
  do
    name <- parseName
    if elem name approvedImports
      then fail "imported name"
      else do
        _ <- M.some $
          do
            _ <- C.char '.'
            _ <- parseName
            return ()
        return ()

structLiteral :: Parser ()
structLiteral =
  do
    _ <- M.choice [parseStructType, parseImportedLookup, parseName >> return ()]
    _ <- C.char '{'
    _ <- M.choice [parseWhitespace, return ()]
    _ <-
      M.choice
        [ M.try $ M.some $ M.try $ parseCommaSeparated (M.try structLiteralNamedMember) '}',
          M.try $ M.many $ M.try $ parseCommaSeparated (M.try parseValue) '}'
        ]
    _ <- M.choice [parseWhitespace, return ()]
    _ <- C.char '}'
    return ()

parseMapLiteral :: Parser ()
parseMapLiteral =
  do
    _ <- parseMapType
    _ <- C.char '{'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- M.try $ M.many $ M.try $ parseCommaSeparated (M.try (mapLiteralMember)) '}'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- C.char '}'
    return ()

structLiteralNamedMember :: Parser ()
structLiteralNamedMember =
  do
    _ <- parseName
    _ <- C.char ':'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- parseValue
    return ()

mapLiteralMember :: Parser ()
mapLiteralMember =
  do
    _ <- parseValue
    _ <- C.char ':'
    _ <- M.choice [parseWhitespace, return ()]
    _ <- parseValue
    return ()

slice :: Parser ()
slice =
  do
    _ <- parseType
    _ <- C.char '{'
    _ <- M.many (M.try $ parseCommaSeparated (M.try parseValue) '}')
    _ <- M.choice [parseWhitespace, return ()]
    _ <- C.char '}'
    return ()

functionCall :: Parser ()
functionCall =
  do
    _ <- M.choice [M.try parseImportedLookup, parseName >> return ()]
    _ <- C.char '('
    _ <- M.many (parseCommaSeparated parseValue ')')
    _ <- C.char ')'
    return ()

parseTypeConversion :: Parser ()
parseTypeConversion =
  do
    _ <- parseType
    _ <- C.char '('
    _ <- M.many (parseCommaSeparated parseValue ')')
    _ <- C.char ')'
    return ()

doubleQuotedString :: Parser ()
doubleQuotedString =
  do
    _ <- C.char '"'
    _ <- M.many $ M.choice [normalStringChars, specialStringChar]
    _ <- C.char '"'
    return ()

normalStringChars :: Parser ()
normalStringChars =
  do
    _ <- M.takeWhile1P Nothing isNormalStringChar
    return ()

isNormalStringChar :: Char -> Bool
isNormalStringChar ch =
  ch /= '"' && ch /= '\\'

specialStringChar :: Parser ()
specialStringChar =
  do
    _ <- C.char '\\'
    _ <- M.satisfy (\_ -> True)
    return ()

parseName :: Parser T.Text
parseName =
  do
    first <- M.satisfy isFirstNameChar
    remainder <- M.takeWhileP Nothing isSubsequentNameChar
    let name = T.cons first remainder
    if elem name forbiddenNames
      then fail "forbidden name"
      else return (T.cons first remainder)

forbiddenNames :: [T.Text]
forbiddenNames =
  [ "panic",
    "copy",
    "append",
    "delete",
    "print",
    "recover"
  ]

isSubsequentNameChar :: Char -> Bool
isSubsequentNameChar ch =
  elem ch ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789" :: [Char])

isFirstNameChar :: Char -> Bool
isFirstNameChar ch =
  elem ch ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" :: [Char])

parseCommaSeparated :: Parser () -> Char -> Parser ()
parseCommaSeparated p end =
  do
    _ <- M.choice [parseWhitespace, return ()]
    _ <- p
    _ <- M.choice [parseWhitespace, return ()]
    _ <- M.choice [C.char ',', M.lookAhead (C.char end)]
    return ()

parseValue :: Parser ()
parseValue =
  do
    _ <- M.choice valueParsers
    return ()

parseImports :: Parser ()
parseImports =
  do
    _ <- M.chunk "import"
    _ <- C.char ' '
    _ <- C.char '('
    _ <- C.char '\n'
    _ <- M.some parseImport
    _ <- C.char ')'
    return ()

parseImport :: Parser ()
parseImport =
  do
    _ <- C.char '\t'
    _ <- C.char '"'
    _ <- parseImportName
    _ <- C.char '"'
    _ <- C.char '\n'
    return ()

approvedImports :: [T.Text]
approvedImports =
  [ "fmt",
    "http",
    "net/http",
    "sync",
    "time",
    "os",
    "errors",
    "io/fs"
  ]

parseImportName :: Parser ()
parseImportName =
  do
    _ <- M.choice $ map M.chunk approvedImports
    return ()

parseWhitespace :: Parser ()
parseWhitespace =
  do
    _ <- M.some (M.choice [parseSpaceChars, parseLineComment])
    return ()

parseSpaceChars :: Parser ()
parseSpaceChars =
  do
    _ <-
      M.takeWhile1P
        (Just "space, tab or new line")
        (\ch -> elem ch [' ', '\t', '\n'])
    return ()

parseLineComment :: Parser ()
parseLineComment =
  do
    _ <- M.chunk "//"
    _ <- M.takeWhileP (Just "line comment") (\ch -> ch /= '\n')
    _ <- C.char '\n'
    return ()

packageDeclaration :: Parser ()
packageDeclaration =
  do
    _ <- M.chunk "package"
    _ <- C.char ' '
    parsePackageName

parsePackageName :: Parser ()
parsePackageName =
  do
    _ <- M.takeWhile1P (Just "lower-case character") isPackageChar
    return ()

isPackageChar :: Char -> Bool
isPackageChar ch =
  elem ch ("abcdefghijklmnopqrstuvwxyz" :: [Char])
