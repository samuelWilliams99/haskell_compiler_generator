{-|
Module      : ParserRequirements
Description : Library containing all the requirements for a parser
Copyright   : (c) Samuel Williams, 2021
                  Graham Hutton, 2016
License     : GPL-3
Maintainer  : samuel.will1999@gmail.com
Stability   : release

This module is copied to any generated parser and contains 5 sections of code:
  [@ParseState@] Data type and helper functions for keeping track of source position
  [@Result@] Maybe-like data type including error string for failure
  [@Parser a@] Modified @Parser@ library from Graham Huttons Programming in Haskell
  [@Scanner@] Configurable scanner/lexer taking a String and returning a list of @Token@s
  [@languageDefsParser@] A set of @Parser@s specifically for the gmr file, and any other similar meta-languages
-}
module ParserRequirements where
import Data.Char
import Data.List
import Data.Foldable
import Control.Applicative
import Control.Monad.State
-- | ParseState
data ParseState = ParseState { line   :: Int
                             , column :: Int
                             , pos    :: Int
                             , code   :: String
                             , rest   :: String
                             } deriving Show
showPos :: ParseState -> String
showPos pos = "Line " ++ (show $ line pos) ++ ", Column " ++ (show $ column pos)
               ++ "\n    " ++ lineStr pos ++ "\n   " ++ replicate (column pos) ' ' ++ "^"
parseState :: String -> ParseState
parseState code = ParseState 1 1 1 code code
inc :: ParseState -> ParseState
inc ps = if nextChar ps == '\n' then incLine ps else incColumn ps
incColumn :: ParseState -> ParseState
incColumn (ParseState line col pos code rest) = ParseState line (col+1) (pos+1) code (tail rest)
incLine :: ParseState -> ParseState
incLine (ParseState line _ pos code rest) = ParseState (line+1) 1 (pos+1) code (tail rest)
nextChar :: ParseState -> Char
nextChar ps = head $ rest ps
hasNextChar :: ParseState -> Bool
hasNextChar ps = length (rest ps) > 0
fromPos :: String -> Int -> ParseState
fromPos code pos = ParseState { code   = code
                              , pos    = clampedPos
                              , line   = (length $ filter (== '\n') prePosCode) + 1
                              , column = column
                              , rest   = drop clampedPos code }
                        where
                            clampedPos = min pos $ length code
                            prePosCode = take clampedPos code
                            column     = length $ takeWhile (/= '\n') $ reverse prePosCode
lineStr :: ParseState -> String
lineStr (ParseState _ column pos code _) = takeWhile (/= '\n') (drop (pos - column) code)

-- Result
data Result a = Result a | Error String
instance Show a => Show (Result a) where
    show (Result a)  = show a
    show (Error str) = "Error: " ++ str
instance Functor Result where
    fmap f r = case r of
        Result a -> Result $ f a
        Error e  -> Error e
instance Applicative Result where
    pure v = Result v
    pg <*> px = case pg of
        Result g -> fmap g px
        Error e  -> Error e
instance Monad Result where
    p >>= f = case p of
        Result a -> f a
        Error e  -> Error e
eitherToResult :: Either String String -> Result String
eitherToResult (Left s) = Error s
eitherToResult (Right c) = Result c

-- Parsing
newtype Parser a = P (ParseState -> (Maybe a, ParseState))
parseToResult :: Parser a -> String -> Result a
parseToResult p inp = case parse p $ parseState inp of
                          (Nothing, ps) -> err ps
                          (Just a, ps)  -> if hasNextChar ps then err ps else Result a
                        where err ps = Error $ "Malformed token at " ++ (showPos ps)
parse :: Parser a -> ParseState -> (Maybe a, ParseState)
parse (P p) s = p s
item :: Parser Char
item = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, inc s)
                else
                    (Nothing, s))
peek :: Parser Char
peek = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, s)
                else
                    (Nothing, s))
canPeek :: Parser Bool
canPeek = P (\s -> (Just $ hasNextChar s, s))
tryPeek :: Parser (Maybe Char)
tryPeek = do able <- canPeek
             if able then fmap Just peek else return Nothing
getState :: Parser ParseState
getState = P (\s -> (Just s, s))
mustFail :: Parser a -> Parser ()
mustFail p = P (\s -> case parse p s of
                          (Nothing, s') -> (Just (), s)
                          (Just _, s')  -> (Nothing, s))
instance Functor Parser where
    fmap g p = P (\s -> case parse p s of
                            (Nothing, s') -> (Nothing, s')
                            (Just v, s')  -> (Just (g v), s'))
instance Applicative Parser where
    pure v = P (\s -> (Just v, s))
    pg <*> px = P (\s -> case parse pg s of
                             (Nothing, s') -> (Nothing, s')
                             (Just g, s')  -> parse (fmap g px) s')
instance Monad Parser where
    p >>= f = P (\s -> case parse p s of
                           (Nothing, s') -> (Nothing, s)
                           (Just v, s')  -> parse (f v) s')
instance Alternative Parser where
    empty = P (\s -> (Nothing, s))
    p <|> q = P (\s -> case parse p s of
                           (Nothing, _) -> parse q s
                           (Just v, s') -> (Just v, s'))
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty
digit :: Parser Char
digit = sat isDigit
lower :: Parser Char
lower = sat isLower
upper :: Parser Char
upper = sat isUpper
letter :: Parser Char
letter = sat isAlpha
alphanum :: Parser Char
alphanum = sat isAlphaNum
char :: Char -> Parser Char
char x = sat (== x)
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
anyString :: [String] -> Parser String
anyString xs = asum $ map string xs
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)
upperIdent :: Parser String
upperIdent = do x  <- upper
                xs <- many alphanum
                return (x:xs)
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat
float :: Parser Float
float = do n <- int
           char '.'
           d <- some digit
           return $ read $ show n ++ '.':d
         <|> fmap fromIntegral int

-- Scanner
data Scanner = Scanner
    { separateCasedIdentifiers :: Bool
    , ignoreWhitespace :: Bool
    , ignoreComments :: Bool
    , operators :: [String]
    , keywords :: [String]
    , blockComment :: Maybe (String, String)
    , lineComment :: Maybe String
    , includeEOF :: Bool
    , parserMap :: ([Parser TokenType] -> [Parser TokenType])
    }
instance Show Scanner where
    show (Scanner sep whsp cmts ops kwds block line eof _) =
        "Scanner {separateCasedIdentifiers = " ++ show sep ++
        ", ignoreWhitespace = " ++ show whsp ++
        ", ignoreComments = " ++ show cmts ++
        ", operators = " ++ show ops ++
        ", keywords = " ++ show kwds ++
        ", blockComment = " ++ show block ++
        ", lineComment = " ++ show line ++
        ", includeEOF = " ++ show eof ++ "}"
data Token = Token ParseState TokenType
instance Show Token where
    show (Token _ tt) = show tt
data TokenType = TokenIdentifier String
               | TokenUpperIdentifier String
               | TokenOperator String
               | TokenStringLit String
               | TokenIntLit Int
               | TokenFloatLit Float
               | TokenKeyword String
               | TokenWhitespace Char
               | TokenBlockComment String
               | TokenLineComment String
               | TokenCustom String String
               | TokenOpenParen  | TokenCloseParen
               | TokenOpenSquare | TokenCloseSquare
               | TokenOpenCurly  | TokenCloseCurly
               | TokenEOF
               deriving (Show)
scanner = Scanner{ separateCasedIdentifiers=True
                 , ignoreWhitespace=True
                 , ignoreComments=True
                 , operators=["+", "-", "*", "/", "="]
                 , keywords=["for", "if"]
                 , blockComment=Just ("/*", "*/")
                 , lineComment=Just "//"
                 , includeEOF=False
                 , parserMap=id
                 }
space :: Parser TokenType
space = fmap TokenWhitespace $ sat isSpace
tokenWithState :: Parser TokenType -> Parser Token
tokenWithState p = do
    ps <- getState
    t <- p
    return $ Token ps t
asTokenComment :: Scanner -> Token -> [Token]
asTokenComment conf t@(Token _ (TokenBlockComment _)) = if ignoreComments conf then [] else [t]
asTokenComment conf t@(Token _ (TokenLineComment _))  = if ignoreComments conf then [] else [t]
asTokenComment conf t                                 = [t]
asToken :: Scanner -> Parser TokenType -> Parser [Token]
asToken conf p = do
    preSpaces <- many $ tokenWithState space
    token <- tokenWithState p
    postSpaces <- many $ tokenWithState space
    let tokens = asTokenComment conf token

    if ignoreWhitespace conf then
        return tokens
    else
        return $ preSpaces ++ tokens ++ postSpaces
formattedString :: Parser String
formattedString = do
    quote <- char '\'' <|> char '"'
    str <- many $ (char '\\' >> item)
                   <|> sat (/= quote)
    char quote

    return str
formattedStringRaw :: Parser String
formattedStringRaw = do
    quote <- char '\'' <|> char '"'
    str <- fmap concat $ many $
        fmap (\x -> ['\\', x]) (char '\\' >> item) <|>
        fmap pure (sat (/= quote))
    char quote

    return $ quote:str ++ [quote]
backLengthSorter = sortOn $ negate . length
operator :: Scanner -> Parser TokenType
operator conf = fmap TokenOperator $ anyString $ backLengthSorter $ operators conf
keyword :: Scanner -> Parser TokenType
keyword conf = fmap TokenKeyword $ anyString $ backLengthSorter $ keywords conf
getCharParser :: Char -> TokenType -> Parser TokenType
getCharParser c t = char c >> return t
nonAlphaNext :: Parser ()
nonAlphaNext = do
    next <- tryPeek
    case next of
        Just v  -> if isAlpha v then empty else return ()
        Nothing -> return ()
stringToken :: Parser TokenType
stringToken = do
    v <- formattedString
    return $ TokenStringLit v
intToken :: Parser TokenType
intToken = do
    v <- int
    nonAlphaNext
    return $ TokenIntLit v
floatToken :: Parser TokenType
floatToken = do
    v <- float
    nonAlphaNext
    return $ TokenFloatLit v
lowerIdentifierToken :: Parser TokenType
lowerIdentifierToken = do
    id <- ident
    return $ TokenIdentifier id
upperIdentifierToken :: Parser TokenType
upperIdentifierToken = do
    id <- upperIdent
    return $ TokenUpperIdentifier id
identifierToken :: Parser TokenType
identifierToken = do
    id <- ident <|> upperIdent
    return $ TokenIdentifier id
brackets :: Parser TokenType
brackets = getCharParser '(' TokenOpenParen  <|>
           getCharParser ')' TokenCloseParen <|>
           getCharParser '{' TokenOpenCurly  <|>
           getCharParser '}' TokenCloseCurly <|>
           getCharParser '[' TokenOpenSquare <|>
           getCharParser ']' TokenCloseSquare
getIdentifierTokenParser :: Bool -> Parser TokenType
getIdentifierTokenParser isSeperate = if isSeperate then lowerIdentifierToken <|> upperIdentifierToken else identifierToken
commentParser :: String -> String -> (String -> TokenType) -> Parser TokenType
commentParser openStr closeStr const = do
        string openStr
        commentBody <- many $ mustFail (string closeStr) >> item
        nextToken <- tryPeek
        if nextToken /= Nothing then string closeStr
        else return empty

        return $ const commentBody
blockCommentParser :: Scanner -> Parser TokenType
blockCommentParser conf = case blockComment conf of
    Nothing -> empty
    Just (openStr, closeStr) -> commentParser openStr closeStr TokenBlockComment
lineCommentParser :: Scanner -> Parser TokenType
lineCommentParser conf = case lineComment conf of
    Nothing -> empty
    Just str -> commentParser str "\n" TokenLineComment
scanParser :: Scanner -> Parser [Token]
scanParser conf = fmap concat $ many $ asum tokenedParserList
  where
    parserList = [ stringToken
                 , blockCommentParser conf
                 , lineCommentParser conf
                 , operator conf
                 , brackets
                 , keyword conf
                 , getIdentifierTokenParser $ separateCasedIdentifiers conf
                 , intToken
                 , floatToken ]
    modifiedParserList = parserMap conf $ parserList
    tokenedParserList = map (asToken conf) modifiedParserList
scan :: Scanner -> String -> Result [Token]
scan conf str = do
    tokens <- parseToResult (scanParser conf) str
    if includeEOF conf then
        return $ tokens ++ [Token (fromPos str $ length str) TokenEOF]
    else
        return tokens

-- language defs parser
unpackCustom :: TokenType -> String
unpackCustom (TokenCustom _ str) = str
unpackCustom _ = ""
nonCurly :: Parser String
nonCurly = do
    c <- item
    if (c == '}') || (c == '{') then
        empty
    else
        return [c]
nonCurlyStr :: Parser String
nonCurlyStr = fmap concat $ many $ (fmap (:[]) $ char '\'') <|> formattedStringRaw <|> nonCurly
innerCodeBlock :: Parser String
innerCodeBlock = fmap unpackCustom (codeBlock True) <|> return ""
codeBlockAux :: Parser String
codeBlockAux = do
    preStr <- nonCurlyStr
    innerBlock <- innerCodeBlock
    let out = preStr ++ innerBlock
    if out == "" then empty else return out
withBraces :: Bool -> String -> String
withBraces True str = "{" ++ str ++ "}"
withBraces False str = str
codeBlock :: Bool -> Parser TokenType
codeBlock includeBraces = do
    char '{'
    str <- fmap concat $ many codeBlockAux
    postStr <- nonCurlyStr
    char '}'
    return $ TokenCustom "CodeBlock" $ withBraces includeBraces $ str ++ postStr
directive :: Parser TokenType
directive = do
    char '%'
    str <- ident
    return $ TokenCustom "Directive" str
identifierPrime :: Parser TokenType
identifierPrime = do
    name <- ident
    primes <- some (char '\'')
    return $ TokenCustom "IdentifierPrime" (name ++ primes)
languageDefsParser :: Parser TokenType
languageDefsParser = codeBlock False <|> directive <|> identifierPrime
