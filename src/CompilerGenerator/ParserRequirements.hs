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
module CompilerGenerator.ParserRequirements (
-- * ParseState
ParseState (..),
showPos,
parseState,
inc,
incColumn,
incLine,
nextChar,
hasNextChar,
fromPos,
lineStr,
-- * Result
Result (..),
eitherToResult,
-- * Parser
Parser (..),
parseToResult,
parse,
item,
peek,
canPeek,
tryPeek,
getState,
mustFail,
sat,
digit,
lower,
upper,
letter,
alphanum,
char,
string,
anyString,
ident,
upperIdent,
nat,
int,
float,
-- * Scanner
Scanner (..),
Token (..),
TokenType (..),
scanner,
space,
tokenWithState,
asToken,
formattedString,
formattedStringRaw,
operator,
keyword,
getCharParser,
nonAlphaNext,
stringToken,
intToken,
floatToken,
lowerIdentifierToken,
upperIdentifierToken,
identifierToken,
brackets,
getIdentifierTokenParser,
commentParser,
blockCommentParser,
lineCommentParser,
scanParser,
scan,
-- * Language definition parsers
unpackCustom,
codeBlock,
directive,
identifierPrime,
languageDefsParser
) where
import Data.Char
import Data.List
import Data.Foldable
import Control.Applicative
import Control.Monad.State
-- | Data type for storing the current parse position within the input code. Used in the Parser library.
data ParseState = ParseState { line   :: Int
                             , column :: Int
                             , pos    :: Int
                             , code   :: String
                             , rest   :: String
                             } deriving Show
{-|
Generates a human readable parse position string, as shown below:

@
Line 1, Column 14
     function text#()
                  ^
@
-}
showPos :: ParseState -> String
showPos pos = "Line " ++ (show $ line pos) ++ ", Column " ++ (show $ column pos)
               ++ "\n    " ++ lineStr pos ++ "\n   " ++ replicate (column pos) ' ' ++ "^"
-- | Builds a starting @ParseState@ from an input string
parseState :: String -> ParseState
parseState code = ParseState 1 1 1 code code
-- | Increments a ParseState by looking at the next character, and adjusting the line/column numbers accordingly
inc :: ParseState -> ParseState
inc ps = if nextChar ps == '\n' then incLine ps else incColumn ps
-- | Increases the column number and global position of the @ParseState@, consumes a character
incColumn :: ParseState -> ParseState
incColumn (ParseState line col pos code rest) = ParseState line (col+1) (pos+1) code (tail rest)
-- | Increases the line number and global position, and resets the column number of a @ParseState@. Consumes a character.
incLine :: ParseState -> ParseState
incLine (ParseState line _ pos code rest) = ParseState (line+1) 1 (pos+1) code (tail rest)
-- | Gets the next character of the ParseState
nextChar :: ParseState -> Char
nextChar ps = head $ rest ps
-- | Checks if the ParseState has another character.
hasNextChar :: ParseState -> Bool
hasNextChar ps = length (rest ps) > 0
-- | Builds a parse state from an input string and global position, automatically working out the line and column numbers
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
-- | Gets the current full line of the @ParseState@
lineStr :: ParseState -> String
lineStr (ParseState _ column pos code _) = takeWhile (/= '\n') (drop (pos - column) code)

-- | Result data type, similar to Maybe (with definitions for Monad) but with an error string
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
-- | Converts an @Either String a@ computation to a @Result a@
eitherToResult :: Either String a -> Result a
eitherToResult (Left s) = Error s
eitherToResult (Right c) = Result c

-- Parsing
-- | Parser data type, which transforms a @ParseState@ to another @ParseState@, with the possibility to fail on the output
newtype Parser a = P (ParseState -> (Maybe a, ParseState))
-- | Takes a @Parser@ and String, and returns a @Result@ from the @Parser@. Fails if the @Parser@ fails to return a value, or if the input is not fully used up.
-- The @ParseState@ will be used to give meaningful errors on failure.
parseToResult :: Parser a -> String -> Result a
parseToResult p inp = case parse p $ parseState inp of
                          (Nothing, ps) -> err ps
                          (Just a, ps)  -> if hasNextChar ps then err ps else Result a
                        where err ps = Error $ "Malformed token at " ++ (showPos ps)
-- | Simply runs a @Parser@ on an input
parse :: Parser a -> ParseState -> (Maybe a, ParseState)
parse (P p) s = p s
-- | Gets the next character, or fails if there aren't any
item :: Parser Char
item = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, inc s)
                else
                    (Nothing, s))
-- | Gets the next character without consuming it from the input, or fails if there isn't one
peek :: Parser Char
peek = P (\s -> if hasNextChar s then
                    (Just $ nextChar s, s)
                else
                    (Nothing, s))
-- | Checks if the @ParseState@ has a next input, as a @Parser@ computation
canPeek :: Parser Bool
canPeek = P (\s -> (Just $ hasNextChar s, s))
-- | Attempts to peek, returning a @Just Char@ if successful, or @Nothing@ otherwise. This cannot fail.
tryPeek :: Parser (Maybe Char)
tryPeek = do able <- canPeek
             if able then fmap Just peek else return Nothing
-- | Gets the current @ParseState@
getState :: Parser ParseState
getState = P (\s -> (Just s, s))
-- | Enforces a @Parser@ to fail, essentially reversing the default failure behaviour.
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
-- | Ensures the next character satisfying a predicate, returning the character if it does.
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty
-- | Gets a digit
digit :: Parser Char
digit = sat isDigit
-- | Gets a lower-case character
lower :: Parser Char
lower = sat isLower
-- | Gets an upper-case character
upper :: Parser Char
upper = sat isUpper
-- | Gets a letter as defined by @isAlpha@
letter :: Parser Char
letter = sat isAlpha
-- | Gets an alphanumeric character
alphanum :: Parser Char
alphanum = sat isAlphaNum
-- | Attempts to read a specific character
char :: Char -> Parser Char
char x = sat (== x)
-- | Attempts to read a specific string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)
-- | Attempts to read any of a list of strings
anyString :: [String] -> Parser String
anyString xs = asum $ map string xs
-- | Attempts to read an identity, as defined by an alphanumeric string starting with a lower-case letter
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)
-- | Attempts to read an upper-identity, as defined by an alphanumeric string starting with a upper-case letter
upperIdent :: Parser String
upperIdent = do x  <- upper
                xs <- many alphanum
                return (x:xs)
-- | Attempts to read a natural number
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)
-- | Attempts to read an integer value
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat
-- | Attempts to read a float value, which can also interpret integers
float :: Parser Float
float = do n <- int
           char '.'
           d <- some digit
           return $ read $ show n ++ '.':d
         <|> fmap fromIntegral int

-- Scanner
-- | Scanner data type containing the configuration for a scanner
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
-- | Instance for show which does not include the @parserMap@ field, as it is not showable
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
-- | Token data type storing the token itself and the ParseState it was taken from
data Token = Token ParseState TokenType
instance Show Token where
    show (Token _ tt) = show tt
-- | Various tokens for the scanner, including a TokenCustom constructor for any extras.
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
-- | Default definition of Scanner, for record modification
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
-- | Matches a single space, and wraps the result in a @TokenWhiteSpace@
space :: Parser TokenType
space = fmap TokenWhitespace $ sat isSpace
-- | Takes a @TokenType@ parser, gets the current @ParseState@, and returns an equivalent @Token@ parser
tokenWithState :: Parser TokenType -> Parser Token
tokenWithState p = do
    ps <- getState
    t <- p
    return $ Token ps t
-- | Throws away @TokenLineComment@ and @TokenBlockComment@ based on the @Scanner@ configuration
asTokenComment :: Scanner -> Token -> [Token]
asTokenComment conf t@(Token _ (TokenBlockComment _)) = if ignoreComments conf then [] else [t]
asTokenComment conf t@(Token _ (TokenLineComment _))  = if ignoreComments conf then [] else [t]
asTokenComment conf t                                 = [t]
-- | Allows a token to be preceeded or proceeded by any amount of spaces, keeping them if the @Scanner@ configuration says to.
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
-- | Reads a string literal, defined as any amount of tokens between two quotes, allowing them to be escaped with a \ if needed.
formattedString :: Parser String
formattedString = do
    quote <- char '\'' <|> char '"'
    str <- many $ (char '\\' >> item)
                   <|> sat (/= quote)
    char quote

    return str
-- | Similar to formattedString, but keeps the quotes and escape characters in the outputted string
formattedStringRaw :: Parser String
formattedStringRaw = do
    quote <- char '\'' <|> char '"'
    str <- fmap concat $ many $
        fmap (\x -> ['\\', x]) (char '\\' >> item) <|>
        fmap pure (sat (/= quote))
    char quote

    return $ quote:str ++ [quote]
-- | Sorts a list by the length of the elements of the list, starting with the longest
backLengthSorter :: [[a]] -> [[a]]
backLengthSorter = sortOn $ negate . length
-- | Creates a @Parser@ for operators from the @Scanner@ configuration
operator :: Scanner -> Parser TokenType
operator conf = fmap TokenOperator $ anyString $ backLengthSorter $ operators conf
-- | Creates a @Parser@ for keywords from the @Scanner@ configuration
keyword :: Scanner -> Parser TokenType
keyword conf = fmap TokenKeyword $ anyString $ backLengthSorter $ keywords conf
-- | Reads a character and returns a specific TokenType on read
getCharParser :: Char -> TokenType -> Parser TokenType
getCharParser c t = char c >> return t
-- | Ensures the next character is not alpha
nonAlphaNext :: Parser ()
nonAlphaNext = do
    next <- tryPeek
    case next of
        Just v  -> if isAlpha v then empty else return ()
        Nothing -> return ()
-- | Parser for string literals, wrapped in @TokenStringLit@
stringToken :: Parser TokenType
stringToken = do
    v <- formattedString
    return $ TokenStringLit v
-- | Parser for integers, wrapped in @TokenIntLit@
intToken :: Parser TokenType
intToken = do
    v <- int
    nonAlphaNext
    return $ TokenIntLit v
-- | Parser for floats, wrapped in @TokenFloatLit@
floatToken :: Parser TokenType
floatToken = do
    v <- float
    nonAlphaNext
    return $ TokenFloatLit v
-- | Parser for lower identifiers, wrapped in @TokenIdentifier@
lowerIdentifierToken :: Parser TokenType
lowerIdentifierToken = do
    id <- ident
    return $ TokenIdentifier id
-- | Parser for upper identifiers, wrapped in @TokenUpperIdentifier@
upperIdentifierToken :: Parser TokenType
upperIdentifierToken = do
    id <- upperIdent
    return $ TokenUpperIdentifier id
-- | Parser for both identifiers and upper identifiers, both wrapped in @TokenIdentifier@
identifierToken :: Parser TokenType
identifierToken = do
    id <- ident <|> upperIdent
    return $ TokenIdentifier id
-- | Parsers for all of the 6 brackets: @[] {} ()@
brackets :: Parser TokenType
brackets = getCharParser '(' TokenOpenParen  <|>
           getCharParser ')' TokenCloseParen <|>
           getCharParser '{' TokenOpenCurly  <|>
           getCharParser '}' TokenCloseCurly <|>
           getCharParser '[' TokenOpenSquare <|>
           getCharParser ']' TokenCloseSquare
-- | Gets parsers for identifiers, either separated by upper and lower identifiers, or both treated as lower identifiers, based on the boolean input
getIdentifierTokenParser :: Bool -> Parser TokenType
getIdentifierTokenParser isSeperate = if isSeperate then lowerIdentifierToken <|> upperIdentifierToken else identifierToken
-- | Parser for comments taking a starting and ending token, along with a constructor.
commentParser :: String -> String -> (String -> TokenType) -> Parser TokenType
commentParser openStr closeStr const = do
        string openStr
        commentBody <- many $ mustFail (string closeStr) >> item
        nextToken <- tryPeek
        if nextToken /= Nothing then string closeStr
        else return empty

        return $ const commentBody
-- | Parser for block comments based on @Scanner@ configuration
blockCommentParser :: Scanner -> Parser TokenType
blockCommentParser conf = case blockComment conf of
    Nothing -> empty
    Just (openStr, closeStr) -> commentParser openStr closeStr TokenBlockComment
-- | Parser for line comments, using the @Scanner@ for the starting token, and a new line for the ending token.
lineCommentParser :: Scanner -> Parser TokenType
lineCommentParser conf = case lineComment conf of
    Nothing -> empty
    Just str -> commentParser str "\n" TokenLineComment
-- | Creates a full scanner from a @Scanner@ configuration
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
-- | Runs a Scanner on a string, returning a @Result@ of the token list
scan :: Scanner -> String -> Result [Token]
scan conf str = do
    tokens <- parseToResult (scanParser conf) str
    if includeEOF conf then
        return $ tokens ++ [Token (fromPos str $ length str) TokenEOF]
    else
        return tokens

-- language defs parser
-- | Unpacks the data value from a @TokenCustom@
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
innerCodeBlock = fmap unpackCustom (codeBlock' True) <|> return ""
codeBlockAux :: Parser String
codeBlockAux = do
    preStr <- nonCurlyStr
    innerBlock <- innerCodeBlock
    let out = preStr ++ innerBlock
    if out == "" then empty else return out
withBraces :: Bool -> String -> String
withBraces True str = "{" ++ str ++ "}"
withBraces False str = str
codeBlock' :: Bool -> Parser TokenType
codeBlock' includeBraces = do
    char '{'
    str <- fmap concat $ many codeBlockAux
    postStr <- nonCurlyStr
    char '}'
    return $ TokenCustom "CodeBlock" $ withBraces includeBraces $ str ++ postStr
-- | Parser for directives of the form @%mydirective@
directive :: Parser TokenType
directive = do
    char '%'
    str <- ident
    return $ TokenCustom "Directive" str
-- | Haskell identifiers, defined as lower identifiers followed by zero or more @'@s
identifierPrime :: Parser TokenType
identifierPrime = do
    name <- ident
    primes <- some (char '\'')
    return $ TokenCustom "IdentifierPrime" (name ++ primes)
-- | Haskell code block, defined as follows: @{ haskellCode }@
codeBlock :: Parser TokenType
codeBlock = codeBlock' False
-- | All three above parsers combined into one parser
languageDefsParser :: Parser TokenType
languageDefsParser = codeBlock <|> directive <|> identifierPrime
