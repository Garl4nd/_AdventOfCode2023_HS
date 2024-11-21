import Text.Megaparsec
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
--import Text.Megaparsec.Byte (string', string, alphaNumChar)
import Control.Monad (void)
import Data.Text.Lazy.Builder.Int (decimal)
type MParser = Parsec Void T.Text
type Value = Int


nullParser :: MParser Value
nullParser = nullWordParser >> return 0
  where
    nullWordParser = string "Null" <|> string "NULL" <|> string "null"

-- pScheme :: MParser T.Text
-- pScheme = string "data"
--   <|> string "file"
--   <|> string "ftp"
--   <|> string "http"
--   <|> string "https"
--   <|> string "irc"
--   <|> string "mailto"

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: MParser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
    , SchemeHttp   <$ string "http"

  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: T.Text
  , uriQuery :: Maybe T.Text
  , uriFragment :: Maybe T.Text
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (T.Text, T.Text) -- (user, password)
  , authHost :: T.Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

-- pUri' :: MParser Uri
-- pUri' = do
--   uriScheme <- pScheme
--   void (char ':')
--   uriAuthority <- optional . try $ do            -- (1)
--     void (string "//")
--     authUser <- optional . try $ do              -- (2)
--       user <- T.pack <$> some alphaNumChar       -- (3)
--       void (char ':')
--       password <- T.pack <$> some alphaNumChar
--       void (char '@')
--       return (user, password)
--     authHost <- T.pack <$> some (alphaNumChar <|> char '.')
--     authPort <- optional (char ':' *> L.decimal) -- (4)
--     return $ Authority authUser authHost authPort                        -- (5)
--   return $ Uri uriScheme uriAuthority
  --[//[user:password@]host[:port]]
-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
--scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

pUri :: MParser Uri
pUri = do
  uriScheme <- pScheme
  void (single ':')
  uriAuthority <- optional $ do 
    void (chunk "//" )
    userPas <- optional . try $ do 
      user <- T.pack <$> some alphaNumChar
      void (single ':')
      password <- T.pack <$> some alphaNumChar
      void (single '@')
      return (user, password)
    host <- T.pack <$> some (alphaNumChar <|> char '.')
    port <- optional $ do 
      void (char ':')
      L.decimal 
    return $ Authority userPas host port
  void (optional. try $ single '/' )
  path <- T.pack <$> some (alphaNumChar <|> char '/')
  query <- optional. try $  do
      void (single '?')
      T.pack <$> some (alphaNumChar <|> char '&' <|> char '=')
  fragment <- optional $ do 
      void (single '#')
      T.pack <$> some (alphaNumChar <|> char '&' <|> char '=')
  return $ Uri uriScheme uriAuthority path query fragment

line = "9vtglcdvkgcgrvm4sevenrhppknqfvhldfgqhpgdqfv"

type SParser = Parsec Void String

parseAOC1 :: String -> [Int]
parseAOC1 = runParser numParser ""  where 
  numParser :: SParser [Int]
  numParser = L.decimal 
