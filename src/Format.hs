module Format where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Text.URI
import Text.Megaparsec (parseMaybe)

data Doc = Doc
  { docLang :: Maybe Text
  , docLines :: [DocEntry]
  }
  deriving Show

data DocEntry =
  TextLine TextLineContent
  | LinkLine URI (Maybe Text)
  | PreBlock (Maybe Text) [Text]
  deriving Show

data TextLineContent =
  SimpleTextLine Text
  | HeadingLine Level Text
  | ListItem Text
  | QuoteLine Text
  deriving Show

data Level = L1 | L2 | L3 deriving (Enum,Show)

isWhiteSpace :: Char -> Bool
isWhiteSpace ' '  = True
isWhiteSpace '\t' = True
isWhiteSpace  _   = False

parseDoc :: Text -> Doc
parseDoc = Doc Nothing . go . Text.lines where
  go [] = []
  go (l:ls)
    | Just l' <- Text.stripPrefix "=>" l = parseLink l' : go ls
    | Just l' <- Text.stripPrefix "```" l =
        let (pre,ls') = parsePre ls
        in PreBlock (parseAlt l') pre : go ls'
    | Just l' <- Text.stripPrefix "#" l =
        let (hd,l'') = Text.break (/= '#') l'
            (hd',hdR) = Text.splitAt 2 hd
            (_,l''') = Text.break (not . isWhiteSpace) (hdR <> l'')
        in TextLine (HeadingLine (toEnum (Text.length hd')) l''') : go ls
    | Just l' <- Text.stripPrefix "* " l = TextLine (ListItem l') : go ls
    | Just l' <- Text.stripPrefix ">" l = TextLine (QuoteLine l') : go ls
    | otherwise = TextLine (SimpleTextLine l) : go ls

parsePre :: [Text] -> ([Text],[Text])
parsePre = fmap tail . break ("```" `Text.isPrefixOf`)
        
parseAlt :: Text -> Maybe Text
parseAlt = guarded (not . Text.null) . Text.dropWhile isWhiteSpace

parseLink :: Text -> DocEntry
parseLink l = fromMaybe (TextLine (SimpleTextLine l)) $ do
  let l' = Text.dropWhile isWhiteSpace l
      (raw,l'') = Text.break isWhiteSpace l'
      mbName = Text.dropWhile isWhiteSpace l''
  uri <- parseMaybe @Text parser raw
  pure (LinkLine uri (guarded (not . Text.null) mbName))

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p x = x <$ guard (p x)
