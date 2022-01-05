{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( request
    , Status(..)
    , SuccessType(..)
    , validateGeminiURI
    , GeminiURI(unGemini)
    ) where

import Debug.Trace

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Coerce
import Data.List
import Data.Maybe
import Text.Read

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Data.ByteString.Lens

import Control.Lens
import Control.Lens.Prism

import Conduit
import Network.Connection
import Network.URI
import Network.URI.Lens
import Data.Conduit.Network
import Data.Conduit.Network.TLS

newtype GeminiURI = GeminiURI { unGemini :: URI } deriving (Eq,Ord)
instance Show GeminiURI where show (GeminiURI uri) = show uri

guriHost :: Lens' GeminiURI BSC.ByteString
guriHost =
  coerced . uriAuthorityLens . singular _Just . uriRegNameLens . packedChars

guriPort :: Lens' GeminiURI Int
guriPort =
  coerced . uriAuthorityLens . singular _Just . uriPortLens .
  lens (readMaybe <=< stripPrefix ":") (const (maybe "" ((':' :) . show))) .
  non 1965

validateGeminiURI :: (Alternative m,MonadFail m) => URI -> m GeminiURI
validateGeminiURI uri@URI{..} = do
  guard (uriScheme == "gemini:") <|> fail "uriScheme must be \"gemini:\" (1.2)"
  URIAuth{..} <- maybe (fail "uriAuthority is mandatory (1.2)") pure uriAuthority
  guard (null uriUserInfo) <|> fail "URI authority user info is not allowed (1.2)"
  guard (not (null uriRegName)) <|> fail "URI authority host is required (1.2)"
  pure (GeminiURI uri)

data Status =
    Input { inputType :: InputType, inputPrompt :: Text }
  | Success { successType :: SuccessType, successMIME :: Text, successPayload :: Text }
  | Redirect { redirectType :: RedirectType, redirectURL :: URI }
  | TemporaryFailure { tfailureType :: TemporaryFailureType, tfailureMessage :: Text }
  | PermanentFailure { pfailureType :: PermanentFailureType, pfailureMessage :: Text }
  | ClientCertificateError { ccertType :: ClientCertificateErrorType, ccertMessage :: Text }
  | ProtocolError { peType :: ProtocolErrorType, peMessage :: Maybe Text }
  deriving Show

data InputType =
    InputNormal
  | InputSensitive
  | InputOther Int
  deriving Show

data SuccessType =
    SuccessNormal
  | SuccessOther Int
  deriving Show

data RedirectType =
    RedirectNormal
  | RedirectTemporary
  | RedirectPermanent
  | RedirectOther Int
  deriving Show

data TemporaryFailureType =
    TemporaryFailureNormal
  | ServerUnavailable
  | CGIError
  | ProxyError
  | SlowDown
  | TemporaryFailureOther Int
  deriving Show
  
data PermanentFailureType =
    PermanentFailureNormal
  | NotFound
  | Gone
  | ProxyRequestRefused
  | BadRequest
  | PermanentFailureOther Int
  deriving Show

data ClientCertificateErrorType =
    ClientCertificateRequired
  | CertificateNotAuthorised
  | CertificateNotValid
  | CertificateOther Int
  deriving Show

data ProtocolErrorType =
    EmptyReply
  | UnknownStatus
  | IncompleteStatus
  | IncompleteHeader
  | DecodingException
  deriving Show
  
request :: GeminiURI -> IO Status
request gUri@(GeminiURI uri) = do
  let host = gUri ^. guriHost
      port = gUri ^. guriPort

  runTLSClient ((tlsClientConfig port host) { tlsClientTLSSettings = TLSSettingsSimple True False False } ) $ \appData -> do
    let query = Text.encodeUtf8 (Text.pack (uriToString id uri "\r\n"))
    runConduit $ yield query .| appSink appData
    fmap (const parse (BSC.unpack . BSCL.toStrict)) . runConduit $ appSource appData .| sinkLazy

parse :: BSCL.ByteString -> Status
parse bs = case BSCL.uncons bs of
  Nothing -> ProtocolError EmptyReply Nothing
  Just ('1',r) -> parseInput r
  Just ('2',r) -> parseSuccess r
  Just ('3',r) -> parseRedirect r
  Just ('4',r) -> parseTFail r
  Just ('5',r) -> parsePFail r
  Just ('6',r) -> parseCFail r
  Just (s,_) -> ProtocolError UnknownStatus (Just $ "Unknown status class: " <> Text.singleton s)

parseInput = error "parseInput"

defaultMIME = "text/gemini; charset=utf-8"

parseSuccess bs = case BSCL.uncons bs of
  Nothing -> ProtocolError IncompleteStatus (Just "2")
  Just (c,r) ->
    let fixMeta m = BSCL.tail $ case BSCL.unsnoc m of
                                  Just (m','\r') -> m'
                                  _ -> m in
    let t = if c == '0' then SuccessNormal else SuccessOther (digitToInt c)
        (buf,r'') = BSCL.splitAt (1 + 1024 + 2) r
        (fixMeta -> meta,r') = BSCL.break (== '\n') buf
    in if BSCL.null r'
         then ProtocolError IncompleteHeader (Just "Missing <CR><LF> at end of header")
       else case Text.decodeUtf8' (BSCL.toStrict (BSCL.tail r' <> r'')) of
              Left e -> ProtocolError DecodingException (Just (Text.pack (show e)))
              Right b -> Success t (Text.decodeUtf8 (BSCL.toStrict meta)) b
    
parseRedirect = error "parseRedirect"
parseTFail = error "parseTFail"
parsePFail = error "parsePFail"
parseCFail = error "parseCFail"
