{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( request
    , Status(..)
    , SuccessType(..)
    , validateGeminiURI
    , GeminiURI(unGemini)
    ) where

import Control.Applicative
import Control.Monad
import Data.Char

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSCL
import qualified Data.ByteString.Builder as BS
import qualified Data.Text.Strict.Lens as Text

import Control.Lens

import Conduit
import Network.Connection
import Text.URI (URI,renderBs',mkScheme,RText,RTextLabel(Host))
import Text.URI.Lens
import Data.Conduit.Network
import Data.Conduit.Network.TLS

newtype GeminiURI = GeminiURI { unGemini :: URI } deriving (Eq,Ord,Show)
makeWrapped ''GeminiURI

guriHost :: Lens' GeminiURI (RText 'Host)
guriHost = _Wrapped . uriAuthority . singular _Right . authHost

guriPort :: Lens' GeminiURI Word
guriPort = _Wrapped . uriAuthority . singular _Right . authPort . non 1965

validateGeminiURI :: (Alternative m,MonadFail m) => URI -> m GeminiURI
validateGeminiURI uri = do
  guard (uri ^. uriScheme == mkScheme "gemini") <|> fail "uriScheme must be \"gemini\" (1.2)"
  auth <- maybe (fail "uriAuthority is mandatory (1.2)") pure (uri ^? uriAuthority . _Right)
  guard (auth ^. authUserInfo . to null) <|> fail "URI authority user info is not allowed (1.2)"
  guard (auth ^. authHost . unRText . to (not . Text.null)) <|> fail "URI authority host is required (1.2)"
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
  let host = gUri ^. guriHost . unRText . re Text.utf8
      port = gUri ^. guriPort . to fromIntegral

      tlsCfg =
        (tlsClientConfig port host)
        { tlsClientTLSSettings = TLSSettingsSimple True False False }

  runTLSClient tlsCfg $ \appData -> do
    let queryString =
          BS.toLazyByteString (renderBs' uri <> BS.string7 "\r\n") ^. strict
    runConduit $ yield queryString .| appSink appData
    fmap parse . runConduit $ appSource appData .| sinkLazy

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

parseInput :: BSCL.ByteString -> Status
parseInput = error "parseInput"

defaultMIME :: BSC.ByteString
defaultMIME = "text/gemini; charset=utf-8"

parseSuccess :: BSCL.ByteString -> Status
parseSuccess bs = case BSCL.uncons bs of
  Nothing -> ProtocolError IncompleteStatus (Just "2")
  Just (c,r) ->
    let fixMeta m = BSCL.tail $ case BSCL.unsnoc m of
                                  Just (m','\r') -> m'
                                  _ -> m in
    let t = if c == '0' then SuccessNormal else SuccessOther (digitToInt c)
        (buf,r'') = BSCL.splitAt (1 + 1024 + 2) r
        (fixMeta -> meta,r') = BSCL.break (== '\n') buf
        mime | BSCL.null meta = defaultMIME
             | otherwise = BSCL.toStrict meta
    in if BSCL.null r'
         then ProtocolError IncompleteHeader (Just "Missing <CR><LF> at end of header")
       else case Text.decodeUtf8' (BSCL.toStrict (BSCL.tail r' <> r'')) of
              Left e -> ProtocolError DecodingException (Just (Text.pack (show e)))
              Right b -> Success t (Text.decodeUtf8 mime) b
    
parseRedirect,parseTFail,parsePFail,parseCFail :: BSCL.ByteString -> Status
parseRedirect = error "parseRedirect"
parseTFail = error "parseTFail"
parsePFail = error "parsePFail"
parseCFail = error "parseCFail"
