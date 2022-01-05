{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cache where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Network.URI
import Text.Read

import Lib (GeminiURI,validateGeminiURI,SuccessType(..))

instance PersistField GeminiURI where
  toPersistValue = PersistText . Text.pack . show
  fromPersistValue = maybe (Left (Text.pack "Invalid Gemini URL")) Right .
                     (validateGeminiURI <=< parseURI . Text.unpack) <=<
                     fromPersistValueText
instance PersistFieldSql GeminiURI where sqlType _ = SqlString

instance PersistField SuccessType where
  toPersistValue SuccessNormal = toPersistValue ("0" :: Text)
  toPersistValue (SuccessOther n) = toPersistValue (Text.pack (show n))
  fromPersistValue (PersistText t)
    | Just n <- readMaybe (Text.unpack t) =
        Right (if n == 0 then SuccessNormal else SuccessOther n)
  fromPersistValue x = Left (Text.pack ("Can't inflate " ++ show x ++ " to SuccessType"))

instance PersistFieldSql SuccessType where sqlType _ = SqlString

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
Entry
    uri GeminiURI
    fetchDate UTCTime
    successType SuccessType
    mime Text
    payload Text

    UniqueUri uri
    deriving Show
|]

fetchFromCache :: GeminiURI -> ReaderT SqlBackend IO (Maybe Entry)
fetchFromCache uri = fmap entityVal <$> getBy (UniqueUri uri)

insertCache :: GeminiURI -> SuccessType -> Text -> Text -> ReaderT SqlBackend IO Entry
insertCache uri successType mime payload = do
  cur <- liftIO getCurrentTime
  let val = Entry uri cur successType mime payload
  entityVal <$> upsertBy (UniqueUri uri) val
    [ EntryFetchDate =. val ^. entryFetchDate
    , EntrySuccessType =. val ^. entrySuccessType
    , EntryMime =. val ^. entryMime
    , EntryPayload =. val ^. entryPayload
    ]
