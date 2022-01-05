{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module History where

import Control.Lens
import Lib

data History = History
  { _back    :: ![GeminiURI]
  , _current :: !(Maybe GeminiURI)
  , _forward :: ![GeminiURI]
  }
  deriving Show
makeLenses ''History

emptyHistory :: History
emptyHistory = History
  { _back = []
  , _current = Nothing
  , _forward = []
  }

push :: GeminiURI -> History -> History
push new History{..} = History
  { _back = maybe id (:) _current _back
  , _current = Just new
  , _forward = []
  }

moveForward :: History -> Maybe History
moveForward History{..} = do
  (new,next) <- uncons _forward
  pure History
    { _back = maybe id (:) _current _back
    , _current = Just new
    , _forward = next
    }

moveBack :: History -> Maybe History
moveBack History{..} = do
  (new,prev) <- uncons _back
  pure History
    { _back = prev
    , _current = Just new
    , _forward = maybe id (:) _current _forward
    }
